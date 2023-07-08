#pragma once
#include <cstddef>
#include <memory>
#include <stdexcept>

// declarations
template <typename T>
class SharedPtr;

template <typename T>
class WeakPtr;

template <typename T>
class EnableSharedFromThis;

template <typename T, typename Alloc, typename... Args>
SharedPtr<T> allocateShared(const Alloc& alloc, Args&&... args);

template <typename T, typename U>
struct Convertible {
    static const bool value = std::is_same_v<T, U> || std::is_base_of_v<T, U>;
};

struct BaseControlBlock {
    using count_type = size_t;

    count_type shared_count = 0;
    count_type weak_count = 0;

    BaseControlBlock(count_type shared_count, count_type weak_count)
        : shared_count(shared_count), weak_count(weak_count) {}

    virtual void* get_pointer() noexcept {
        return nullptr;
    }

    virtual void destroy() noexcept {}

    virtual ~BaseControlBlock() = default;

    virtual void deallocate() {
        delete this;
    }
};

// definitions
template <typename T>
class SharedPtr {
  public:
    using weak_type = WeakPtr<T>;
    using count_type = size_t;

  private:
    template <typename Alloc, typename Deleter>
    struct ControlBlockRegular : BaseControlBlock {
        Alloc alloc;
        Deleter deleter;
        T* object_ptr = nullptr;

        using AllocControlBlock = typename std::allocator_traits<
            Alloc>::template rebind_alloc<ControlBlockRegular<Alloc, Deleter>>;
        using AllocControlBlockTraits =
            std::allocator_traits<AllocControlBlock>;

        ControlBlockRegular(count_type shared_count, count_type weak_count,
                            const Alloc& alloc, const Deleter& deleter,
                            T* object_ptr)
            : BaseControlBlock(shared_count, weak_count),
              alloc(alloc),
              deleter(deleter),
              object_ptr(object_ptr) {}

        void* get_pointer() noexcept override {
            return object_ptr;
        }

        void destroy() noexcept override {
            deleter(object_ptr);
        }

        void deallocate() override {
            ControlBlockRegular<Alloc, Deleter>* ptr = this;
            // new allocator
            AllocControlBlock new_alloc = alloc;
            // destroy and deallocate
            this->~ControlBlockRegular();
            // AllocControlBlockTraits::destroy(new_alloc, ptr); // but why???
            AllocControlBlockTraits::deallocate(new_alloc, ptr, 1);
        }

        ~ControlBlockRegular() override {}
    };

    template <typename Alloc>
    struct ControlBlockMakeShared : BaseControlBlock {
        Alloc alloc;
        T object;

        using AllocControlBlock = typename std::allocator_traits<
            Alloc>::template rebind_alloc<ControlBlockMakeShared<Alloc>>;
        using AllocControlBlockTraits =
            std::allocator_traits<AllocControlBlock>;

        template <typename... Args>
        ControlBlockMakeShared(count_type shared_count, count_type weak_count,
                               const Alloc& alloc, Args&&... args)
            : BaseControlBlock(shared_count, weak_count),
              alloc(alloc),
              object(std::forward<Args>(args)...) {}

        void* get_pointer() noexcept override {
            return &object;
        }

        void destroy() noexcept override {
            std::allocator_traits<Alloc>::destroy(alloc, &object);
        }

        void deallocate() override {
            ControlBlockMakeShared<Alloc>* ptr = this;
            // new allocator
            AllocControlBlock new_alloc = alloc;
            // destroy and deallocate
            AllocControlBlockTraits::deallocate(new_alloc, ptr, 1);
        }

        ~ControlBlockMakeShared() override {}
    };

    BaseControlBlock* cb = nullptr;

    template <typename Alloc, typename Deleter>
    static BaseControlBlock* allocate_and_construct(T* ptr, Alloc alloc,
                                                    Deleter deleter) {
        // rebind
        using AllocControlBlock =
            typename ControlBlockRegular<Alloc, Deleter>::AllocControlBlock;
        // allocator traits
        using AllocControlBlockTraits =
            typename ControlBlockRegular<Alloc,
                                         Deleter>::AllocControlBlockTraits;
        // new allocator
        AllocControlBlock new_alloc = alloc;
        // allocate and construct
        ControlBlockRegular<Alloc, Deleter>* cb_ptr =
            AllocControlBlockTraits::allocate(new_alloc, 1);
        new (cb_ptr)
            ControlBlockRegular<Alloc, Deleter>(1, 0, alloc, deleter, ptr);
        // AllocControlBlockTraits::construct(new_alloc, cb_ptr, 1, 0, alloc, deleter, ptr); // but why???
        return static_cast<BaseControlBlock*>(cb_ptr);
    }

    template <typename Alloc>
    SharedPtr(ControlBlockMakeShared<Alloc>* cb)
        : cb(static_cast<BaseControlBlock*>(cb)) {
        if constexpr (std::is_base_of_v<EnableSharedFromThis<T>, T>) {
            get()->wptr = *this;
        }
    }

  public:
    constexpr SharedPtr() noexcept = default;

    // T* constructors
    template <typename U, std::enable_if_t<Convertible<T, U>::value, int> = 0>
    explicit SharedPtr(U* ptr)
        : cb(allocate_and_construct<std::allocator<T>, std::default_delete<U>>(
              ptr, std::allocator<T>(), std::default_delete<U>())) {
        if constexpr (std::is_base_of_v<EnableSharedFromThis<T>, U>) {
            ptr->wptr = *this;
        }
    }

    template <typename U, typename Deleter,
              std::enable_if_t<Convertible<T, U>::value, int> = 0>
    SharedPtr(U* ptr, const Deleter& deleter)
        : cb(allocate_and_construct<std::allocator<T>, Deleter>(
              ptr, std::allocator<T>(), deleter)) {
        if constexpr (std::is_base_of_v<EnableSharedFromThis<T>, U>) {
            ptr->wptr = *this;
        }
    }

    template <typename U, typename Deleter, typename Alloc,
              std::enable_if_t<Convertible<T, U>::value, int> = 0>
    SharedPtr(U* ptr, const Deleter& deleter, const Alloc& alloc)
        : cb(allocate_and_construct<Alloc, Deleter>(ptr, alloc, deleter)) {
        if constexpr (std::is_base_of_v<EnableSharedFromThis<T>, U>) {
            ptr->wptr = *this;
        }
    }

    // constructors from smart pointers
    SharedPtr(const SharedPtr& other) : cb(other.cb) {
        if (cb != nullptr) {
            ++cb->shared_count;
        }
    }
    template <typename U, std::enable_if_t<Convertible<T, U>::value, int> = 0>
    SharedPtr(const SharedPtr<U>& other) : cb(other.cb) {
        if (cb) {
            ++cb->shared_count;
        }
    }

    SharedPtr(SharedPtr&& other) : cb(other.cb) {
        other.cb = nullptr;
    }
    template <typename U, std::enable_if_t<Convertible<T, U>::value, int> = 0>
    SharedPtr(SharedPtr<U>&& other) : cb(other.cb) {
        other.cb = nullptr;
    }

    template <typename U, std::enable_if_t<Convertible<T, U>::value, int> = 0>
    explicit SharedPtr(const WeakPtr<U>& w_other) : cb(w_other.cb) {
        if (w_other.expired()) {
            throw std::bad_weak_ptr();
        }
        if (cb) {
            ++cb->shared_count;
        }
    }

    // assignment operators
    SharedPtr& operator=(const SharedPtr& other) noexcept {
        if (this != &other) {
            SharedPtr<T>(other).swap(*this);
        }
        return *this;
    }
    template <typename U, std::enable_if_t<Convertible<T, U>::value, int> = 0>
    SharedPtr& operator=(const SharedPtr<U>& other) noexcept {
        SharedPtr<T>(other).swap(*this);
        return *this;
    }

    SharedPtr& operator=(SharedPtr&& other) noexcept {
        SharedPtr<T>(std::move(other)).swap(*this);
        return *this;
    }
    template <typename U, std::enable_if_t<Convertible<T, U>::value, int> = 0>
    SharedPtr& operator=(SharedPtr<U>&& other) noexcept {
        SharedPtr<T>(std::move(other)).swap(*this);
        return *this;
    }

    // destructor
    ~SharedPtr() {
        if (cb != nullptr) {
            // --cb->shared_count; // we can't, because of
            // enable_shared_from_this
            if (cb->shared_count == 1) {
                // destroyes object
                cb->destroy();
                if (cb->weak_count == 0) {
                    // deallocates the control block
                    cb->deallocate();
                    return;
                }
            }
            --cb->shared_count;
        }
    }

    // object oriented methods
    T* get() const noexcept {
        if (cb != nullptr) {
            return static_cast<T*>(cb->get_pointer());
        }
        return nullptr;
    }
    T* operator->() const noexcept {
        return get();
    }
    T& operator*() const noexcept {
        return *(operator->());
    }

    // other methods
    int64_t use_count() const noexcept {
        if (cb != nullptr) {
            return static_cast<int64_t>(cb->shared_count);
        }
        return 0;
    }

    void
    reset() noexcept {  // release resource and convert to empty shared_ptr object
        SharedPtr().swap(*this);
    }

    template <typename U, std::enable_if_t<Convertible<T, U>::value, int> = 0>
    void reset(U* ptr) {
        SharedPtr<T>(ptr).swap(*this);
    }

    template <typename U, typename Deleter>
    void reset(U* ptr, Deleter deleter,
               std::enable_if_t<Convertible<T, U>::value, int> /*unused*/ = 0) {
        SharedPtr<T>(ptr, deleter).swap(*this);
    };

    template <typename U, typename Deleter, typename Alloc>
    void reset(U* ptr, Deleter deleter, Alloc alloc,
               std::enable_if_t<Convertible<T, U>::value, int> /*unused*/ = 0) {
        SharedPtr<T>(ptr, deleter, alloc).swap(*this);
    };

    void swap(SharedPtr& other) {
        std::swap(cb, other.cb);
    }

    // friends
    template <typename U>
    friend class SharedPtr;

    template <typename U>
    friend class WeakPtr;

    template <typename U, typename Alloc, typename... Args>
    friend SharedPtr<U> allocateShared(const Alloc& alloc, Args&&... args);
};

template <typename T>
class WeakPtr {
  public:
    using shared_type = SharedPtr<T>;
    using count_type = size_t;

    // using BaseControlBlock = typename shared_type::BaseControlBlock;

  private:
    BaseControlBlock* cb = nullptr;

  public:
    constexpr WeakPtr() noexcept = default;

    // copy-constructors
    WeakPtr(const WeakPtr& other) noexcept : cb(other.cb) {
        if (cb != nullptr) {
            ++cb->weak_count;
        }
    }
    template <typename U, std::enable_if_t<Convertible<T, U>::value, int> = 0>
    WeakPtr(const WeakPtr<U>& other) noexcept : cb(other.cb) {
        if (cb) {
            ++cb->weak_count;
        }
    }
    template <typename U, std::enable_if_t<Convertible<T, U>::value, int> = 0>
    WeakPtr(const SharedPtr<U>& other) noexcept : cb(other.cb) {
        if (cb) {
            ++cb->weak_count;
        }
    }

    // move-constructors
    WeakPtr(WeakPtr&& other) noexcept : cb(other.cb) {
        other.cb = nullptr;
    }
    template <typename U, std::enable_if_t<Convertible<T, U>::value, int> = 0>
    WeakPtr(WeakPtr<U>&& other) noexcept : cb(other.cb) {
        other.cb = nullptr;
    }

    // copy-assignment operators
    WeakPtr& operator=(const WeakPtr& other) noexcept {
        if (this != &other) {
            WeakPtr<T>(other).swap(*this);
        }
        return *this;
    }
    template <typename U, std::enable_if_t<Convertible<T, U>::value, int> = 0>
    WeakPtr& operator=(const WeakPtr<U>& other) noexcept {
        WeakPtr<T>(other).swap(*this);
        return *this;
    }
    template <typename U, std::enable_if_t<Convertible<T, U>::value, int> = 0>
    WeakPtr& operator=(const SharedPtr<U>& other) noexcept {
        WeakPtr<T>(other).swap(*this);
        return *this;
    }

    // move-assignment operators
    WeakPtr& operator=(WeakPtr&& other) noexcept {
        WeakPtr<T>(std::move(other)).swap(*this);
        return *this;
    }
    template <typename U, std::enable_if_t<Convertible<T, U>::value, int> = 0>
    WeakPtr& operator=(WeakPtr<U>&& other) noexcept {
        WeakPtr<T>(std::move(other)).swap(*this);
        return *this;
    }

    // destructor
    ~WeakPtr() {
        if (cb != nullptr) {
            --cb->weak_count;
            if (cb->shared_count == 0 && cb->weak_count == 0) {
                cb->deallocate();
            }
        }
    }

    // other methods
    bool expired() const noexcept {
        return use_count() == 0;
    }

    SharedPtr<T> lock() const noexcept {
        return expired() ? SharedPtr<T>() : SharedPtr<T>(*this);
    };

    int64_t use_count() const noexcept {
        if (cb != nullptr) {
            return static_cast<int64_t>(cb->shared_count);
        }
        return 0;
    }

    void swap(WeakPtr& other) noexcept {
        std::swap(cb, other.cb);
    }

    template <typename U>
    friend class SharedPtr;

    template <typename U>
    friend class WeakPtr;
};

template <typename T>
class EnableSharedFromThis {
    WeakPtr<T> wptr;

  public:
    SharedPtr<T> shared_from_this() {
        if (wptr.expired()) {
            throw std::bad_weak_ptr();
        }
        return wptr.lock();
    }

    friend SharedPtr<T>;
};

template <typename T, typename Alloc, typename... Args>
SharedPtr<T> allocateShared(const Alloc& alloc, Args&&... args) {
    // rebind
    using AllocControlBlock = typename SharedPtr<
        T>::template ControlBlockMakeShared<Alloc>::AllocControlBlock;
    // allocator traits
    using AllocControlBlockTraits = typename SharedPtr<
        T>::template ControlBlockMakeShared<Alloc>::AllocControlBlockTraits;
    // new allocator
    AllocControlBlock new_alloc = alloc;
    // allocate and construct
    typename SharedPtr<T>::template ControlBlockMakeShared<Alloc>* cb_ptr =
        AllocControlBlockTraits::allocate(new_alloc, 1);
    AllocControlBlockTraits::construct(new_alloc, cb_ptr, 1, 0, alloc,
                                       std::forward<Args>(args)...);
    return SharedPtr<T>(cb_ptr);
}

// and, hence
template <typename T, typename... Args>
SharedPtr<T> makeShared(Args&&... args) {
    std::allocator<T> alloc;
    return allocateShared<T, std::allocator<T>, Args...>(
        alloc, std::forward<Args>(args)...);
}
