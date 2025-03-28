export const fromArrayImpl = (items) => {
    return [...items]
}

export const toArrayImpl = (items) => {
    return [...items]
}

export const shiftImpl = (items) => {
    items.shift()
}

export const unshiftImpl = (item, items) => {
    items.unshift(item)
}

export const pushImpl = (item, items) => {
    items.push(item)
}

export const foldlImpl = (f, acc_, items) => {
    let acc = acc_
    for (let i = 0; i < items.length; i++) {
        acc = f(acc, items[i])
    }

    return acc
}

export const popImpl = (items) => {
    items.pop()
}

export const lengthImpl = (items) => {
    return items.length
}