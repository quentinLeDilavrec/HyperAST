fn new<E>(x: Result<impl Iterator, E>) {
    let Ok(mut a) = x else { return };
    loop {
        let Some(_) = a.next() else { break };
    }
}

fn old<E>(x: Result<impl Iterator, E>) {
    let a = if let Ok(mut a) = x {
        a
    } else {
        return;
    };
    loop {
        if let Some(_) = a.next() {
        } else {
            break;
        };
    }
}
