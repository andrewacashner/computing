x l=take(length l`div`2)l;l k s=all(\c->elem c$foldl(++)[]$map x k)s
