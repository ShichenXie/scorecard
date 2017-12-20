# information entropy
# ent(D) = -\sum_k(p_k*log_2(p_k)), if p_k=0 then p_k*log_2(p_k)=0

# information gain (ID3)
# gain = ent(D) - \sum(abs(\frac{D^v}{D})*ent(D^v))

# gain ratio (C4.5)
# gain_ratio(D,a) = Gain(D,a)/IV(a)
# instrinsic value: IV(a) = -\sum_v(abs(\frac{D^v}{D})*log_2(abs(\frac{D^v}{D})))


# gini index (CART)
# gini(D) = 1-\sum_k(p_k^2)
# gini_index(D) = \sum_v(abs(\frac{D^v}{D})*gini(D^v))



# [chimerge](http://blog.csdn.net/qunxingvip/article/details/50449376)