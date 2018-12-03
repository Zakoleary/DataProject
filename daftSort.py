from daftlistings import Daft, SortOrder, SortType, RentType

daft = Daft()

daft.set_county("Dublin City")
daft.set_listing_type(RentType.ANY)
daft.set_sort_order(SortOrder.ASCENDING)
daft.set_sort_by(SortType.PRICE)
daft.set_max_price(2500)

listings = daft.search()

for listing in listings:
    print(listing.formalised_address)
    print(listing.daft_link)
    print(listing.price)
    features = listing.features
    if features is not None:
        print('Features: ')
        for feature in features:
            print(feature)
    print("")
