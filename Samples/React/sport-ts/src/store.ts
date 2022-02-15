import { configureStore, getDefaultMiddleware } from "@reduxjs/toolkit";
import { sportApi } from './api';

export const store = configureStore({
  reducer: {
    [sportApi.reducerPath]: sportApi.reducer
  },
  middleware: (getDefaultMiddleware) => getDefaultMiddleware().concat(sportApi.middleware)
})