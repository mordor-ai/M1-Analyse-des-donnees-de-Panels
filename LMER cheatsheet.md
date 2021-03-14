from : https://stats.stackexchange.com/questions/13166/rs-lmer-cheat-sheet/13173#13173



- 𝑌𝑠𝑖=β0+β1𝑋𝑖+𝑒𝑠𝑖Ysi=β0+β1Xi+esi
- N/A (Not a mixed-effects model)

- 𝑌𝑠𝑖=β0+𝑆0𝑠+β1𝑋𝑖+𝑒𝑠𝑖Ysi=β0+S0s+β1Xi+esi
- `Y ∼ X+(1∣Subject)`

- 𝑌𝑠𝑖=β0+𝑆0𝑠+(β1+𝑆1𝑠)𝑋𝑖+𝑒𝑠𝑖Ysi=β0+S0s+(β1+S1s)Xi+esi
- `Y ∼ X+(1 + X∣Subject)`

- 𝑌𝑠𝑖=β0+𝑆0𝑠+𝐼0𝑖+(β1+𝑆1𝑠)𝑋𝑖+𝑒𝑠𝑖Ysi=β0+S0s+I0i+(β1+S1s)Xi+esi
- `Y ∼ X+(1 + X∣Subject)+(1∣Item)`

- 𝑌𝑠𝑖=β0+𝑆0𝑠+𝐼0𝑖+β1𝑋𝑖+𝑒𝑠𝑖Ysi=β0+S0s+I0i+β1Xi+esi
- `Y ∼ X+(1∣Subject)+(1∣Item)`

- As (4), but 𝑆0𝑠S0s, 𝑆1𝑠S1s independent 
- `Y ∼ X+(1∣Subject)+(0 + X∣ Subject)+(1∣Item)`

- 𝑌𝑠𝑖=β0+𝐼0𝑖+(β1+𝑆1𝑠)𝑋𝑖+𝑒𝑠𝑖Ysi=β0+I0i+(β1+S1s)Xi+esi
- `Y ∼ X+(0 + X∣Subject)+(1∣Item)`