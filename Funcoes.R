################ CondTools - Tratamento e Validação dados do Sicon ##################

## Fluxos para manutenção do pacote - Alterou função existente

# 1. Atualizar documentação (SE mudou algo na doc ou argumentos)
devtools::document()

# 2. Testar localmente (usa código da pasta R/)
devtools::load_all()


# 3. Versionar
system("git add .")
system('git commit -m "ajuste função mapa_faixas"')

# 4. Sincronizar com GitHub
system("git pull --rebase")
system("git push")

# 5. Atualizar pacote instalado
devtools::install()

### Criou uma função nova:

# 1. Criar arquivo da função
usethis::use_r("mapa_nova")

# 2. (você escreve a função com # ' @export no arquivo)

# 3. Gerar documentação (OBRIGATÓRIO)
devtools::document()

# 4. Testar localmente
devtools::load_all()

# teste
mapa_nova(...)

# 5. Versionar
system("git add .")
system('git commit -m "nova função mapa_nova"')

# 6. Sincronizar com GitHub
system("git pull --rebase")
system("git push")

# 7. Instalar pacote atualizado
devtools::install()
