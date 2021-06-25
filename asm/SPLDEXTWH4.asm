*          DATA SET SPLDEXTWH4 AT LEVEL 148 AS OF 06/20/00                      
*PHASE SPEXTWH4                                                                 
*INCLUDE CLUNPK                                                                 
*INCLUDE ADDAY                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE PRNTBL                                                                 
*INCLUDE RECUP                                                                  
         TITLE 'DMLDEXTWH4 - CONVERT 2001 JW ORDERS TO MINDSHARE'               
*                                                                               
*                                                                               
* PARAMETER LIST                                                                
*                                                                               
* P1=A(RECORD)  PASS FIRST BYTE X'00'= INITIALISE                               
*                               X'01'= RECORD IN CORE                           
*                               X'FF'= END OF FILE                              
*               RETURN VALUE    X'00'= KEEP RECORD                              
*                               X'FF'= PURGE RECORD                             
*                               X'FF'/C'EOJ'=PURGE & CAUSE EOJ                  
* P2=A(TAPEOUT) PASS FIRST BYTE X'80'= TAPE INPUT                               
*                               X'40'= TAPE OUTPUT                              
*                               X'20'= RECORD IS I/S FILE RECORD                
* P3=A(PARAM CARD)                                                              
* P4=A(FILE DEFN)                                                               
* P5=A(PRINTER)                                                                 
* P6=A(CPRINT)                                                                  
*                                                                               
         PRINT NOGEN                                                            
DMLDEXT  CSECT                                                                  
         NMOD1 WORKX-WORKD,DMLDEXT                                              
         USING WORKD,RC                                                         
         L     R8,12(R1)           DMLDDEFN                                     
         USING LDDEFND,R8                                                       
         EJECT                                                                  
*                                                                               
* CONTROL FLOW LOGIC                                                            
*                                                                               
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         SPACE 2                                                                
         CLI   PLIST,X'00'                                                      
         BE    DMXINIT             INITIALISE                                   
         CLI   PLIST,X'01'                                                      
         BE    DMXREC              PROCESS                                      
         CLI   PLIST,X'FF'                                                      
         BE    DMXEOF              END-OF-FILE                                  
         B     DMXIT                                                            
         SPACE 2                                                                
DMXKEEP  DS    0H                                                               
         L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         B     DMXIT                                                            
         SPACE 2                                                                
DMXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         B     DMXIT                                                            
         SPACE 2                                                                
DMXPGEOF L     R1,APARM            PURGE AND CAUSE INPUT EOF EXIT               
         MVI   0(R1),X'FF'                                                      
         MVC   1(3,R1),=C'EOF'                                                  
         B     DMXIT                                                            
*                                                                               
DMXEOF   DS    0H                                                               
         MVI   P,C'*'                                                           
         MVC   P+1(39),P                                                        
         GOTO1 VPRINTER                                                         
         MVC   P(26),=CL26'NUMBER IF ORDERS CHANGED:  '                         
         EDIT  (4,COUNT),(8,P+28),ZERO=NOBLANK                                  
         GOTO1 VPRINTER                                                         
         B     DMXIT                                                            
*                                                                               
DMXIT    DS    0H                                                               
         XMOD1 1                                                                
         SPACE 2                                                                
*                                                                               
* INITIALIZE LOGIC                                                              
*                                                                               
DMXINIT  DS    0H                                                               
         MVI   DONE,C'N'                                                        
         XC    COUNT,COUNT                                                      
         B     DMXIT                                                            
         EJECT                                                                  
*                                                                               
* PROCESS RECORD LOGIC                                                          
*                                                                               
DMXREC   L     R5,AREC                                                          
***************                                                                 
* CHECK FOR DARE ORDER RECORD                                                   
***************                                                                 
DMXDAR00 CLC   =X'0D34',0(R5)      DARE ORDER RECORD?                           
         BNE   DMXDBT00                                                         
         USING DOKEY,R5                                                         
*                                                                               
         CLI   DOKAGMD,X'B1'       JW RECORD?                                   
         BNE   DMXKEEP             NO, SKIP                                     
         LA    R1,ORDERTBL                                                      
         USING TABLED,R1                                                        
DMXDAR10 CLI   0(R1),X'FF'         ORDER PART OF OUR LIST?                      
         BE    DMXKEEP             NO, SKIP IT                                  
         CLC   TBLORDER,DOKORDER                                                
         BE    *+12                YES, CHANGE DOKAGMD & DORAGY                 
         AHI   R1,TBLLENQ                                                       
         B     DMXDAR10                                                         
*                                                                               
         MVI   DOKAGMD,X'31'       CHANGE THE AGENCY/MEDIA COMBINATION          
         MVC   DORAGY,=C'H7'       CHANGE THE AGENCY ALPHA CODE                 
*                                                                               
DMXKPCNT L     R0,COUNT                                                         
         AHI   R0,1                                                             
         ST    R0,COUNT                                                         
*                                                                               
         EDIT  (B4,COUNT),(3,P),FILL=0                                          
         GOTO1 LHEXOUT,DMCB,(R5),P+4,13,0                                       
         GOTO1 VPRINTER                                                         
         B     DMXKEEP                                                          
*********                                                                       
* CHECK FOR DARE BATCH RECORD                                                   
*********                                                                       
DMXDBT00 CLC   =X'0D35',0(R5)      DARE BATCH RECORD?                           
         BNE   DMXDMN00                                                         
         USING DBTKEY,R5                                                        
*                                                                               
         CLI   DBTKAGMD,X'B1'      JW RECORD?                                   
         BNE   DMXKEEP             NO, SKIP                                     
         LA    R1,ORDERTBL                                                      
         USING TABLED,R1                                                        
DMXDBT10 CLI   0(R1),X'FF'         MATCHED ON STA/CLT/PRD/EST/PB?               
         BE    DMXKEEP             NO, SKIP IT                                  
         CLC   TBLBSTA,DBTKSTA                                                  
         BNE   DMXDBT15                                                         
         CLC   TBLBCLT,DBTKCLT                                                  
         BNE   DMXDBT15                                                         
         CLC   TBLBPRD,DBTKPRD                                                  
         BNE   DMXDBT15                                                         
         CLC   TBLBEST,DBTKEST                                                  
         BNE   DMXDBT15                                                         
         CLC   TBLBPR2,DBTKPRD2                                                 
         BE    DMXDBT20                                                         
DMXDBT15 AHI   R1,TBLLENQ                                                       
         B     DMXDBT10                                                         
*                                                                               
DMXDBT20 MVI   DBTKAGMD,X'31'      CHANGE THE AGENCY/MEDIA COMBINATION          
         MVC   DBTRAGY,=C'H7'      CHANGE THE AGENCY ALPHA CODE                 
         B     DMXKPCNT                                                         
*********                                                                       
* CHECK FOR DARE MKGD NOTICE RECORD                                             
*********                                                                       
DMXDMN00 CLC   =X'0D36',0(R5)      DARE MAKEGOOD NOTICE RECORD?                 
         BNE   DMXDMO00                                                         
         USING MNKEY,R5                                                         
*                                                                               
         CLI   MNKAGMD,X'B1'       JW RECORD?                                   
         BNE   DMXKEEP             NO, SKIP                                     
         LA    R1,ORDERTBL                                                      
         USING TABLED,R1                                                        
DMXDMN10 CLI   0(R1),X'FF'         MATCHED ON ORDER NUMBER?                     
         BE    DMXKEEP             NO, SKIP IT                                  
         CLC   TBLORDER,MNKORDER                                                
         BE    DMXDMN20                                                         
DMXDMN15 AHI   R1,TBLLENQ                                                       
         B     DMXDMN10                                                         
*                                                                               
DMXDMN20 MVI   MNKAGMD,X'31'       CHANGE THE AGENCY/MEDIA COMBINATION          
         MVC   MNRAGY,=C'H7'       CHANGE THE AGENCY ALPHA CODE                 
         B     DMXKPCNT                                                         
*********                                                                       
* CHECK FOR DARE MKGD OFFER RECORD                                              
*********                                                                       
DMXDMO00 CLC   =X'0D37',0(R5)      DARE MAKEGOOD OFFER RECORD?                  
         BNE   DMXDFL00                                                         
         USING MOKEY,R5                                                         
*                                                                               
         CLI   MOKAGMD,X'B1'       JW RECORD?                                   
         BNE   DMXKEEP             NO, SKIP                                     
         LA    R1,ORDERTBL                                                      
         USING TABLED,R1                                                        
DMXDMO10 CLI   0(R1),X'FF'         MATCHED ON ORDER NUMBER?                     
         BE    DMXKEEP             NO, SKIP IT                                  
         CLC   TBLORDER,MOKORDER                                                
         BE    DMXDMO20                                                         
DMXDMO15 AHI   R1,TBLLENQ                                                       
         B     DMXDMO10                                                         
*                                                                               
DMXDMO20 MVI   MOKAGMD,X'31'       CHANGE THE AGENCY/MEDIA COMBINATION          
         MVC   MORAGY,=C'H7'       CHANGE THE AGENCY ALPHA CODE                 
         B     DMXKPCNT                                                         
*********                                                                       
* CHECK FOR DARE FLIGHT RECORD                                                  
*********                                                                       
DMXDFL00 CLC   =X'0D37',0(R5)      DARE FLIGHT RECORD?                          
         BNE   DMXKEEP                                                          
         USING DFLKEY,R5                                                        
*                                                                               
         CLI   DFLKAGMD,X'B1'      JW RECORD?                                   
         BNE   DMXKEEP             NO, SKIP                                     
         LA    R1,ORDERTBL                                                      
         USING TABLED,R1                                                        
*                                                                               
DMXDFL10 CLI   0(R1),X'FF'         MATCHED ON STA/CLT/PRD/EST/PB?               
         BE    DMXKEEP             NO, SKIP IT                                  
         CLC   TBLBCLT,DFLKCLT                                                  
         BNE   DMXDFL15                                                         
         CLC   TBLBEST,DFLKEST                                                  
         BNE   DMXDFL15                                                         
         CLC   =C'POL',DFLKPRD                                                  
         BE    DMXDFL20                                                         
         CLC   TBLQPRD,DFLKPRD                                                  
         BE    DMXDFL20                                                         
DMXDFL15 AHI   R1,TBLLENQ                                                       
         B     DMXDFL10                                                         
*                                                                               
DMXDFL20 MVI   DFLKAGMD,X'31'      CHANGE THE AGENCY/MEDIA COMBINATION          
         MVC   DFLRAGYA,=C'H7'     CHANGE THE AGENCY ALPHA CODE                 
         B     DMXKPCNT                                                         
         EJECT                                                                  
         GETEL R6,24,ELCODE                                                     
*                                                                               
COUNT    DS    F                                                                
BYTE     DS    X                                                                
DONE     DS    C                                                                
ELCODE   DS    X                                                                
HALF     DS    H                                                                
ELEM     DS    CL100                                                            
JDTTODAY DS    XL4                                                              
CPKEY    DS    XL13                CLIENT PASSIVE KEY FOR DARE ORDERS           
*                                                                               
IOLEN    DS    XL2                                                              
IONUL    DC    2X'00'                                                           
IO       DS    XL30                                                             
         LTORG                                                                  
         EJECT                                                                  
ORDERTBL DS    0C                                                               
         DC    X'B1D85EFE955958E2A9CB020052',C'GEN'                             
         DC    X'B1D858FEEA59D642A9CB020052',C'GEN'                             
         DC    X'B1D84EFF1B5A1142A9CB020052',C'GEN'                             
         DC    X'B1D856FFA05A26E2A9CB020052',C'GEN'                             
         DC    X'B1D855FF975A4802A9CB020052',C'GEN'                             
         DC    X'B1D858FEE95A6482A9CB020052',C'GEN'                             
         DC    X'B1D84EFF1A5D3FA2A9CB020052',C'GEN'                             
         DC    X'B1D85EFEB15D7AA2A9CB020052',C'GEN'                             
         DC    X'B1D84EFF195F6242A9CB020052',C'GEN'                             
         DC    X'B1D84EFF185F74C2A9CB020052',C'GEN'                             
         DC    X'B1D85EFE7C5F8C82A9CB020052',C'GEN'                             
         DC    X'B1D856FF9F5FEFE2A9CB020052',C'GEN'                             
         DC    X'B1D851FFFFC43D62A9CB020052',C'GEN'                             
         DC    X'B1D857FF1BC47C82A9CB020052',C'GEN'                             
         DC    X'B1D850FF18C485E2A9CB020052',C'GEN'                             
         DC    X'B1D860FE90C496E2A9CB020052',C'GEN'                             
         DC    X'B1D858FF2CC4AB82A9CB020052',C'GEN'                             
         DC    X'B1D856FE25C4AD42A9CB020052',C'GEN'                             
         DC    X'B1D850FF17C4B942A9CB020052',C'GEN'                             
         DC    X'B1D86BFFFBC4C302A9CB020052',C'GEN'                             
         DC    X'B1D858FF2DC4C762A9CB020052',C'GEN'                             
         DC    X'B1D85EFE45C4C902A9CB020052',C'GEN'                             
         DC    X'B1D856FE24C4DB82A9CB020052',C'GEN'                             
         DC    X'B1D858FF2BC4ECA2A9CB020052',C'GEN'                             
         DC    X'B1D855FF96C50AE2A9CB020052',C'GEN'                             
         DC    X'B1D84EFF17C518E2A9CB020052',C'GEN'                             
         DC    X'B1D85FFF0BC57282A9CB020052',C'GEN'                             
         DC    X'B1D860FF3AC5BC82A9CB020052',C'GEN'                             
         DC    X'B1D851FFFEC5DB02A9CB020052',C'GEN'                             
         DC    X'B1D857FF1AC5F342A9CB020052',C'GEN'                             
         DC    X'B1D858FE23C603C2A9CB020052',C'GEN'                             
         DC    X'B1D85EFE63C60822A9CB020052',C'GEN'                             
         DC    X'B1D850FFF9C620E2A9CB020052',C'GEN'                             
         DC    X'B1D858FE22C622E2A9CB020052',C'GEN'                             
         DC    X'B1D857FF19C63902A9CB020052',C'GEN'                             
         DC    X'B1D858FF10C69402A9CB020052',C'GEN'                             
         DC    X'B1D858FEE8C6A9C2A9CB020052',C'GEN'                             
         DC    X'B1D850FFF8C6AD22A9CB020052',C'GEN'                             
         DC    X'B1D850FFF7C6B8E2A9CB020052',C'GEN'                             
         DC    X'B1D856FE23C6D802A9CB020052',C'GEN'                             
         DC    X'B1D851FFA2C6E202A9CB020052',C'GEN'                             
         DC    X'B1D856FE28C70C02A9CB020052',C'GEN'                             
         DC    X'B1D860FE55C70CA2A9CB020052',C'GEN'                             
         DC    X'B1D860FF39C77442A9CB020052',C'GEN'                             
         DC    X'B1D84FFF26C78242A9CB020052',C'GEN'                             
         DC    X'B1D85FFF0AC78442A9CB020052',C'GEN'                             
         DC    X'B1D851FFFDC79DA2A9CB020052',C'GEN'                             
         DC    X'B1D856FE27C7B082A9CB020052',C'GEN'                             
         DC    X'B1D858FE96C7B822A9CB020052',C'GEN'                             
         DC    X'B1D860FF38C7CFC2A9CB020052',C'GEN'                             
         DC    X'B1D85FFF09C7D922A9CB020052',C'GEN'                             
         DC    X'B1D858FE21C81B02A9CB020052',C'GEN'                             
         DC    X'B1D858FE95C822E2A9CB020052',C'GEN'                             
         DC    X'B1D856FE26C870C2A9CB020052',C'GEN'                             
         DC    X'B1D860FE54C892E2A9CB020052',C'GEN'                             
         DC    X'B1D855FF95C8B862A9CB020052',C'GEN'                             
         DC    X'B1D857FF97C90D22A9CB020052',C'GEN'                             
         DC    X'B1D86BFFFAC94E42A9CB020052',C'GEN'                             
         DC    X'B1D850FFF6C96022A9CB020052',C'GEN'                             
         DC    X'B1D86BFFF9C972C2A9CB020052',C'GEN'                             
         DC    X'B1D851FFFCC97562A9CB020052',C'GEN'                             
         DC    X'B1D86BFFF8C97D22A9CB020052',C'GEN'                             
         DC    X'B1D855FF94C983E2A9CB020052',C'GEN'                             
         DC    X'B1D851FFA1C9C702A9CB020052',C'GEN'                             
         DC    X'B1D851FFA0C9F522A9CB020052',C'GEN'                             
         DC    X'B1D856FE22CA2362A9CB020052',C'GEN'                             
         DC    X'B1D860FE53CA2782A9CB020052',C'GEN'                             
         DC    X'B1D858FFBDCA6C02A9CB020052',C'GEN'                             
         DC    X'B1D860FE8DCA9062A9CB020052',C'GEN'                             
         DC    X'B1D855FF93CA94C2A9CB020052',C'GEN'                             
         DC    X'B1D860FE44CAAE02A9CB020052',C'GEN'                             
         DC    X'B1D860FE8BCAD4E2A9CB020052',C'GEN'                             
         DC    X'B1D860FE42CAD702A9CB020052',C'GEN'                             
         DC    X'B1D86BFFF7CAE282A9CB020052',C'GEN'                             
         DC    X'B1D860FF37CAEC22A9CB020052',C'GEN'                             
         DC    X'B1D850FF16CB6B42A9CB020052',C'GEN'                             
         DC    X'B1D857FF96CBC662A9CB020052',C'GEN'                             
         DC    X'B1D857FF18CC0F42A9CB020052',C'GEN'                             
         DC    X'B1D860FE3FCC0FE2A9CB020052',C'GEN'                             
         DC    X'B1D857FF95CC1CC2A9CB020052',C'GEN'                             
         DC    X'B1D85FFF08CC2542A9CB020052',C'GEN'                             
         DC    X'B1D857FF94CCF8A2A9CB020052',C'GEN'                             
         DC    X'B1D88AFFA45AC682E23F010020',C'CAR'                             
         DC    X'B1D88AFFDE5B01C2E23F010020',C'CAR'                             
         DC    X'B1D88AFFDD5B7802E23F010020',C'CAR'                             
         DC    X'B1D89CFFD45B7962E23F010020',C'CAR'                             
         DC    X'B1D883FFDE5C3DE2E23F010020',C'CAR'                             
         DC    X'B1D883FFD65C9502E23F010020',C'CAR'                             
         DC    X'B1D89CFFD35CCF22E23F010020',C'CAR'                             
         DC    X'B1D887FFEC5CCF42E23F010020',C'CAR'                             
         DC    X'B1D89CFFD25D3182E23F010020',C'CAR'                             
         DC    X'B1D88AFFA35D98C2E23F010020',C'CAR'                             
         DC    X'B1D887FFEB5DEB82E23F010020',C'CAR'                             
         DC    X'B1D88AFFA25E2BA2E23F010020',C'CAR'                             
         DC    X'B1D887FFEA5EFEA2E23F010020',C'CAR'                             
         DC    X'B1D883FFDB5F0322E23F010020',C'CAR'                             
         DC    X'B1D88AFFDC5FEE42E23F010020',C'CAR'                             
         DC    X'B1D887FFE9613C22E23F010020',C'CAR'                             
         DC    X'B1D87CFFDC6143E2E23F010020',C'CAR'                             
         DC    X'B1D88AFFA15AC682E23F010021',C'CAR'                             
         DC    X'B1D88AFFE15B01C2E23F010021',C'CAR'                             
         DC    X'B1D88AFFE05B7802E23F010021',C'CAR'                             
         DC    X'B1D89CFFD15B7962E23F010021',C'CAR'                             
         DC    X'B1D883FFDD5C3DE2E23F010021',C'CAR'                             
         DC    X'B1D883FFD55C9502E23F010021',C'CAR'                             
         DC    X'B1D89CFFCF5CCF22E23F010021',C'CAR'                             
         DC    X'B1D887FFE85CCF42E23F010021',C'CAR'                             
         DC    X'B1D89CFFD05D3182E23F010021',C'CAR'                             
         DC    X'B1D88AFFA05D98C2E23F010021',C'CAR'                             
         DC    X'B1D887FFE75DEB82E23F010021',C'CAR'                             
         DC    X'B1D88AFF9F5E2BA2E23F010021',C'CAR'                             
         DC    X'B1D887FFE65EFEA2E23F010021',C'CAR'                             
         DC    X'B1D883FFDC5F0322E23F010021',C'CAR'                             
         DC    X'B1D88AFFDF5FEE42E23F010021',C'CAR'                             
         DC    X'B1D887FFE5613C22E23F010021',C'CAR'                             
         DC    X'B1D87CFFDB6143E2E23F010021',C'CAR'                             
         DC    X'B1D88AFF985AC682E23F010024',C'CAR'                             
         DC    X'B1D88AFFE85B01C2E23F010024',C'CAR'                             
         DC    X'B1D88AFFE75B7802E23F010024',C'CAR'                             
         DC    X'B1D89CFFC85B7962E23F010024',C'CAR'                             
         DC    X'B1D883FFD25C3DE2E23F010024',C'CAR'                             
         DC    X'B1D883FFCA5C9502E23F010024',C'CAR'                             
         DC    X'B1D89CFFC75CCF22E23F010024',C'CAR'                             
         DC    X'B1D887FFDE5CCF42E23F010024',C'CAR'                             
         DC    X'B1D89CFFC65D3182E23F010024',C'CAR'                             
         DC    X'B1D88AFF975D98C2E23F010024',C'CAR'                             
         DC    X'B1D887FFDD5DEB82E23F010024',C'CAR'                             
         DC    X'B1D88AFF965E2BA2E23F010024',C'CAR'                             
         DC    X'B1D887FFDC5EFEA2E23F010024',C'CAR'                             
         DC    X'B1D883FFCF5F0322E23F010024',C'CAR'                             
         DC    X'B1D88AFFE95FEE42E23F010024',C'CAR'                             
         DC    X'B1D887FFDB613C22E23F010024',C'CAR'                             
         DC    X'B1D88AFF955AC682E23F010025',C'CAR'                             
         DC    X'B1D88AFFEC5B01C2E23F010025',C'CAR'                             
         DC    X'B1D88AFFEB5B7802E23F010025',C'CAR'                             
         DC    X'B1D89CFFC55B7962E23F010025',C'CAR'                             
         DC    X'B1D883FFD15C3DE2E23F010025',C'CAR'                             
         DC    X'B1D883FFC95C9502E23F010025',C'CAR'                             
         DC    X'B1D89CFFC45CCF22E23F010025',C'CAR'                             
         DC    X'B1D89CFFC35D3182E23F010025',C'CAR'                             
         DC    X'B1D88AFF945D98C2E23F010025',C'CAR'                             
         DC    X'B1D887FFDA5DEB82E23F010025',C'CAR'                             
         DC    X'B1D88AFF935E2BA2E23F010025',C'CAR'                             
         DC    X'B1D887FFD95EFEA2E23F010025',C'CAR'                             
         DC    X'B1D883FFD05F0322E23F010025',C'CAR'                             
         DC    X'B1D88AFFEA5FEE42E23F010025',C'CAR'                             
         DC    X'B1D887FFD8613C22E23F010025',C'CAR'                             
         DC    X'B1D872FFFBC81982E41F0100DF',C'CAR'                             
         DC    X'FF'                                                            
         EJECT                                                                  
TABLED   DSECT                                                                  
TBLAGYMD DS    XL1                 BINARY AGENCY/MEDIA COMBO                    
TBLORDER DS    XL4                        ORDER NUMBER                          
TBLBSTA  DS    XL3                        STATION                               
TBLBCLT  DS    XL2                        CLIENT CODE                           
TBLBPRD  DS    XL1                        PRODUCT                               
TBLBPR2  DS    XL1                        PIGGYBACK PRD                         
TBLBEST  DS    XL1                        ESTIMATE                              
TBLQPRD  DS    CL3                 EBCDIC PRODUCT CODE                          
TBLLENQ  EQU   *-TABLED                                                         
         EJECT                                                                  
WORKD    DSECT                                                                  
DUB      DS    D                                                                
WORK     DS    CL64                                                             
DMCB     DS    6F                                                               
APARM    DS    A                                                                
VMSUNPK  DS    A                                                                
         DS    0F                                                               
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
*                                                                               
ADSNBFFR DS    A                                                                
WORKX    EQU   *                                                                
         EJECT                                                                  
*DMLDDEFN                                                                       
       ++INCLUDE DMLDDEFN                                                       
         EJECT                                                                  
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
       ++INCLUDE SPGENDRORD                                                     
       ++INCLUDE SPGENDRBTC                                                     
       ++INCLUDE SPGENDRMKN                                                     
       ++INCLUDE SPGENDRMKO                                                     
       ++INCLUDE SPGENDRFLT                                                     
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
         PRINT OFF                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'148SPLDEXTWH406/20/00'                                      
         END                                                                    
