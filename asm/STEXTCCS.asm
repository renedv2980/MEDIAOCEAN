*          DATA SET STEXTCCS   AT LEVEL 007 AS OF 04/23/92                      
*          DATA SET SPEXTWT    AT LEVEL 024 AS OF 04/02/90                      
*PHASE STEXTCC,*                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
         TITLE 'DMLDEXT - LOAD/DUMP MODEL EXTERNAL ROUTINE'                     
* SPOT4 MARKET FIX FOR CCUSA TV STATIONS AND MARKET RECS                        
* APR22/92                                                                      
         SPACE                                                                  
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
         NMOD1 20,DMLDEXT                                                       
         USING WORKD,RC                                                         
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
DMXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
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
         SPACE 2                                                                
DMXIT    XMOD1 1                                                                
         SPACE 2                                                                
*                                                                               
* INITIALISE LOGIC                                                              
*                                                                               
DMXINIT  DS    0H                                                               
         LA    R2,MTABLEN                                                       
         LA    R3,MTABLE                                                        
         SPACE                                                                  
INIT10   LA    R4,MTABLEN                                                       
         LA    R5,MTABLE                                                        
         SPACE                                                                  
INIT20   CR    R3,R5               SAME ENTRY                                   
         BE    INIT24                                                           
         CLC   0(4,R3),0(R5)       DUPE MKT                                     
         BNE   INIT22                                                           
         MVC   P(7),=C'OLD MKT'                                                 
         MVC   P+8(4),0(R3)                                                     
         MVC   P+20(10),=C'OLD MARKET'                                          
         MVC   P+31(4),0(R5)                                                    
         GOTO1 VPRINTER                                                         
         SPACE                                                                  
INIT22   CLC   4(4,R3),4(R5)  SAME NEW MARKET                                   
         BNE   INIT24                                                           
         MVC   P+10(7),=C'NEW MKT'                                              
         MVC   P+20(5),4(R3)                                                    
         MVC   P+30(5),4(R5)                                                    
         GOTO1 VPRINTER                                                         
         SPACE                                                                  
INIT24   LA    R5,L'MTABENT(,R5)                                                
         BCT   R4,INIT20                                                        
         LA    R3,L'MTABENT(,R3)                                                
         BCT   R2,INIT10                                                        
         SPACE                                                                  
         B     DMXIT                                                            
         EJECT                                                                  
*                                                                               
* PROCESS RECORD LOGIC                                                          
*                                                                               
DMXREC   DS    0H                                                               
         L     R3,AREC             POINT TO RECORD                              
         SPACE                                                                  
         AP    TOTRD,=P'1'                                                      
         SPACE                                                                  
         USING STARECD,R3                                                       
         CLI   STAKTYPE,C'M'       MARKET REC?                                  
         BE    STMKT                                                            
         CLI   STAKTYPE,C'S'       STATION REC?                                 
         BNE   DMXKEEP                                                          
         SPACE                                                                  
         CLI   STAKMED,C'T'        MEDIA TV                                     
         BNE   DMXKEEP                                                          
         SPACE                                                                  
         CLC   STAKAGY,=C'CC'      CCUSA                                        
         BNE   DMXKEEP                                                          
         SPACE                                                                  
         SPACE                                                                  
         AP    TSTACT,=P'1'                                                     
         SPACE                                                                  
         LA    R4,MTABLEN                                                       
         LA    R5,MTABLE                                                        
         USING MTABLED,R5                                                       
STSTA10  CLC   SMKT,MOLDMKT                                                     
         BNE   STSTA14                                                          
         MVC   SMKT,MNEWMKT                                                     
         MVC   P(7),=C'STA REC'                                                 
         MVC   P+10(7),=C'STATION'                                              
         MVC   P+18(5),STAKCALL                                                 
         MVC   P+10(6),=C'MARKET'                                               
         SPACE                                                                  
         MVC   P+20(7),=C'OLD MKT'                                              
         MVC   P+30(4),MOLDMKT                                                  
         SPACE                                                                  
         MVC   P+40(7),=C'NEW MKT'                                              
         MVC   P+50(4),MNEWMKT                                                  
         AP    TSTANEW,=P'1'                                                    
         BC    0,DMXKEEP                                                        
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'STA NEW MKT'                                            
         B     PRT                                                              
         SPACE                                                                  
STSTA14  LA    R5,MTABNXT                                                       
         BCT   R4,STSTA10                                                       
         SPACE                                                                  
         MVC   P(7),=C'STA REC'                                                 
         MVC   P+10(7),=C'MISSING'                                              
         MVC   P+20(5),STAKCALL                                                 
         MVC   P+30(4),SMKT                                                     
         GOTO1 VPRINTER                                                         
         LA    R4,=CL20'STA RECCORD'                                            
         B     DMXKEEP                                                          
         DROP  R3                                                               
         SPACE                                                                  
* FIX THE MARKET RECS HERE                                                      
         SPACE                                                                  
         USING MKTRECD,R3                                                       
STMKT    CLI   MKTKMED,C'T'        MEDIA TV                                     
         BNE   DMXKEEP                                                          
         SPACE                                                                  
         CLC   MKTKAGY,=C'CC'      CCUSA                                        
         BNE   DMXKEEP                                                          
         SPACE                                                                  
         AP    TMKTCT,=P'1'                                                     
         SPACE                                                                  
         LA    R4,MTABLEN                                                       
         LA    R5,MTABLE                                                        
         USING MTABLED,R5                                                       
STMKT10  CLC   MKTKMKT,MOLDMKT                                                  
         BNE   STMKT14                                                          
         MVC   MKTKMKT,MNEWMKT                                                  
         AP    TMKTNEW,=P'1'                                                    
         MVC   P(7),=C'MKT REC'                                                 
         MVC   P+10(7),=C'OLD MKT'                                              
         MVC   P+18(4),MOLDMKT                                                  
         MVC   P+25(3),=C'NEW'                                                  
         MVC   P+30(4),MNEWMKT                                                  
         GOTO1 VPRINTER                                                         
         BC    0,DMXKEEP                                                        
         MVI   *-3,X'F0'                                                        
         LA    R4,=CL20'MKT NEW MKT'                                            
         B     PRT                                                              
         B     DMXKEEP                                                          
         SPACE                                                                  
STMKT14  LA    R5,MTABNXT                                                       
         BCT   R4,STMKT10                                                       
         SPACE                                                                  
         MVC   P(7),=C'MKT REC'                                                 
         MVC   P+10(7),=C'MISSING'                                              
         MVC   P+20(4),MKTKMKT                                                  
         GOTO1 VPRINTER                                                         
         LA    R4,=CL20'MKT RECCORD'                                            
         B     DMXKEEP                                                          
         DROP  R3                                                               
         SPACE                                                                  
PRT      DS    0H                                                               
         LA    R5,116                                                           
         SPACE                                                                  
         GOTO1 =V(PRNTBL),DMCB,(20,(R4)),(R3),C'DUMP',(R5),=C'0D'               
         B     DMXKEEP                                                          
         SPACE                                                                  
*                                                                               
* END-OF-FILE LOGIC                                                             
*                                                                               
DMXEOF   DS    0H                                                               
         MVC   P(100),RTITLE                                                    
         GOTO1 VPRINTER                                                         
         LA    R2,TOTCTRS                                                       
         LA    R3,TOTRD                                                         
DMXEOF10 MVC   P+5(28),5(R3)                                                    
         EDIT  (P5,0(R3)),(8,P+33)                                              
         GOTO1 VPRINTER                                                         
         LA    R3,33(,R3)                                                       
         BCT   R2,DMXEOF10                                                      
         SPACE                                                                  
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         SPACE                                                                  
         B     DMXIT                                                            
         EJECT                                                                  
TOTRD    DC    PL5'0',CL28'TOT RECS READ'                                       
TSTACT   DC    PL5'0',CL28'TOT STA RECS'                                        
TSTANEW  DC    PL5'0',CL28'TOT STAS NEW'                                        
TMKTCT   DC    PL5'0',CL28'TOT MKT RECS'                                        
TMKTNEW  DC    PL5'0',CL28'TOT MKTS NEW'                                        
TOTCTRS  EQU   (*-TOTRD)/33                                                     
          SPACE                                                                 
WORK     DS    CL64                                                             
         SPACE                                                                  
RTITLE   DC    CL100'TV STATION REC MARKET FIX FOR CCUSA'                       
         LTORG                                                                  
          SPACE                                                                 
         DS    0D                                                               
MTABLE   DC    C'00119662',F'0'                                                 
         DC    C'00219657',F'0'                                                 
         DC    C'00619525',F'0'                                                 
         DC    C'00719532',F'0'                                                 
         DC    C'00819790',F'0'                                                 
         DC    C'00919644',F'0'                                                 
         DC    C'01109583',F'0'                                                 
         DC    C'01119634',F'0'                                                 
         DC    C'01219743',F'0'                                                 
         DC    C'01319002',F'0'                                                 
         DC    C'01519524',F'0'                                                 
         DC    C'01619520',F'0'                                                 
         DC    C'01719635',F'0'                                                 
         DC    C'01819800',F'0'                                                 
         DC    C'01919512',F'0'                                                 
         DC    C'02019537',F'0'                                                 
         DC    C'02119716',F'0'                                                 
         DC    C'02219692',F'0'                                                 
         DC    C'02319559',F'0'                                                 
         DC    C'02379531',F'0'                                                 
         DC    C'02519756',F'0'                                                 
         DC    C'02559821',F'0'                                                 
         DC    C'02619746',F'0'                                                 
         DC    C'02719502',F'0'                                                 
         DC    C'02819630',F'0'                                                 
         DC    C'02919687',F'0'                                                 
         DC    C'03119757',F'0'                                                 
         DC    C'03219506',F'0'                                                 
         DC    C'03319736',F'0'                                                 
         DC    C'03419006',F'0'                                                 
         DC    C'03519514',F'0'                                                 
         DC    C'03619523',F'0'                                                 
         DC    C'03719762',F'0'                                                 
         DC    C'03729754',F'0'                                                 
         DC    C'03819540',F'0'                                                 
         DC    C'03919767',F'0'                                                 
         DC    C'04019637',F'0'                                                 
         DC    C'04119564',F'0'                                                 
         DC    C'04219519',F'0'                                                 
         DC    C'04319517',F'0'                                                 
         DC    C'04359029',F'0'                                                 
         DC    C'04419575',F'0'                                                 
         DC    C'04519759',F'0'                                                 
         DC    C'04619602',F'0'                                                 
         DC    C'04719868',F'0'                                                 
         DC    C'04819648',F'0'                                                 
         DC    C'04919515',F'0'                                                 
         DC    C'05019598',F'0'                                                 
         DC    C'05119510',F'0'                                                 
         DC    C'05219752',F'0'                                                 
         DC    C'05329604',F'0'                                                 
         DC    C'05419546',F'0'                                                 
         DC    C'05519522',F'0'                                                 
         DC    C'05619673',F'0'                                                 
         DC    C'05719535',F'0'                                                 
         DC    C'05819600',F'0'                                                 
         DC    C'05919623',F'0'                                                 
         DC    C'06019682',F'0'                                                 
         DC    C'06119542',F'0'                                                 
         DC    C'06319751',F'0'                                                 
         DC    C'06419679',F'0'                                                 
         DC    C'06519505',F'0'                                                 
         DC    C'06619606',F'0'                                                 
         DC    C'06719676',F'0'                                                 
         DC    C'06849565',F'0'                                                 
         DC    C'06919765',F'0'                                                 
         DC    C'07119516',F'0'                                                 
         DC    C'07219801',F'0'                                                 
         DC    C'07319802',F'0'                                                 
         DC    C'07419649',F'0'                                                 
         DC    C'07519745',F'0'                                                 
         DC    C'07559024',F'0'                                                 
         DC    C'07619513',F'0'                                                 
         DC    C'07719615',F'0'                                                 
         DC    C'07819570',F'0'                                                 
         DC    C'07919571',F'0'                                                 
         DC    C'08019670',F'0'                                                 
         DC    C'08119866',F'0'                                                 
         DC    C'08219724',F'0'                                                 
         DC    C'08419509',F'0'                                                 
         DC    C'08459592',F'0'                                                 
         DC    C'08519798',F'0'                                                 
         DC    C'08619563',F'0'                                                 
         DC    C'08719755',F'0'                                                 
         DC    C'08819658',F'0'                                                 
         DC    C'08919518',F'0'                                                 
         DC    C'09019647',F'0'                                                 
         DC    C'09119773',F'0'                                                 
         DC    C'09219567',F'0'                                                 
         DC    C'09319545',F'0'                                                 
         DC    C'09419636',F'0'                                                 
         DC    C'09519569',F'0'                                                 
         DC    C'09619710',F'0'                                                 
         DC    C'09819766',F'0'                                                 
         DC    C'09919744',F'0'                                                 
         DC    C'10019618',F'0'                                                 
         DC    C'10119533',F'0'                                                 
         DC    C'10219691',F'0'                                                 
         DC    C'10319758',F'0'                                                 
         DC    C'10419527',F'0'                                                 
         DC    C'10449718',F'0'                                                 
         DC    C'10519639',F'0'                                                 
         DC    C'10619561',F'0'                                                 
         DC    C'10809013',F'0'                                                 
         DC    C'10819574',F'0'                                                 
         DC    C'10919734',F'0'                                                 
         DC    C'11019603',F'0'                                                 
         DC    C'11219616',F'0'                                                 
         DC    C'11419557',F'0'                                                 
         DC    C'11519702',F'0'                                                 
         DC    C'11619582',F'0'                                                 
         DC    C'11719642',F'0'                                                 
         DC    C'11819643',F'0'                                                 
         DC    C'11919566',F'0'                                                 
         DC    C'12019551',F'0'                                                 
         DC    C'12119749',F'0'                                                 
         DC    C'12219839',F'0'                                                 
         DC    C'12259015',F'0'                                                 
         DC    C'12319541',F'0'                                                 
         DC    C'12419558',F'0'                                                 
         DC    C'12519722',F'0'                                                 
         DC    C'12619693',F'0'                                                 
         DC    C'12719803',F'0'                                                 
         DC    C'12819529',F'0'                                                 
         DC    C'12919651',F'0'                                                 
         DC    C'13019016',F'0'                                                 
         DC    C'13119503',F'0'                                                 
         DC    C'13219669',F'0'                                                 
         DC    C'13419737',F'0'                                                 
         DC    C'13519553',F'0'                                                 
         DC    C'13619611',F'0'                                                 
         DC    C'13719813',F'0'                                                 
         DC    C'13819640',F'0'                                                 
         DC    C'13919711',F'0'                                                 
         DC    C'14019528',F'0'                                                 
         DC    C'14119617',F'0'                                                 
         DC    C'14219613',F'0'                                                 
         DC    C'14519686',F'0'                                                 
         DC    C'14619628',F'0'                                                 
         DC    C'14649828',F'0'                                                 
         DC    C'14719698',F'0'                                                 
         DC    C'15019659',F'0'                                                 
         DC    C'15119622',F'0'                                                 
         DC    C'15219501',F'0'                                                 
         DC    C'15319740',F'0'                                                 
         DC    C'15419544',F'0'                                                 
         DC    C'15519633',F'0'                                                 
         DC    C'15619650',F'0'                                                 
         DC    C'15719652',F'0'                                                 
         DC    C'15819534',F'0'                                                 
         DC    C'15919631',F'0'                                                 
         DC    C'15949804',F'0'                                                 
         DC    C'16019656',F'0'                                                 
         DC    C'16119597',F'0'                                                 
         DC    C'16219632',F'0'                                                 
         DC    C'16419675',F'0'                                                 
         DC    C'16519504',F'0'                                                 
         DC    C'16619753',F'0'                                                 
         DC    C'16719508',F'0'                                                 
         DC    C'16819500',F'0'                                                 
         DC    C'16919820',F'0'                                                 
         DC    C'17019552',F'0'                                                 
         DC    C'17119521',F'0'                                                 
         DC    C'17219717',F'0'                                                 
         DC    C'17319560',F'0'                                                 
         DC    C'17419764',F'0'                                                 
         DC    C'17519811',F'0'                                                 
         DC    C'17719556',F'0'                                                 
         DC    C'17819020',F'0'                                                 
         DC    C'17919573',F'0'                                                 
         DC    C'18019538',F'0'                                                 
         DC    C'18119610',F'0'                                                 
         DC    C'18219761',F'0'                                                 
         DC    C'18319862',F'0'                                                 
         DC    C'18419576',F'0'                                                 
         DC    C'18519770',F'0'                                                 
         DC    C'18619661',F'0'                                                 
         DC    C'18719641',F'0'                                                 
         DC    C'18819825',F'0'                                                 
         DC    C'18919807',F'0'                                                 
         DC    C'19119855',F'0'                                                 
         DC    C'19219021',F'0'                                                 
         DC    C'19319507',F'0'                                                 
         DC    C'19419819',F'0'                                                 
         DC    C'19619612',F'0'                                                 
         DC    C'19719624',F'0'                                                 
         DC    C'19819725',F'0'                                                 
         DC    C'19889023',F'0'                                                 
         DC    C'20119588',F'0'                                                 
         DC    C'20219881',F'0'                                                 
         DC    C'20319619',F'0'                                                 
         DC    C'20419543',F'0'                                                 
         DC    C'20519609',F'0'                                                 
         DC    C'20619638',F'0'                                                 
         DC    C'20719555',F'0'                                                 
         DC    C'20819530',F'0'                                                 
         DC    C'20919539',F'0'                                                 
         DC    C'21019581',F'0'                                                 
         DC    C'21119547',F'0'                                                 
         DC    C'21219605',F'0'                                                 
         DC    C'21319789',F'0'                                                 
         DC    C'21419671',F'0'                                                 
         DC    C'21559620',F'0'                                                 
         DC    C'21619760',F'0'                                                 
         DC    C'21719709',F'0'                                                 
         DC    C'21819526',F'0'                                                 
         DC    C'21859626',F'0'                                                 
         DC    C'22019625',F'0'                                                 
         DC    C'22119511',F'0'                                                 
         DC    C'22219549',F'0'                                                 
         DC    C'22319705',F'0'                                                 
         DC    C'22419548',F'0'                                                 
         DC    C'22519554',F'0'                                                 
         DC    C'22559025',F'0'                                                 
         DC    C'22619627',F'0'                                                 
         DC    C'22719678',F'0'                                                 
         DC    C'22819577',F'0'                                                 
         DC    C'22919550',F'0'                                                 
         DC    C'23049810',F'0'                                                 
         DC    C'23119536',F'0'                                                 
         DC    C'23219771',F'0'                                                 
         DC    C'23319596',F'0'                                                 
         DC    C'91129112',F'0'                                                 
MTABLEN  EQU   (*-MTABLE)/L'MTABENT                                             
         SPACE 2                                                                
MISMKTAB DC    400F'0'                                                          
         SPACE 2                                                                
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
APARM    DS    A                                                                
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
MTABLED  DSECT                                                                  
MTABENT  DS   0XL12                                                             
MOLDMKT  DS    CL4                                                              
MNEWMKT  DS    CL4                                                              
MOMKTB   DS    XL2                                                              
MNMKTB   DS    XL2                                                              
MTABNXT  EQU   *                                                                
         EJECT                                                                  
*DMLDDEFN                                                                       
       ++INCLUDE DMLDDEFN                                                       
         EJECT                                                                  
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
         SPACE 2                                                                
STARECD   DSECT                                                                 
       ++INCLUDE SPGENSTA                                                       
MKTRECD   DSECT                                                                 
       ++INCLUDE SPGENMKT                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007STEXTCCS  04/23/92'                                      
         END                                                                    
