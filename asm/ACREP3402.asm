*          DATA SET ACREP3402  AT LEVEL 031 AS OF 06/03/15                      
*PHASE AC3402A,*                                                                
*INCLUDE SQUASHER                                                               
*INCLUDE SORTER                                                                 
         TITLE 'CLIENT INCOME ANALYSIS'                                         
AC3402   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**AC34**,R9,RR=R5                                              
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING AC3402D,RC                                                       
         ST    R5,RELO                                                          
         EJECT                                                                  
*              RUNFRST                                                          
         SPACE 2                                                                
CIA1     CLI   MODE,RUNFRST                                                     
         BNE   CIA2                                                             
         SPACE 1                                                                
         L     RF,=A(BUFFALOC)                                                  
         A     RF,RELO                                                          
         ST    RF,ABUFF                                                         
         B     CIAXIT                                                           
         EJECT                                                                  
*              REQFRST                                                          
         SPACE 2                                                                
CIA2     CLI   MODE,REQFRST                                                     
         BNE   CIA3                                                             
         SPACE 1                                                                
         BAS   RE,SETSORT                                                       
         GOTO1 MYROLLER,DMCB,0,ACCUMS,5,3                                       
         SPACE 1                                                                
         LA    R1,SORTREC1         CLEAR SORTRECORDS                            
         LA    R2,4                                                             
         MVC   0(L'SORTREC1,R1),SPACES                                          
         LA    R1,L'SORTREC1(R1)                                                
         BCT   R2,*-10                                                          
         SPACE 1                                                                
         MVI   SORTREC1+(SORTTYPE-SORTRECD),C'C'                                
         MVI   SORTREC2+(SORTTYPE-SORTRECD),C'P'                                
         MVI   SORTREC3+(SORTTYPE-SORTRECD),C'I'                                
         MVI   SORTREC4+(SORTTYPE-SORTRECD),C'T'                                
         SPACE 1                                                                
         MVC   BTCHM(1),QSTART+1   BUILD BATCH MONTH                            
         MVC   BTCHM+1(1),QSTART+3                                              
         CLC   QSTART+2(2),=C'10'                                               
         BL    CIA2A                                                            
         MVI   BTCHM+1,C'A'                                                     
         CLC   QSTART+2(2),=C'11'                                               
         BL    CIA2A                                                            
         MVI   BTCHM+1,C'B'                                                     
         CLC   QSTART+2(2),=C'12'                                               
         BL    CIA2A                                                            
         MVI   BTCHM+1,C'C'                                                     
         SPACE 1                                                                
CIA2A    MVC   MYHEAD4,SPACES                                                   
         MVC   MYHEAD4+1(6),=C'CLIENT'                                          
         MVC   MYHEAD4+83(16),=C'FOR THE MONTH OF'                              
         SPACE 1                                                                
         MVC   WORK(4),QSTART                                                   
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(6,MYHEAD4+100)                             
         MVI   DATNO,1                                                          
         CLC   QEND,SPACES         ARE WE DEALING WITH 2 DATES                  
         BNE   CIA2B                                                            
         MVI   QOPT3,C'N'          IF NOT FORCE NO SUMMARY                      
         B     CIA2C                                                            
CIA2B    MVI   DATNO,2                                                          
         MVC   QSTART+4(2),=C'01'                                               
         GOTO1 DATCON,DMCB,(0,QSTART),(1,START3)                                
         MVC   QEND+4(2),=C'01'                                                 
         GOTO1 (RF),(R1),(0,QEND),(1,END3)                                      
         MVC   MYHEAD4+83(22),=C'FROM XXXXX  TO XXXXX '                         
         MVC   MYHEAD4+100(6),SPACES                                            
         GOTO1 DATCON,DMCB,(0,QSTART),(9,MYHEAD4+88)                            
         GOTO1 (RF),(R1),(0,QEND),(9,MYHEAD4+98)                                
         SPACE 1                                                                
CIA2C    MVC   SAVEKEY,SPACES                                                   
         MVC   SAVEKEY(15),KEY                                                  
         L     R4,=A(ACCBUFF)                                                   
         A     R4,RELO                                                          
         MVC   0(42,R4),SPACES                                                  
         MVC   0(1,R4),QCOMPANY                                                 
         MVC   1(2,R4),=C'SJ'                                                   
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCOUNT',(R4),(R4)                    
         MVI   BYTE,X'16'                                                       
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING ACHEIRD,R4                                                       
         ZIC   R1,ACHRLEVA                                                      
         STC   R1,CLILEN                                                        
         ZIC   R2,ACHRLEVB                                                      
         SR    R2,R1                                                            
         STC   R2,PRODLEN                                                       
         SPACE 1                                                                
         L     R4,=A(ACCBUFF)                                                   
         A     R4,RELO                                                          
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCOUNT',SAVEKEY,(R4)                 
         SPACE 1                                                                
         CLI   QOPT3,C'Y'          CLIENT SUMMARY                               
         BE    CIA2D                                                            
         CLI   QOPT3,C'O'          CLIENT SUMMARY ONLY                          
         BNE   CIAXIT                                                           
CIA2D    GOTO1 BUFFALO,DMCB,=C'SET',ABUFF                                       
         B     CIAXIT                                                           
         EJECT                                                                  
*              ACCFRST                                                          
         SPACE 2                                                                
CIA3     CLI   MODE,ACCFRST                                                     
         BNE   CIA4                                                             
         MVI   INCOK,X'80'         INCOME AC RECORD'S KEY HAS CHANGED           
         LA    R6,SORTREC3                                                      
         USING SORTRECD,R6                                                      
         MVC   SINCKEY,SPACES                                                   
         L     R4,ADACC                                                         
         MVC   SINCKEY,3(R4)                                                    
         MVC   SORTDATA,SPACES                                                  
         BAS   RE,REKEY                                                         
         SPACE 1                                                                
         L     R4,ADACCNAM                                                      
         USING ACNAMED,R4                                                       
         ZIC   R1,ACNMLEN                                                       
         SH    R1,=H'03'                                                        
         EXMVC R1,SORTDATA,ACNMNAME                                             
         B     CIAXIT                                                           
         EJECT                                                                  
*              SBACFRST                                                         
         SPACE 2                                                                
CIA4     CLI   MODE,SBACFRST                                                    
         BNE   CIA5                                                             
         SPACE 1                                                                
         LA    R6,SORTREC1         CLIENT RECORD                                
         USING SORTRECD,R6                                                      
         MVI   READTRNS,C'Y'                                                    
         L     R4,ADSUBAC                                                       
         LA    R4,TRSBACNT-TRSUBHD(R4)                                          
         CLC   1(2,R4),=C'SJ'                                                   
         BE    CIA41                                                            
         CLC   1(2,R4),=C'SM'                                                   
         BNE   CIA4A0                                                           
CIA41    ZIC   R1,CLILEN                                                        
         BCTR  R1,0                                                             
         CLC   QSELECT,SPACES                                                   
         BE    CIA4A                                                            
         EXCLC R1,QSELECT,3(R4)                                                 
         BE    CIA4AA                                                           
         TM    QSELECT,X'40'                                                    
         BO    CIA4A0                                                           
         NI    3(R4),X'BF'                                                      
         EXCLC R1,QSELECT,3(R4)                                                 
         BE    CIA4A0                                                           
         OI    3(R4),X'40'                                                      
         B     CIA4A                                                            
CIA4A0   MVI   READTRNS,C'N'       NOT SJ OR CLIENT REQ                         
         B     CIAXIT                                                           
         SPACE 1                                                                
CIA4A    ZIC   R1,CLILEN                                                        
         BCTR  R1,0                                                             
         MVI   BYTE,0                                                           
         EXCLC R1,SCLIKEY,3(R4)                                                 
         BE    CIA4B               SAME CLIENT                                  
CIA4AA   MVC   SCLIKEY,SPACES                                                   
         EXMVC R1,SCLIKEY,3(R4)                                                 
*&&US*&& MVC   SORTUL(2),1(R4)     U/L - MAY BE SM, NOT SJ                      
         MVI   CLIOK,X'80'                                                      
         BAS   RE,REKEY                                                         
         MVI   BYTE,1                                                           
         SPACE 1                                                                
CIA4B    LA    R6,SORTREC2         PRODUCT RECORD                               
         ZIC   R2,PRODLEN                                                       
         BCTR  R2,0                                                             
         LA    R4,4(R1,R4)         POINT TO START OF PRODUCT KEY                
         EXCLC R2,SPACES,0(R4)                                                  
         BNE   CIA4BA                                                           
         MVC   SPRODKEY,=CL6'ALL'                                               
         B     CIA4CA                                                           
         SPACE 1                                                                
CIA4BA   OC    BYTE,BYTE                                                        
         BNZ   CIA4C               NEW CLIENT,MUST BE NEW PRODUCT               
         EXCLC R2,SPRODKEY,0(R4)                                                
         BE    CIA4D               SAME PRODUCT                                 
         SPACE 1                                                                
CIA4C    MVC   SPRODKEY,SPACES                                                  
         EXMVC R2,SPRODKEY,0(R4)                                                
CIA4CA   MVI   PRODOK,X'80'                                                     
         OI    INCOK,X'80'         INCOME AC RECORD'S KEY HAS CHANGED           
         BAS   RE,REKEY                                                         
         L     R4,ADSUBAC                                                       
         USING TRSUBHD,R4                                                       
         SPACE 1                                                                
         MVC   SORTDATA,SPACES                                                  
         ZIC   R1,TRSBLEN                                                       
         SH    R1,=H'18'                                                        
         LTR   R1,R1                                                            
         BM    CIA4D                                                            
         EXMVC R1,SORTDATA,TRSBNAME                                             
         SPACE 1                                                                
CIA4D    B     CIAXIT                                                           
         EJECT                                                                  
*              PROCTRNS                                                         
         SPACE 2                                                                
CIA5     CLI   MODE,PROCTRNS                                                    
         BNE   CIA6                                                             
         SPACE 1                                                                
         CLI   READTRNS,C'Y'                                                    
         BNE   CIAXIT                                                           
         SPACE 1                                                                
         L     R4,ADTRANS                                                       
         USING TRANSD,R4                                                        
         CLI   TRNSEL,X'44'                                                     
         BNE   CIAXIT                                                           
         SPACE 1                                                                
         CLI   DATNO,2             ARE WE DEALING WITH 2 DATES                  
         BE    CIA5A                                                            
         CLC   BTCHM,TRNSBTCH                                                   
         BNE   CIAXIT                                                           
         B     CIA5C                                                            
*                                                                               
CIA5A    DS    0H                                                               
         USING ACMD,R2                                                          
         L     R2,AMONACC          GET MOS FROM MONACC                          
         MVC   WORK(2),ACMMDTE                                                  
         DROP  R2                                                               
*                                                                               
         CLC   WORK(2),START3      TEST CONVERTED MOS(AS YYMM)                  
         BL    CIAXIT              AGAINST START/END DATES                      
         CLC   WORK(2),END3                                                     
         BH    CIAXIT                                                           
*                                                                               
CIA5C    LA    R6,SORTREC4                                                      
         USING SORTRECD,R6                                                      
         MVC   SINVNUM,TRNSREF                                                  
         MVC   SINVDATE,TRNSDATE                                                
         ZAP   SINVGROS,TRNSAMNT                                                
         ZAP   SINVCOMM,TRNSAMNT                                                
         MVC   SINVMOS,WORK        SAVE MOS AS PWOS YYMM                        
         SPACE 1                                                                
         MVI   BYTE,X'50'                                                       
CIA5E    BAS   RE,NEXTEL                                                        
         BNE   CIA5EA                                                           
         SPACE 1                                                                
         USING TRCASHD,R4                                                       
         CLI   TRCSTYPE,C'G'                                                    
         BNE   CIA5E               NOT GROSS ELEMENT,TRY AGAIN                  
         ZAP   SINVGROS,TRCSAMNT                                                
         B     CIA5G                                                            
         SPACE 1                                                                
CIA5EA   L     R4,ADTRANS                                                       
         USING TRANSD,R4                                                        
         CLI   TRNSTYPE,8                                                       
         BE    CIA5F                                                            
         CLI   TRNSTYPE,21                                                      
         BE    CIA5F                                                            
*&&US                                                                           
         CLI   TRNSTYPE,0          BUG IN BILLING                               
         BE    CIA5F                                                            
         CLI   TRNSTYPE,6                                                       
         BE    CIA5F                                                            
         CLI   TRNSTYPE,7                                                       
         BE    CIA5F                                                            
         CLI   TRNSTYPE,9                                                       
         BE    CIA5F                                                            
*&&                                                                             
         B     CIA5G                                                            
CIA5F    ZAP   SINVGROS,=P'0'                                                   
         SPACE 1                                                                
CIA5G    OI    CLIOK,1             O.K. RELEASE OF CLIENT,PRODUCT               
         OI    INCOK,1             INCOME ACCOUNT RECORDS                       
         OI    PRODOK,1                                                         
         BAS   RE,PUTSORT          RELEASE TRANSACTION RECORD                   
         B     CIAXIT                                                           
         EJECT                                                                  
*              SBACLAST                                                         
         SPACE 2                                                                
CIA6     CLI   MODE,SBACLAST                                                    
         BNE   CIA7                                                             
         SPACE 1                                                                
         MVI   READTRNS,C'Y'                                                    
         LA    R6,SORTREC1                                                      
         USING SORTRECD,R6                                                      
         TM    CLIOK,X'81'                                                      
         BNO   CIA6A                                                            
         SPACE 1                                                                
         BAS   RE,PUTSORT                                                       
         MVC   LASTSORT,SORTREC                                                 
         MVC   SPRODKEY,=12X'FF'   PUT OUT A LAST RECORD                        
         MVC   SINCKEY,=12X'FF'                                                 
         MVC   SINVNUM,=12X'FF'                                                 
         BAS   RE,PUTSORT                                                       
         MVC   SORTREC,LASTSORT                                                 
         XC    CLIOK,CLIOK                                                      
         SPACE 1                                                                
CIA6A    LA    R6,SORTREC2                                                      
         TM    PRODOK,X'81'                                                     
         BNO   CIA6B                                                            
         SPACE 1                                                                
         BAS   RE,PUTSORT                                                       
         MVC   LASTSORT,SORTREC                                                 
         MVC   SINCKEY,=12X'FF'                                                 
         MVC   SINVNUM,=12X'FF'                                                 
         BAS   RE,PUTSORT                                                       
         MVC   SORTREC,LASTSORT                                                 
         XC    PRODOK,PRODOK                                                    
         SPACE 1                                                                
CIA6B    LA    R6,SORTREC3                                                      
         TM    INCOK,X'81'                                                      
         BNO   CIA6C                                                            
         BAS   RE,PUTSORT                                                       
         MVC   LASTSORT,SORTREC                                                 
         MVC   SINVNUM,=12X'FF'                                                 
         BAS   RE,PUTSORT                                                       
         MVC   SORTREC,LASTSORT                                                 
         XC    INCOK,INCOK                                                      
         SPACE 1                                                                
CIA6C    EQU   *                                                                
         B     CIAXIT                                                           
         EJECT                                                                  
*              REQLAST                                                          
         SPACE 2                                                                
CIA7     CLI   MODE,REQLAST                                                     
         BNE   CIA8                                                             
         SPACE 1                                                                
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         XC    LASTSORT,LASTSORT                                                
         MVI   LASTSPAC,2                                                       
         ZAP   CLICONT,=P'0'                                                    
         ZAP   PRODCONT,=P'0'                                                   
         ZAP   INCCONT,=P'0'                                                    
         ZAP   TRNSCONT,=P'0'                                                   
         LA    R4,P                                                             
         USING PLINED,R4                                                        
         USING SORTRECD,R6                                                      
         LA    R6,SORTREC1                                                      
         SPACE 1                                                                
CIA7A    BAS   RE,GETSORT                                                       
         SPACE 1                                                                
         CLC   SORTREC,SPACES                                                   
         BE    CIA7X               E.O.F.                                       
         SPACE 1                                                                
         CLI   SORTTYPE,C'T'                                                    
         BE    CIA7E                                                            
         CLC   SORTKEY,LASTSORT                                                 
         BE    CIA7A               DUPLICATE RECORD QUITE POSSIBLE              
         MVC   LASTSORT,SORTREC                                                 
         SPACE 1                                                                
         CLI   SORTTYPE,C'C'                                                    
         BNE   CIA7C                                                            
         CLC   SINVNUM,=12X'FF'                                                 
         BE    CIA7H                                                            
         SPACE 1                                                                
CIA7B    MVI   FORCEHED,C'Y'       1ST FOR CLIENT                               
         BAS   RE,GETNAME                                                       
         MVC   MYHEAD4+8(60),SPACES                                             
         MVC   WORK,SPACES                                                      
         MVC   WORK(L'SCLIKEY),SCLIKEY                                          
*&&US*&& MVC   WORK+3(2),SPACES    DON'T PRINT U/L                              
         MVC   WORK+L'SCLIKEY+1(L'SORTDATA),SORTDATA                            
         LA    R2,L'SCLIKEY+L'SORTDATA+1                                        
         SPACE 1                                                                
         GOTO1 =V(SQUASHER),DMCB,WORK,(R2),RR=RB                                
         SPACE 1                                                                
         L     R2,DMCB+4           NEW LENGTH OF SQUASHED DATA                  
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   MYHEAD4+8(0),WORK                                                
         SPACE 1                                                                
         AP    CLICONT,=P'1'                                                    
         ZAP   PRODCONT,=P'0'                                                   
         MVI   LASTSPAC,2                                                       
         LA    R2,4                                                             
         B     CIA7Z                                                            
         SPACE 2                                                                
CIA7C    CLI   SORTTYPE,C'P'                                                    
         BNE   CIA7D                                                            
         CLC   SINVNUM,=12X'FF'                                                 
         BE    CIA7G                                                            
         SPACE 1                                                                
         CLI   LASTSPAC,2          1ST FOR PRODUCT                              
         BNL   CIA7CB                                                           
         CLI   FORCEHED,C'Y'                                                    
         BE    CIA7CB                                                           
         CLI   QOPT3,C'O'                                                       
         BE    *+8                                                              
         BAS   RE,CIAREPRT         PRINT A BLANK LINE                           
         SPACE 1                                                                
CIA7CB   MVC   WORK,SPACES                                                      
         MVC   WORK(L'SPRODKEY),SPRODKEY                                        
         MVC   WORK+L'SPRODKEY+1(L'SORTDATA),SORTDATA                           
         LA    R2,L'SPRODKEY+L'SORTDATA+1                                       
         GOTO1 =V(SQUASHER),DMCB,WORK,(R2),RR=RB                                
         L     R2,DMCB+4                                                        
         BCTR  R2,0                                                             
         EXMVC R2,PRODNAME,WORK                                                 
         MVI   SPACING,2                                                        
         SPACE 1                                                                
         AP    PRODCONT,=P'1'                                                   
         ZAP   INCCONT,=P'0'                                                    
         SPACE 1                                                                
         CLI   QOPT3,C'O'                                                       
         BE    *+8                                                              
         BAS   RE,CIAREPRT                                                      
         LA    R2,3                                                             
         B     CIA7Z                                                            
         SPACE 2                                                                
CIA7D    CLI   SORTTYPE,C'I'                                                    
         BNE   CIA7A               DUD RECORD,IGNORE IT                         
         CLC   SINVNUM,=12X'FF'                                                 
         BE    CIA7F                                                            
         SPACE 1                                                                
         CLI   LASTSPAC,2          1ST FOR INCOME ACCOUNT                       
         BNL   CIA7DA                                                           
         CLI   QOPT3,C'O'                                                       
         BE    *+8                                                              
         BAS   RE,CIAREPRT         PRINT A BLANK LINE                           
CIA7DA   MVC   CHOPBLOC,SPACES                                                  
         MVC   CHOPBLOC+L'CHOPBLOC,SPACES                                       
         MVC   WORK,SPACES                                                      
         MVC   WORK(L'SINCKEY),SINCKEY                                          
         MVC   WORK+L'SINCKEY+1(L'SORTDATA),SORTDATA                            
         LA    R2,L'SINCKEY+L'SORTDATA+1                                        
         GOTO1 =V(SQUASHER),DMCB,WORK,(R2),RR=RB                                
         L     R2,DMCB+4                                                        
         LA    R3,L'CHOPBLOC                                                    
         GOTO1 CHOPPER,DMCB,((R2),WORK),((R3),CHOPBLOC),2                       
         AP    INCCONT,=P'1'                                                    
         ZAP   TRNSCONT,=P'0'                                                   
         LA    R2,2                                                             
         MVC   BUFFNAME,SORTDATA   SAVE CONTRA NAME                             
         B     CIA7Z                                                            
         SPACE 2                                                                
CIA7E    EQU   *                                                                
         CLC   CHOPBLOC,SPACES     TRANSACTION                                  
         BE    CIA7E1                                                           
         MVC   PINCNAME,CHOPBLOC                                                
         MVC   CHOPBLOC,CHOPBLOC+L'CHOPBLOC                                     
         MVC   CHOPBLOC+L'CHOPBLOC,SPACES                                       
         SPACE 1                                                                
CIA7E1   MVC   PINVNUM,SINVNUM                                                  
         GOTO1 DATCON,DMCB,(1,SINVDATE),(5,PINVDATE)                            
         SPACE 1                                                                
         AP    TRNSCONT,=P'1'                                                   
         SPACE 1                                                                
         GOTO1 MYROLLER,DMCB,2,ACCUMS,1      CLEAR                              
         GOTO1 (RF),(R1),1                   FIND THE LINE                      
         L     R2,DMCB                                                          
         ZAP   0(8,R2),SINVGROS              UPDATE FIGURES                     
         ZAP   8(8,R2),SINVCOMM                                                 
         GOTO1 (RF),(R1),6         CROSS CAST & ADD DOWN                        
         LA    R2,1                                                             
         BAS   RE,FORMAT                                                        
         CLI   QOPT3,C'O'                                                       
         BE    *+8                                                              
         BAS   RE,CIAREPRT                                                      
         CLI   QOPT3,C'Y'                                                       
         BE    CIA7E3                                                           
         CLI   QOPT3,C'O'                                                       
         BNE   CIA7A                                                            
CIA7E3   MVC   BUFFKAC,SINCKEY                                                  
         MVC   BUFFKMON,SINVMOS                                                 
         ZAP   BUFFGRSS,SINVGROS                                                
         ZAP   BUFFCOMM,SINVCOMM                                                
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,BUFFREC                               
         MVC   BUFFKMON,=12X'FE'   INCOME ACCT TOTAL                            
         BASR  RE,RF                                                            
         MVC   BUFFKAC,=12X'FE'    CLIENT TOTAL                                 
         BASR  RE,RF                                                            
         MVC   BUFFKAC,=12X'FF'    MONTHS FOR ALL INCOME ACCOUNTS               
         MVC   BUFFKMON,SINVMOS                                                 
         BASR  RE,RF                                                            
         B     CIA7A                                                            
         SPACE 2                                                                
CIA7F    EQU   *                                                                
         CLC   CHOPBLOC,SPACES     LAST FOR INCOME ACCOUNT                      
         BE    CIA7F1                                                           
         MVC   PINCNAME,CHOPBLOC                                                
         MVC   CHOPBLOC,SPACES                                                  
         CLI   QOPT3,C'O'                                                       
         BE    *+8                                                              
         BAS   RE,CIAREPRT                                                      
         SPACE 1                                                                
CIA7F1   CP    TRNSCONT,=P'1'                                                   
         BNH   CIA7F2              NO SUMMARY                                   
         MVC   WORK,SPACES                                                      
         MVC   WORK(L'SINCKEY),SINCKEY                                          
         MVC   WORK+L'SINCKEY+1(5),=C'TOTAL'                                    
         LA    R2,L'SINCKEY+6                                                   
         GOTO1 =V(SQUASHER),DMCB,WORK,(R2),RR=RB                                
         MVC   PINCNAME,WORK                                                    
         LA    R2,2                                                             
         BAS   RE,FORMAT                                                        
CIA7F2   MVI   SPACING,2                                                        
         CLI   QOPT3,C'O'                                                       
         BE    *+8                                                              
         BAS   RE,CIAREPRT                                                      
         B     CIA7A                                                            
         SPACE 2                                                                
CIA7G    EQU   *                                                                
         CP    INCCONT,=P'1'       LAST FOR PRODUCT                             
         BNH   CIA7G2              NO SUMMARY                                   
         CLI   LASTSPAC,2                                                       
         BNL   CIA7G1                                                           
         CLI   QOPT3,C'O'                                                       
         BE    CIA7G1                                                           
         BAS   RE,CIAREPRT                                                      
CIA7G1   MVC   PRODNAME(13),=C'PRODUCT TOTAL'                                   
         LA    R2,3                                                             
         BAS   RE,FORMAT                                                        
         MVI   SPACING,2                                                        
         CLI   QOPT3,C'O'                                                       
         BE    *+8                                                              
         BAS   RE,CIAREPRT                                                      
CIA7G2   CLI   QOPT1,C'Y'          NEW PAGE/PRODUCT                             
         BNE   CIA7A                                                            
         MVI   FORCEHED,C'Y'                                                    
         B     CIA7A                                                            
         SPACE 2                                                                
CIA7H    EQU   *                                                                
         CLI   LASTSPAC,2          LAST FOR CLIENT                              
         BNL   CIA7J                                                            
         CLI   QOPT3,C'O'                                                       
         BE    CIA7J                                                            
         BAS   RE,CIAREPRT         PRINT A BLANK LINE                           
CIA7J    MVC   PRODNAME(12),=C'CLIENT TOTAL'                                    
         LA    R2,4                                                             
         BAS   RE,FORMAT                                                        
         MVI   SPACING,2                                                        
         CLI   QOPT3,C'O'                                                       
         BE    *+8                                                              
         BAS   RE,CIAREPRT                                                      
         CLI   QOPT3,C'Y'          CLIENT SUMMARY OPTION                        
         BE    CIA7Y                                                            
         CLI   QOPT3,C'O'          CLIENT SUMMARY ONLY                          
         BNE   *+8                                                              
CIA7Y    BAS   RE,CLISUM                                                        
         MVI   FORCEHED,C'Y'                                                    
         B     CIA7A                                                            
         SPACE 2                                                                
CIA7X    CP    CLICONT,=P'1'       E.O.F.                                       
         BE    CIAXIT                                                           
         CLI   LASTSPAC,2                                                       
         BNL   *+8                                                              
         BAS   RE,CIAREPRT                                                      
         CLI   QOPT1,C'Y'                                                       
         BNE   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         MVC   PRODNAME(13),=C'REQUEST TOTAL'                                   
         LA    R2,5                                                             
         BAS   RE,FORMAT                                                        
         BAS   RE,CIAREPRT                                                      
         B     CIAXIT                                                           
         SPACE 2                                                                
CIA7Z    GOTO1 MYROLLER,DMCB,2,ACCUMS,(R2)                                      
         B     CIA7A                                                            
         SPACE 1                                                                
CIAXIT   XMOD1 1                                                                
XIT      XIT1                                                                   
         GETEL R4,DATADISP,BYTE                                                 
         EJECT                                                                  
*              RUNLAST                                                          
         SPACE 2                                                                
CIA8     CLI   MODE,RUNLAST                                                     
         BNE   CIAXIT                                                           
         BAS   RE,ENDSORT                                                       
         B     CIAXIT                                                           
         EJECT                                                                  
*              ROUTINE PROPAGATES A NEW KEY DOWN ARRAY OF SORT RECORDS          
         SPACE 2                                                                
REKEY    NTR1                                                                   
         USING SORTRECD,R6                                                      
         LA    RF,SORTREC3+L'SORTKEY                                            
         LA    RE,L'SORTREC        PREPARE TO BXLE                              
         SPACE 1                                                                
         LA    R1,L'SCLIKEY-1                                                   
         LA    R2,SCLIKEY-SORTRECD                                              
         CLI   SORTTYPE,C'C'       CLIENT                                       
         BE    REK10                                                            
         LA    R1,L'SPRODKEY-1                                                  
         LA    R2,SPRODKEY-SORTRECD                                             
         CLI   SORTTYPE,C'P'       PRODUCT                                      
         BE    REK10                                                            
         LA    R1,L'SINCKEY-1                                                   
         LA    R2,SINCKEY-SORTRECD DEFAULT TO INCOME ACCOUNT                    
         SPACE 1                                                                
REK10    LA    R2,0(R2,R6)                                                      
         SPACE 1                                                                
REK20    LA    R3,L'SORTREC(R2)                                                 
         EXMVC R1,0(R3),0(R2)                                                   
         BXLE  R2,RE,REK20                                                      
         B     XIT                                                              
         EJECT                                                                  
*              PRODUCE OPTIONAL CLIENT SUMMARY                                  
         SPACE 2                                                                
CLISUM   NTR1                                                                   
         MVC   P,SPACES                                                         
         MVC   PSECOND,SPACES                                                   
         BAS   RE,CIAREPRT                                                      
         MVC   MID1+1(14),=C'CLIENT SUMMARY'                                    
         MVC   MID2+1(14),=C'--------------'                                    
         MVI   INCSW,C'Y'                                                       
         MVI   FORCEMID,C'Y'                                                    
         LA    R4,P                                                             
         USING PLINED,R4                                                        
         XC    BUFFKEY,BUFFKEY                                                  
         GOTO1 BUFFALO,DMCB,=C'HIGH',ABUFF,BUFFREC,1                            
CLSM2    TM    DMCB+8,X'80'                                                     
         BO    CLSMX                                                            
         CLI   BUFFKAC,X'FF'       MONTHS FOR ALL INCOME ACCTS                  
         BE    CLSM8                                                            
         CLI   BUFFKAC,X'FE'       CLIENT TOTAL                                 
         BNE   CLSM4                                                            
         BAS   RE,CIAREPRT                                                      
         MVC   PINVNUM+3(12),=C'CLIENT TOTAL'                                   
         B     CLSM10                                                           
CLSM4    CLI   BUFFKMON,X'FE'      INCOME ACCT TOTAL                            
         BNE   CLSM6                                                            
         MVC   PINVDATE(5),=C'TOTAL'                                            
         MVI   INCSW,C'Y'                                                       
         B     CLSM10                                                           
CLSM6    CLI   INCSW,C'Y'          DO WE WANT TO PRINT INCOME ACCT              
         BNE   CLSM8                                                            
         MVI   INCSW,C'N'                                                       
         MVC   WORK,SPACES                                                      
         MVC   WORK(L'BUFFKAC),BUFFKAC                                          
         MVC   WORK+L'BUFFKAC+1(L'BUFFNAME),BUFFNAME                            
         LA    R2,L'BUFFKAC+L'BUFFNAME+1                                        
         GOTO1 =V(SQUASHER),DMCB,WORK,(R2),RR=RB                                
         L     R2,DMCB+4                                                        
         BCTR  R2,0                                                             
         EXMVC R2,PRODNAME,WORK                                                 
CLSM8    MVC   WORK(2),BUFFKMON                                                 
         MVI   WORK+2,X'01'                                                     
         GOTO1 DATCON,DMCB,(1,WORK),(9,PINVDATE)                                
CLSM10   LA    RF,BUFFGRSS                                                      
         ST    RF,FULL                                                          
         BAS   RE,FORMCTRS                                                      
         BAS   RE,CIAREPRT                                                      
         CLI   BUFFKAC,X'FE'       DID WE JUST DO CLIENT TOTAL                  
         BNE   CLSM12                                                           
         BAS   RE,CIAREPRT                                                      
         MVC   P+1(12),=C'ALL ACCOUNTS'                                         
CLSM12   GOTO1 BUFFALO,DMCB,=C'SEQ',ABUFF,BUFFREC,1                             
         B     CLSM2                                                            
         SPACE 1                                                                
CLSMX    GOTO1 BUFFALO,DMCB,=C'RESET',ABUFF                                     
         B     CIAXIT                                                           
         EJECT                                                                  
*              GETNAME FINDS CLIENT IN SJ LEDGER                                
         SPACE 2                                                                
GETNAME  NTR1                                                                   
         USING SORTRECD,R6                                                      
         L     R4,=A(ACCBUFF)                                                   
         A     R4,RELO                                                          
         MVC   0(42,R4),SPACES                                                  
         SPACE 1                                                                
         MVC   0(1,R4),QCOMPANY                                                 
*&&UK                                                                           
         MVC   1(2,R4),=C'SJ'                                                   
         MVC   3(L'SCLIKEY,R4),SCLIKEY                                          
*&&                                                                             
*&&US                                                                           
         MVC   1(2,R4),SORTUL                                                   
         MVC   3(3,R4),SCLIKEY                                                  
*&&                                                                             
         SPACE 1                                                                
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCOUNT',(R4),(R4)                    
         SPACE 1                                                                
         MVI   BYTE,X'20'                                                       
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
         MVC   SORTDATA,SPACES                                                  
         USING ACNAMED,R4                                                       
         ZIC   R1,ACNMLEN                                                       
         SH    R1,=H'03'                                                        
         LTR   R1,R1                                                            
         BM    XIT                                                              
         EXMVC R1,SORTDATA,ACNMNAME                                             
         B     XIT                                                              
         EJECT                                                                  
*              FORMAT MASSAGES & EDITS FIGURES ONTO PRINT LINE                  
         SPACE 2                                                                
FORMAT   NTR1                                                                   
         USING PLINED,R4                                                        
         GOTO1 MYROLLER,DMCB,1,ACCUMS,(R2)                                      
         MVC   FULL,DMCB                                                        
         BAS   RE,FORMCTRS                                                      
         B     CIAXIT                                                           
         SPACE 2                                                                
FORMCTRS NTR1                                                                   
         L     R5,FULL                                                          
         LA    R6,PGROSAMT                                                      
         CP    0(8,R5),=P'0'                                                    
         BE    *+8                                                              
         BAS   RE,EDIT1                                                         
         SPACE 1                                                                
         CLI   QOPT2,C'Y'              SUPPRESS COMM, NET , PCT                 
         BE    FORM10                                                           
         LA    R5,8(R5)                                                         
         LA    R6,PCOMMAMT                                                      
         CP    0(8,R5),=P'0'                                                    
         BE    *+8                                                              
         BAS   RE,EDIT1                                                         
         SPACE 1                                                                
         L     R5,FULL                                                          
         LA    R6,PNETAMT                                                       
         ZAP   DOUBLE,0(8,R5)                                                   
         SP    DOUBLE,8(8,R5)                                                   
         LA    R5,DOUBLE                                                        
         CP    0(8,R5),=P'0'                                                    
         BE    *+8                                                              
         BAS   RE,EDIT1                                                         
         SPACE 1                                                                
FORM2    L     R5,FULL                                                          
         CP    0(8,R5),=P'0'                                                    
         BE    FORM10                                                           
         CP    8(8,R5),=P'0'                                                    
         BE    FORM10                                                           
         ZAP   WORK(16),8(8,R5)                COMMISSION                       
         MP    WORK(16),=P'100000'                                              
         DP    WORK(16),0(8,R5)                DIVIDED BY GROSS                 
         AP    WORK(8),=P'5'                   ROUND                            
         CP    WORK(8),=P'0'                                                    
         BH    *+10                                                             
         SP    WORK(8),=P'10'                                                   
         LA    R5,WORK                                                          
         LA    R6,PERCENT                                                       
         CP    0(8,R5),=P'0'                                                    
         BE    FORM10                                                           
         BAS   RE,EDIT2                                                         
         SPACE 1                                                                
FORM10   B     XIT                                                              
         SPACE 1                                                                
EDIT1    EDIT  (P8,(R5)),(15,(R6)),2,COMMAS=YES,MINUS=YES                       
         BR    RE                                                               
         SPACE 1                                                                
EDIT2    EDIT  (P8,(R5)),(8,(R6)),3,MINUS=YES                                   
         MVC   7(1,R6),8(R6)        LOSE THIRD DECIMAL PLACE                    
         BR    RE                                                               
         EJECT                                                                  
*              SORTER INTERFACE                                                 
         SPACE 2                                                                
         USING SORTRECD,R6                                                      
SETSORT  NTR1                                                                   
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECDCARD,RR=RB                          
         B     XIT                                                              
         SPACE 2                                                                
PUTSORT  NTR1                                                                   
         GOTO1 =V(SORTER),DMCB,=C'PUT',SORTREC,RR=RB                            
         B     XIT                                                              
         SPACE 2                                                                
GETSORT  NTR1                                                                   
         MVC   SORTREC,SPACES                                                   
         GOTO1 =V(SORTER),DMCB,=C'GET',0,RR=RB                                  
         L     R1,DMCB+4                                                        
         LTR   R1,R1                                                            
         BZ    XIT                                                              
         MVC   SORTREC,0(R1)                                                    
         B     XIT                                                              
         SPACE 2                                                                
ENDSORT  NTR1                                                                   
         GOTO1 =V(SORTER),DMCB,=C'END',RR=RB                                    
         B     XIT                                                              
         SPACE 1                                                                
SORTCARD DC    CL80'SORT FIELDS=(01,30,A),FORMAT=BI,WORK=1'                     
RECDCARD DC    CL80'RECORD TYPE=F,LENGTH=67'                                    
         EJECT                                                                  
*              MYROLLER BECAUSE PROLLER CAN'T COPE WITH PL8                     
*              THEY LOOK EXACTLY THE SAME                                       
         SPACE 2                                                                
MYROLLER NTR1                                                                   
         L     R1,DMCB+4           PT TO TABLE ASSUME DMCB USED                 
         CLC   DMCB(4),=F'0'                                                    
         BE    MYR1                                                             
         CLC   DMCB(4),=F'1'                                                    
         BE    MYR2                                                             
         CLC   DMCB(4),=F'3'                                                    
         BE    MYR3                                                             
         CLC   DMCB(4),=F'6'                                                    
         BE    MYR4                                                             
         CLC   DMCB(4),=F'2'                                                    
         BE    MYR5                                                             
         B     XIT                                                              
         SPACE 2                                                                
MYR1     EQU   *                   SET TABLE                                    
         MVC   0(8,R1),DMCB+8      SET ROWS AND COLUMNS                         
         LA    R2,8(R1)                                                         
         L     R3,DMCB+8                                                        
         MH    R3,DMCB+14          HALF WORD LIMIT ON COLUMNS                   
         SPACE 1                                                                
MYR1A    ZAP   0(8,R2),=P'0'                                                    
         LA    R2,8(R2)                                                         
         BCT   R3,MYR1A                                                         
         B     XIT                                                              
         SPACE 2                                                                
MYR2     EQU   *                   FIND A ROW                                   
         L     R3,DMCB+8                                                        
         BAS   RE,MYRA                                                          
         ST    R2,DMCB                                                          
         B     XIT                                                              
         SPACE 2                                                                
MYR3     EQU   *                   ADD A NO INTO A TABLE ENTRY                  
         L     R3,DMCB+12                                                       
         BAS   RE,MYRA                                                          
         L     R3,DMCB+16                                                       
         C     R3,4(R1)                                                         
         BNH   *+6                                                              
         DC    H'0'                NOT POSSIBLE FAULTY CALL                     
MYR3A    BCT   R3,*+8                                                           
         B     MYR3B                                                            
         LA    R2,8(R2)                                                         
         B     MYR3A                                                            
         SPACE 1                                                                
MYR3B    L     R4,DMCB+8                                                        
         AP    0(8,R2),0(8,R4)                                                  
         B     XIT                                                              
         SPACE 2                                                                
MYR4     EQU   *                   CROSS CAST AND ADD DOWN                      
         L     R3,DMCB+8                                                        
         BAS   RE,MYRA                                                          
         L     R4,4(R1)            NO OF COLUMNS                                
         ZAP   DUB,=P'0'                                                        
         LR    R5,R2               SAVE R2                                      
         SPACE 1                                                                
MYR4A    CH    R4,=H'1'                                                         
         BNE   MYR4B                                                            
         AP    0(8,R2),DUB                                                      
         B     MYR4C                                                            
MYR4B    AP    DUB,0(8,R2)                                                      
         LA    R2,8(R2)                                                         
         BCT   R4,MYR4A                                                         
         SPACE 1                                                                
MYR4C    L     R3,0(R1)            FIND LAST LINE                               
         BAS   RE,MYRA                                                          
         LR    R7,R2                                                            
         L     R6,4(R1)                                                         
         MH    R6,=H'8'            GIVES LENGTH OF ONE LINE                     
         LA    R6,0(R6,R5)         R5 POINTS TO START LINE                      
         SPACE 1                                                                
MYR4D    CR    R7,R6               R7 POINTS TO END LINE                        
         BL    XIT                                                              
         LR    R3,R5                                                            
         L     R8,4(R1)                                                         
MYR4E    AP    0(8,R6),0(8,R3)                                                  
         LA    R3,8(R3)                                                         
         LA    R6,8(R6)                                                         
         BCT   R8,MYR4E                                                         
         B     MYR4D                                                            
         SPACE 2                                                                
MYR5     EQU   *                   CLEAR A LINE                                 
         L     R3,DMCB+8                                                        
         BAS   RE,MYRA                                                          
         L     R3,4(R1)                                                         
MYR5A    ZAP   0(8,R2),=P'0'                                                    
         LA    R2,8(R2)                                                         
         BCT   R3,MYR5A                                                         
         B     XIT                                                              
         SPACE 2                                                                
MYRA     EQU   *                   SET R2 TO POINT TO R3TH LINE                 
         LA    R2,8(R1)            POINT TO START OF NOS                        
         C     R3,0(R1)                                                         
         BNH   *+6                                                              
         DC    H'0'                CAN'T BE DONE                                
         L     R4,4(R1)                                                         
         MH    R4,=H'8'            GIVES LENGTH OF ONE WHOLE LINE               
MYRAA    BCT   R3,*+6                                                           
         BR    RE                                                               
         LA    R2,0(R4,R2)                                                      
         B     MYRAA                                                            
         EJECT                                                                  
*              CIAREPRT,PRINT ROUTINE                                           
         SPACE 2                                                                
CIAREPRT NTR1                                                                   
         ZIC   R1,LINE                                                          
         LA    R1,1(R1)                                                         
         ZIC   R0,MAXLINES                                                      
         CR    R1,R0                                                            
         BL    CREP10                                                           
         MVI   FORCEHED,C'Y'                                                    
         CLC   P,SPACES                                                         
         BNE   CREP10                                                           
         MVI   LASTSPAC,2          NO NEED ACTUALLY TO PRINT                    
         B     XIT                                                              
         SPACE 1                                                                
CREP10   MVC   LASTSPAC,SPACING                                                 
         MVC   HEAD4,MYHEAD4                                                    
         MVC   HEAD8,SPACES                                                     
         MVC   HEAD9,SPACES                                                     
         CLI   QOPT2,C'Y'                                                       
         BE    CREP12                                                           
         MVC   HEAD8+67(10),=C'COMMISSION'                                      
         MVC   HEAD9+67(10),=10C'-'                                             
         MVC   HEAD8+83(3),=C'PCT'                                              
         MVC   HEAD9+83(3),=10C'-'                                              
         MVC   HEAD8+98(3),=C'NET'                                              
         MVC   HEAD9+98(3),=10C'-'                                              
CREP12   DS    0H                                                               
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         EJECT                                                                  
*              LTORG                                                            
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              SORTRECD, DSECT COVERS SORT RECORDS                              
         SPACE                                                                  
SORTRECD DSECT                                                                  
SORTREC  DS    0CL67                                                            
SORTKEY  DS    0CL30                                                            
SCLIKEY  DS    CL6                                                              
         ORG   SCLIKEY+3                                                        
SORTUL   DS    CL3                                                              
SPRODKEY DS    CL6                                                              
SINCKEY  DS    CL12                                                             
SINVNUM  DS    CL6                                                              
         SPACE 1                                                                
SORTTYPE DS    CL1                                                              
         SPACE 1                                                                
SORTDATA DS    0CL36                                                            
SINVDATE DS    CL3                                                              
SINVGROS DS    PL6                                                              
SINVCOMM DS    PL6                                                              
SINVMOS  DS    XL2                                                              
         EJECT                                                                  
*              PLINED, DSECT COVERS PRINT LINE                                  
         SPACE 2                                                                
PLINED DSECT                                                                    
PLINE    DS    0CL110                                                           
         DS    CL1                                                              
PRODNAME DS    CL46                                                             
         ORG   PRODNAME+3                                                       
PINCNAME DS    CL23                                                             
         DS    CL1                                                              
PINVNUM  DS    CL6                                                              
         DS    CL3                                                              
PINVDATE DS    CL9                                                              
         DS    CL1                                                              
PGROSAMT DS    CL15                                                             
         DS    CL1                                                              
PCOMMAMT DS    CL15                                                             
         DS    CL1                                                              
PERCENT  DS    CL7                                                              
         DS    CL1                                                              
PNETAMT  DS    CL15                                                             
         EJECT                                                                  
*              AC3402D,DSECT FOR LOCAL WORKING STORAGE                          
         SPACE 2                                                                
AC3402D  DSECT                                                                  
         DS    0D                                                               
RELO     DS    F                                                                
ABUFF    DS    A                                                                
ACCUMS   DS    CL8,(3*5)PL8                                                     
TRNSCONT DS    PL4                                                              
INCCONT  DS    PL4                                                              
PRODCONT DS    PL4                                                              
CLICONT  DS    PL4                                                              
         SPACE 1                                                                
SAVEKEY  DS    CL42                                                             
CHOPBLOC DS    2CL(L'PINCNAME)                                                  
SORTREC1 DS    CL(L'SORTREC)                                                    
SORTREC2 DS    CL(L'SORTREC)                                                    
SORTREC3 DS    CL(L'SORTREC)                                                    
SORTREC4 DS    CL(L'SORTREC)                                                    
LASTSORT DS    CL(L'SORTREC)                                                    
         SPACE 1                                                                
LASTSPAC DS    CL1                                                              
READTRNS DS    CL1                                                              
CLIOK    DS    CL1                                                              
PRODOK   DS    CL1                                                              
INCOK    DS    CL1                                                              
         SPACE 1                                                                
MYHEAD4  DS    CL132                                                            
CLILEN   DS    CL1                                                              
PRODLEN  DS    CL1                                                              
BTCHM    DS    CL2                                                              
DATNO    DS    CL1                                                              
INCSW    DS    CL1                                                              
START3   DS    XL3                                                              
END3     DS    XL3                                                              
BUFFREC  DS    0CL62                                                            
BUFFKEY  DS    0CL14                                                            
BUFFKAC  DS    CL12                                                             
BUFFKMON DS    XL2                                                              
BUFFNAME DS    CL36                                                             
BUFFGRSS DS    PL8                                                              
BUFFCOMM DS    PL8                                                              
         SPACE 2                                                                
ACCBUFF  CSECT                                                                  
         DS    CL1024              1K FOR DATAMGR READ                          
         SPACE 2                                                                
*              STANDARD DSECTS                                                  
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
         SPACE 1                                                                
         BUFF  LINES=100,ROWS=1,COLUMNS=2,COMMENT=36,FLAVOR=PACKED,    X        
               KEYLIST=(14,A)                                                   
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'031ACREP3402 06/03/15'                                      
         END                                                                    
