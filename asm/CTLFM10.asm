*          DATA SET CTLFM10    AT LEVEL 020 AS OF 05/20/02                      
*PHASE TA0210A                                                                  
         TITLE 'CTLFM10 - CONTROL FILE MAINT - EXTRACT RULES RECORDS'           
CTLFM10  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WRKX-WRKD,**LFMG**                                               
         USING WRKD,RC             RC=A(TEMP W/S)                               
         LR    R9,R1                                                            
         USING LFMTEMP,R9          R9=A(GLOBAL W/S)                             
         L     R3,ATWA                                                          
         USING LFMSAVE,R3          R3=A(SAVE W/S)                               
         LR    R2,R3                                                            
         USING CTLFMFFD,R2         R2=A(TWA)                                    
         LA    R4,IOAREA                                                        
         USING CTXREC,R4           R4=A(RECORD)                                 
*&&US*&& MVI   COUNTRY,US          SET COUNTRY CODE                             
*&&UK*&& MVI   COUNTRY,UK                                                       
         EJECT                                                                  
*              READ ID RECORD AND EXTRACT AGENCY ID FOR SPOT SYSTEM             
*                                                                               
KEYVAL   LA    R5,IOAREA                                                        
         USING CTIREC,R5                                                        
         XC    CTIKEY,CTIKEY       BUILD A KEY                                  
         MVI   CTIKEY,C'I'                                                      
         LR    RE,R2                                                            
         USING TWAD,RE                                                          
         MVC   CTIKEY+L'CTIKEY-2(2),TWAUSRID                                    
         MVC   KEY,CTIKEY                                                       
         DROP  RE                                                               
         GOTO1 AREAD               READ ID RECORD                               
         CLI   DMCB+8,0                                                         
         BNE   EIIO                                                             
         LA    R5,CTIDATA                                                       
         SR    R6,R6                                                            
*                                  NOW FIND SYSTEM ELEMENT ON ID REC            
KEYV2    CLI   0(R5),0                                                          
         BE    EANA                NOT AUTHORIZED FOR SYSTEM                    
         CLI   0(R5),X'06'                                                      
         BNE   KEYV4                                                            
         USING CTAGYD,R5                                                        
         MVC   AGYALPH,CTAGYID                                                  
         B     KEYV6                                                            
*                                                                               
KEYV4    IC    R6,1(R5)            BUMP TO NEXT ELEMENT                         
         AR    R5,R6                                                            
         B     KEYV2                                                            
         DROP  R5                                                               
*                                  VALIDATE KEY FIELDS                          
KEYV6    XC    CTXKEY,CTXKEY                                                    
         MVI   CTXKEY,C'X'         BUILD VIRGIN KEY                             
         MVC   CTXKAGY,AGYALPH                                                  
         MVI   FORMAT,0                                                         
*                                  VALIDATE CLIENT                              
         LA    R1,EXTCLIH                                                       
         CLI   5(R1),0             SET TO 'ALL' IF N/I                          
         BNE   *+18                                                             
         MVC   8(3,R1),=C'ALL'                                                  
         MVI   5(R1),3                                                          
         OI    6(R1),X'80'                                                      
         GOTO1 AFVAL                                                            
         CLI   COUNTRY,UK                                                       
         BE    *+12                                                             
         CLI   FLDH+5,3                                                         
         BH    EFTL                                                             
         CLI   FLDH+5,3                                                         
         BNE   KEYV8                                                            
         CLC   FLD(3),=C'ALL'                                                   
         BNE   KEYV8                                                            
         OI    FORMAT,X'80'        SET CLIENT=ALL                               
         B     KEYVA                                                            
*                                                                               
KEYV8    MVC   CTXKCLT,FLD                                                      
         CLI   FLDH+5,3                                                         
         BNH   KEYVA                                                            
         CLI   FLDH+5,4                                                         
         BE    EIIF                                                             
         MVC   WORK(4),=C'0000'    UK=XNNNN                                     
         MVZ   WORK(4),FLD+1                                                    
         CLC   WORK(4),=C'0000'                                                 
         BNE   EFNN                                                             
         PACK  DUB,FLD+1(4)                                                     
         CVB   R1,DUB                                                           
         STH   R1,DUB                                                           
         MVC   CTXKCLT+1(2),DUB                                                 
*                                  VALIDATE PRODUCT                             
KEYVA    LA    R1,EXTPROH                                                       
         CLI   5(R1),0             SET TO 'ALL' IF N/I                          
         BNE   *+18                                                             
         MVC   8(3,R1),=C'ALL'                                                  
         MVI   5(R1),3                                                          
         OI    6(R1),X'80'                                                      
         GOTO1 AFVAL                                                            
         CLC   FLD(3),=C'ALL'      NO - INPUT S/B 'ALL'                         
         BNE   KEYVC                                                            
         OI    FORMAT,X'40'                                                     
         B     KEYVE                                                            
*                                                                               
KEYVC    MVC   CTXKPRD,FLD                                                      
         TM    FORMAT,X'80'        WAS CLIENT 'ALL'                             
         BO    EIIF                                                             
*                                  VALIDATE ESTIMATE                            
KEYVE    LA    R1,EXTESTH                                                       
         CLI   5(R1),0             SET TO 'ALL' IF N/I                          
         BNE   *+18                                                             
         MVC   8(3,R1),=C'ALL'                                                  
         MVI   5(R1),3                                                          
         OI    6(R1),X'80'                                                      
         GOTO1 AFVAL                                                            
         TM    FLDH+4,X'08'        TEST NUMERIC                                 
         BO    KEYVG                                                            
         CLC   FLD(3),=C'ALL'      NO - INPUT S/B 'ALL'                         
         BNE   EFNN                                                             
         OI    FORMAT,X'20'                                                     
         B     KEYVI                                                            
*                                                                               
KEYVG    CLC   FLDH(4),=F'255'                                                  
         BH    EIIF                                                             
         OC    FLDH(4),FLDH                                                     
         BZ    EIIF                                                             
*&&UK                                                                           
         TM    FORMAT,X'40'                                                     
         BO    EIIF                                                             
*&&                                                                             
*&&US                                                                           
         TM    FORMAT,X'80'        CLI=ALL N/V FOR 1 ESTIMATE                   
         BO    EIIF                                                             
*&&                                                                             
         MVC   CTXKEST,FLDH+3                                                   
*                                  VALIDATE START DATE                          
KEYVI    LA    R1,EXTSDTH                                                       
         GOTO1 AFVAL                                                            
         BZ    KEYVK                                                            
         GOTO1 VDATVAL,DMCB,(0,FLD),WORK                                        
         OC    DMCB(4),DMCB                                                     
         BZ    EIDF                                                             
         GOTO1 VDATCON,DMCB,(0,WORK),(1,CTXKSTRT)                               
*                                  VALIDATE END DATE                            
KEYVK    LA    R1,EXTEDTH                                                       
         GOTO1 AFVAL                                                            
         BZ    KEYVM                                                            
         GOTO1 VDATVAL,DMCB,(0,FLD),WORK                                        
         OC    DMCB(4),DMCB                                                     
         BZ    EIDF                                                             
         GOTO1 VDATCON,DMCB,(0,WORK),(1,CTXKEND)                                
*                                  CHECK DATES ARE COMPATIBLE                   
KEYVM    LA    R1,EXTSDTH                                                       
         ST    R1,FADR                                                          
         OC    CTXKEND,CTXKEND                                                  
         BZ    *+14                                                             
         CLC   CTXKSTRT,CTXKEND                                                 
         BH    EDNC                                                             
         LA    R1,EXTCLIH                                                       
         ST    R1,FADR                                                          
         MVC   KEY,CTXKEY                                                       
         MVC   KEYNEXT,KEY                                                      
         CLI   ACTN,CHANGE         SET ACTN TO DISPLAY IF ACTN IS               
         BNE   *+18                CHANGE AND RECORD IS NOT ALREADY             
         CLC   KEY,LKEY            DISPLAYED                                    
         BE    *+8                                                              
         MVI   ACTN,DISPLAY                                                     
         CLI   ACTN,DISPLAY                                                     
         BE    *+8                                                              
         MVI   UPDATE,C'Y'         SET RFU INDIC                                
         GOTO1 AREAD                                                            
         BZ    EIIO                                                             
         CLI   ACTN,COPY           COPY IS TREATED AS AN ADD                    
         BNE   *+8                                                              
         MVI   ACTN,ADD                                                         
         TM    DMCB+8,X'10'        N/F ONLY VALID FOR ADD                       
         BZ    *+16                                                             
         CLI   ACTN,ADD                                                         
         BE    DATAVAL                                                          
         B     ERNF                                                             
         CLI   ACTN,ADD            RECORD CAN'T EXIST FOR ADD                   
         BE    ERAE                                                             
         TM    DMCB+8,X'02'                                                     
         BZ    *+12                                                             
         CLI   ACTN,DISPLAY        CAN ONLY DISPLAY DELETED RECORDS             
         BNE   ERID                                                             
         CLI   ACTN,CHANGE                                                      
         BE    DATAVAL                                                          
         B     DISPREC                                                          
         EJECT                                                                  
*              DISPLAY EXTRACT RULES RECORD                                     
*                                                                               
DISPREC  LA    R1,EXTDE1H          CLEAR DOWN TWA                               
         GOTO1 ACLEAR                                                           
         LA    R5,CTXDATA                                                       
         MVI   FORMAT,0            NUMBER OF X'76' ELEMENTS ON REC              
*                                                                               
DISPR2   CLI   0(R5),0             END-OF-RECORD                                
         BE    DISPRE                                                           
         CLI   0(R5),X'74'         DEMO EXTRACT ELEMENT                         
         BE    DISPR6                                                           
         CLI   0(R5),X'76'         PROGTYPE EQUIV ELEMENT                       
         BE    DISPRC                                                           
*                                                                               
DISPR4   SR    R6,R6               BUMP TO NEXT ELEMENT                         
         IC    R6,1(R5)                                                         
         AR    R5,R6                                                            
         B     DISPR2                                                           
*                                  DISPLAY DEMO EXTRACT ELEMENT                 
DISPR6   DS    0H                                                               
         USING CTDXD,R5                                                         
         LA    RE,UNSCNBL1                                                      
         SR    R6,R6                                                            
         IC    R6,1(R5)                                                         
         SH    R6,=H'2'            R6=NUMBER OF ENTRIES IN CTDXLIST             
         LA    R7,CTDXLIST                                                      
         SR    RF,RF               RF=NUMBER OF ENTRIES IN UNSCAN BLK           
*                                  EDIT-OUT AN ENTRY                            
DISPR8   MVI   0(RE),C' '                                                       
         MVC   1(L'UNSCNBL1-1,RE),0(RE)                                         
         CLI   0(R7),0                                                          
         BNE   *+14                                                             
         MVC   0(4,RE),=C'NONE'                                                 
         B     DISPRA                                                           
         CLI   0(R7),X'F1'                                                      
         BNE   *+14                                                             
         MVC   0(5,RE),=C'FIRST'                                                
         B     DISPRA                                                           
         CLI   0(R7),X'F2'                                                      
         BNE   *+14                                                             
         MVC   0(6,RE),=C'SECOND'                                               
         B     DISPRA                                                           
         CLI   0(R7),X'F3'                                                      
         BNE   *+14                                                             
         MVC   0(5,RE),=C'THIRD'                                                
         B     DISPRA                                                           
         CLI   0(R7),X'F4'                                                      
         BNE   *+14                                                             
         MVC   0(6,RE),=C'FOURTH'                                               
         B     DISPRA                                                           
         SR    R1,R1                                                            
         IC    R1,0(R7)                                                         
         EDIT  (R1),(3,0(RE)),ALIGN=LEFT                                        
*                                                                               
DISPRA   LA    RE,L'UNSCNBL1(RE)   BUMP TO NEXT                                 
         LA    R7,1(R7)                                                         
         LA    RF,1(RF)                                                         
         BCT   R6,DISPR8                                                        
         LA    R1,UNSCNBL1         DONE-UNSCAN INTO TWA                         
         ST    R1,DUB                                                           
         STC   RF,DUB                                                           
         LA    R1,DEMFLDS                                                       
         ST    R1,DUB+4                                                         
         BAS   RE,UNSCAN                                                        
         B     DISPR4                                                           
         DROP  R5                                                               
*                                  ADD PRGTYPE EQUIV ENTRY TO BLOCK             
DISPRC   DS    0H                                                               
         USING CTPQD,R5                                                         
         SR    RE,RE                                                            
         IC    RE,FORMAT                                                        
         LA    RF,1(RE)                                                         
         STC   RF,FORMAT                                                        
         MH    RE,=H'20'           RE=A(BLOCK ENTRY)                            
         LA    RE,UNSCNBL2(RE)                                                  
         MVI   0(RE),C' '                                                       
         MVC   1(L'UNSCNBL2-1,RE),0(RE)                                         
         MVC   0(3,RE),CTPQDP                                                   
         CLI   0(RE),0                                                          
         BNE   *+10                                                             
         MVC   0(3,RE),1(RE)                                                    
         MVC   10(1,RE),CTPQCODE                                                
         B     DISPR4                                                           
         DROP  R5                                                               
*                                  UNSCAN PRGTYPE EQUIV INTO TWA                
DISPRE   CLI   FORMAT,0            ANY FOUND ?                                  
         BE    DISPEND                                                          
         LA    R1,UNSCNBL2                                                      
         ST    R1,DUB                                                           
         MVC   DUB(1),FORMAT                                                    
         LA    R1,PROFLDS                                                       
         ST    R1,DUB+4                                                         
         BAS   RE,UNSCAN                                                        
*                                  SET NEXT ACTION & EXIT                       
DISPEND  MVI   FERN,X'FF'                                                       
         LA    R1,EXTDE1H                                                       
         ST    R1,FADR                                                          
         TM    CTXSTAT,X'80'                                                    
         BZ    *+12                                                             
         MVI   NACTN,OKRES                                                      
         B     EXIT                                                             
         MVI   NACTN,OKDEL+OKCHA+OKCOPY                                         
         B     EXIT                                                             
         EJECT                                                                  
*              ADD/COPY/CHANGE EXTRACT RULES RECORD                             
*                                                                               
DATAVAL  MVC   ACTN,ACTN2          RESTORE ACTION (IF COPY)                     
         CLI   ACTN,COPY                                                        
         BNE   DATAV2                                                           
         MVC   KEYSAVE,KEY         COPY RECORD                                  
         MVC   KEY,LKEY                                                         
         GOTO1 AREAD                                                            
         CLI   DMCB+8,0                                                         
         BNE   EIIO                                                             
         MVI   TEMP,X'01'          HAS NEW ACTIVITY ELEMENT                     
         GOTO1 ADELEL                                                           
         GOTO1 ABLDACT                                                          
         GOTO1 APUTEL                                                           
         MVC   KEY,KEYSAVE                                                      
         MVC   CTXKEY,KEY                                                       
         B     DATAVM              GO AND ADD RECORD                            
*                                                                               
DATAV2   MVI   TEMP,0              BUILD BASIC RECORD (KEY/LEN/ACT)             
         GOTO1 ABLDREC                                                          
         GOTO1 ABLDACT                                                          
         GOTO1 APUTEL                                                           
*                                  VALIDATE DEMO EXTRACT LIST                   
         LA    R1,DEMFLDS          SCAN DEMO EXTRACT LIST FIELDS                
         BAS   RE,SCAN                                                          
         BE    *+16                                                             
         L     R1,DMCB             SET FADR TO FIELD IN ERROR                   
         ST    R1,FADR                                                          
         B     EIIF                                                             
*                                                                               
         CLI   NLINES,0            ANY INPUT                                    
         BNE   *+16                                                             
         LA    R1,EXTDE1H          NO - MISSING INPUT ON FIRST LINE             
         ST    R1,FADR                                                          
         B     EMIF                                                             
*                                  PROCESS SCAN BLOCK                           
         LA    R5,FLINES                                                        
         ST    R5,ALINE                                                         
         LA    R5,SCANBLK          R5=A(SCAN BLOCK ENTRY)                       
         SR    R6,R6                                                            
         IC    R6,NLINES           R6=TOTAL LINES INPUT                         
         XC    LOOKLIST,LOOKLIST                                                
         XC    TEMP,TEMP                                                        
         MVC   TEMP(2),=X'7402'    BUILD VIRGIN ELEMENT                         
         LA    R7,TEMP                                                          
         USING CTDXD,R7                                                         
         MVI   FNDX,1                                                           
         LA    R8,CTDXLIST                                                      
*                                                                               
DATAV4   L     R1,ALINE            SET FADR/FNDX FOR THIS ENTRY                 
         CLI   0(R1),X'FF'                                                      
         BE    DATAVC                                                           
         CLC   FNDX,0(R1)                                                       
         BNH   *+16                                                             
         LA    R1,4(R1)                                                         
         MVI   FNDX,1                                                           
         B     DATAV4+4                                                         
         ST    R1,ALINE                                                         
*                                                                               
         MVC   FADR,0(R1)          SET A(INPUT FIELD)                           
         CLI   1(R5),0             L'SECOND                                     
         BNE   EIIF                                                             
         CLI   0(R5),1             L'FIRST                                      
         BL    EFTS                                                             
         CLI   0(R5),6             L'FIRST                                      
         BH    EFTL                                                             
         TM    2(R5),X'80'         T'FIRST (NUMERIC)                            
         BO    DATAV8                                                           
         SR    R1,R1                                                            
         IC    R1,0(R5)            R1=L'FIRST                                   
         BCTR  R1,0                                                             
         LA    RE,INPLIST                                                       
*                                  SEARCH TABLE FOR KEYWORD MATCH               
DATAV6   CLI   0(RE),X'FF'                                                      
         BE    EIIF                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R5),0(RE)                                                   
         BE    *+12                                                             
         LA    RE,L'INPLIST(RE)                                                 
         B     DATAV6                                                           
         MVC   WORK(1),7(RE)       EXTRACT VALUE IF FOUND                       
         B     DATAVA                                                           
*                                                                               
DATAV8   OC    4(4,R5),4(R5)       CAN'T BE ZERO                                
         BZ    EFLM                                                             
         MVC   DUB(2),=H'230'      SET MAX DEMO FOR COUNTRY                     
         CLI   COUNTRY,US                                                       
         BE    *+10                                                             
         MVC   DUB(2),=H'11'                                                    
         L     R1,4(R5)                                                         
         CH    R1,DUB                                                           
         BH    EFTB                                                             
         MVC   WORK(1),7(R5)                                                    
*                                  SEE IF VALUE PREVIOUSLY DEFINED              
DATAVA   SR    R1,R1                                                            
         IC    R1,WORK                                                          
         LA    R1,LOOKLIST(R1)                                                  
         CLI   0(R1),0                                                          
         BNE   EDIF                                                             
         MVI   0(R1),C'Y'          SET DEFINED                                  
         MVC   0(1,R8),WORK                                                     
         LA    R8,1(R8)            BUMP TO NEXT ENTRY                           
         LA    R5,L'SCANBLK(R5)                                                 
         SR    R1,R1                                                            
         IC    R1,FNDX                                                          
         LA    R1,1(R1)                                                         
         STC   R1,FNDX                                                          
         B     DATAV4              RETURN FOR MORE                              
*                                  IF 'NONE' INPUT CHECK NO OTHER               
DATAVC   CLI   LOOKLIST,C'Y'       VALUES WERE INPUT                            
         BNE   DATAVE                                                           
         OC    LOOKLIST+1(L'LOOKLIST-1),LOOKLIST+1                              
         BZ    DATAVE                                                           
         XC    BASHDR,BASHDR                                                    
         MVC   BASHDR(30),=C'ERROR ''NONE'' IS NOT COMPATIBLE'                  
         OI    BASHDRH+6,X'80'                                                  
         MVI   FERN,X'FE'          SPECIAL EXIT MODE                            
         B     EXIT                                                             
*                                  ADD ELEMENT TO RECORD                        
DATAVE   LA    R7,TEMP                                                          
         SR    R8,R7                                                            
         STC   R8,TEMP+1                                                        
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
*                                  VALIDATE PROGTYPE EQUIV LIST                 
         LA    R1,PROFLDS          SCAN PROGTYPE EQUIV LIST FIELDS              
         BAS   RE,SCAN                                                          
         BE    *+16                                                             
         L     R1,DMCB             SET FADR TO FIELD IN ERROR                   
         ST    R1,FADR                                                          
         B     EIIF                                                             
*                                                                               
         CLI   NLINES,0            ANY INPUT ?                                  
         BE    DATAVM                                                           
*                                  PROCESS SCAN BLOCK                           
         LA    R5,FLINES                                                        
         ST    R5,ALINE                                                         
         LA    R5,SCANBLK          R5=A(SCAN BLOCK ENTRY)                       
         SR    R6,R6                                                            
         IC    R6,NLINES           R6=TOTAL LINES INPUT                         
         XC    TEMP,TEMP                                                        
         MVC   TEMP(2),=X'7606'    BUILD VIRGIN ELEMENT                         
         LA    R7,TEMP                                                          
         USING CTPQD,R7                                                         
         MVI   FNDX,1                                                           
*                                                                               
*                                                                               
DATAVG   L     R1,ALINE            SET FADR/FNDX FOR THIS ENTRY                 
         CLI   0(R1),X'FF'                                                      
         BE    DATAVM                                                           
         CLC   FNDX,0(R1)                                                       
         BNH   *+16                                                             
         LA    R1,4(R1)                                                         
         MVI   FNDX,1                                                           
         B     DATAVG+4                                                         
         ST    R1,ALINE                                                         
         MVC   FADR,0(R1)                                                       
*                                                                               
         CLI   0(R5),2             L'FIRST                                      
         BL    EFTS                                                             
         CLI   0(R5),3             L'FIRST                                      
         BH    EFTL                                                             
         CLI   1(R5),1             L'SECOND                                     
         BL    EMIF                                                             
         BH    EFTL                                                             
         XC    CTPQDP,CTPQDP                                                    
         MVC   CTPQDP+1(2),12(R5)                                               
         CLI   0(R5),2             2 OR 3 CHR INPUT                             
         BE    *+10                                                             
         MVC   CTPQDP,12(R5)                                                    
         MVC   CTPQCODE,22(R5)                                                  
         LA    R1,CTXDATA                                                       
         SR    RE,RE                                                            
*                                  CHECK NO DUPLICATE CODES ON REC              
DATAVI   CLI   0(R1),0                                                          
         BE    DATAVK                                                           
         CLI   0(R1),X'76'                                                      
         BNE   *+14                                                             
         CLC   CTPQDP,2(R1)                                                     
         BE    EDIF                                                             
         IC    RE,1(R1)                                                         
         AR    R1,RE                                                            
         B     DATAVI                                                           
*                                  OK-ADD AN ELEMENT                            
DATAVK   GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
         LA    R5,L'SCANBLK(R5)    BUMP TO NEXT BLOCK ENTRY                     
         SR    R1,R1                                                            
         IC    R1,FNDX                                                          
         LA    R1,1(R1)                                                         
         STC   R1,FNDX                                                          
         B     DATAVG              RETURN FOR MORE                              
*                                                                               
DATAVM   MVI   FNDX,0              RESET FIELD INDEX (IN CASE OF ERROR)         
         L     RF,AWRITE                                                        
         CLI   ACTN,CHANGE                                                      
         BE    *+8                                                              
         L     RF,AADD                                                          
         LA    R1,EXTCLIH          SET FADR TO FIRST KEY FIELD                  
         ST    R1,FADR                                                          
         BASR  RE,RF               ADD/WRITE RECORD                             
         CLI   DMCB+8,0                                                         
         BNE   EIIO                                                             
*                                  GENERATE TURNAROUND REQUEST                  
         LA    R5,TEMP                                                          
         USING REQD,R5                                                          
         XC    REQHDR,REQHDR                                                    
         MVI   REQUEST,C' '                                                     
         MVC   REQUEST+1(L'REQUEST-1),REQUEST                                   
         MVI   REQNUMB,30                                                       
         MVC   REQPROG,=C'30'                                                   
         MVC   REQAGYA,CTXKAGY                                                  
         MVC   REQSTOR,=C'FILE CONTROL'                                         
         GOTO1 VDATAMR,DMCB,=C'DMADD',=C'CTREQ  ',(R5),(R5)                     
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R5                                                               
*                                                                               
         MVI   FERN,X'FF'          SET NEXT ACTION & EXIT                       
         MVI   NACTN,OKDEL+OKCHA+OKCOPY                                         
         LA    R1,BASACTNH                                                      
         ST    R1,FADR                                                          
         B     EXIT                                                             
         EJECT                                                                  
*              UNSCAN A BLOCK INTO TWA.                                         
*              DUB(1)=NUMBER OF FIELDS                                          
*              DUB+1(3)=A(SCAN BLOCK)                                           
*              DUB+4(4)=A(TABLE OF OUTPUT FIELDS)                               
UNSCAN   NTR1                                                                   
         L     R6,DUB+4                                                         
         MVC   DMCB(4),DUB                                                      
UNSCAN1  SR    R1,R1                                                            
         EX    R1,0(R6)                                                         
         ST    R1,DMCB+4                                                        
         GOTO1 VUNSCAN,DMCB                                                     
         CLI   DMCB,0                                                           
         BE    UNSCANX                                                          
         LA    R6,4(R6)                                                         
         CLI   0(R6),X'FF'                                                      
         BNE   UNSCAN1                                                          
         DC    H'0'                                                             
UNSCANX  XIT1                                                                   
         EJECT                                                                  
*              SCAN A LIST OF FIELDS INTO SCANBLK.                              
*              R1=A(LIST OF INPUT FIELDS)                                       
*                                                                               
SCAN     NTR1                                                                   
         XC    FLINES,FLINES                                                    
         MVI   NLINES,0                                                         
         LR    R5,R1                                                            
         LA    R6,FLINES                                                        
SCAN1    SR    R1,R1                                                            
         EX    R1,0(R5)                                                         
         GOTO1 AFVAL                                                            
         BZ    SCAN2                                                            
         ST    R1,0(R6)                                                         
         ST    R1,DMCB                                                          
         SR    R1,R1                                                            
         IC    R1,NLINES                                                        
         SLL   R1,5                                                             
         LA    R1,SCANBLK(R1)                                                   
         ST    R1,DMCB+4                                                        
         GOTO1 VSCANNER,DMCB                                                    
         CLI   DMCB+4,0                                                         
         BE    SCAN3                                                            
         SR    R1,R1                                                            
         IC    R1,DMCB+4                                                        
         SR    RE,RE                                                            
         IC    RE,NLINES                                                        
         AR    RE,R1                                                            
         STC   RE,NLINES                                                        
         STC   R1,0(R6)                                                         
         LA    R6,4(R6)                                                         
SCAN2    LA    R5,4(R5)                                                         
         CLI   0(R5),X'FF'                                                      
         BNE   SCAN1                                                            
         MVI   0(R6),X'FF'                                                      
         CR    RE,RE                                                            
         B     SCANX                                                            
SCAN3    CR    R1,RE                                                            
SCANX    XIT1                                                                   
         EJECT                                                                  
* CTLFMERRS                                                                     
       ++INCLUDE CTLFMERRS                                                      
         EJECT                                                                  
*              LITERALS ETC.                                                    
*                                                                               
         LTORG                                                                  
*                                                                               
MEDTBL   DC    X'0204'             US/UK SPOT SYSTEM SE NUMBERS                 
*                                                                               
INPLIST  DS    0CL8                                                             
         DC    CL7'NONE   ',X'00'                                               
         DC    CL7'FIRST  ',X'F1'                                               
         DC    CL7'SECOND ',X'F2'                                               
         DC    CL7'THIRD  ',X'F3'                                               
         DC    CL7'FOURTH',X'F4'                                                
         DC    X'FF'                                                            
*                                                                               
DEMFLDS  LA    R1,EXTDE1H                                                       
         LA    R1,EXTDE2H                                                       
         DC    X'FFFF'                                                          
PROFLDS  LA    R1,EXTPE1H                                                       
         LA    R1,EXTPE2H                                                       
         LA    R1,EXTPE3H                                                       
         LA    R1,EXTPE4H                                                       
         LA    R1,EXTPE5H                                                       
         LA    R1,EXTPE6H                                                       
         LA    R1,EXTPE7H                                                       
         LA    R1,EXTPE8H                                                       
         LA    R1,EXTPE9H                                                       
         LA    R1,EXTPEAH                                                       
         LA    R1,EXTPEBH                                                       
         LA    R1,EXTPECH                                                       
         DC    X'FFFF'                                                          
         EJECT                                                                  
*              DSECT TO COVER TEMP W/S                                          
*                                                                               
WRKD     DSECT                                                                  
ALINE    DS    F                                                                
         DS    0F                                                               
FLINES   DS    CL64                                                             
NLINES   DS    C                                                                
FORMAT   DS    C                                                                
COUNTRY  DS    C                                                                
AGYALPH  DS    CL2                                                              
AGYBIN   DS    C                                                                
AGYSEN   DS    C                                                                
LOOKLIST DS    CL256                                                            
SCANBLK  DS    60CL32                                                           
         ORG   SCANBLK                                                          
UNSCNBL1 DS    60CL20                                                           
UNSCNBL2 DS   120CL20                                                           
WRKX     EQU   *                                                                
* CTLFMDSECT                                                                    
       ++INCLUDE CTLFMDSECT                                                     
* CTLFMREQ                                                                      
         PRINT OFF                                                              
       ++INCLUDE CTLFMREQ                                                       
         PRINT ON                                                               
* FADSECTS                                                                      
       ++INCLUDE FADSECTS                                                       
* CTLFMACTNS                                                                    
       ++INCLUDE CTLFMACTNS                                                     
*                                                                               
US       EQU   1                                                                
UK       EQU   2                                                                
         EJECT                                                                  
* CTLFMTWA                                                                      
       ++INCLUDE CTLFMTWA                                                       
* CTLFMEFD                                                                      
       ++INCLUDE CTLFMEFD                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'020CTLFM10   05/20/02'                                      
         END                                                                    
