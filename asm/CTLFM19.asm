*          DATA SET CTLFM19    AT LEVEL 035 AS OF 05/01/02                      
*PHASE TA0219A,+0                                                               
*INCLUDE SCINKEY                                                                
         TITLE 'CTLFM19 - CONTROL FILE MAINT - DEMO ADJUSTMENT RECORDS'         
CTLFM19  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKX-WORKD,**LFMX**                                             
         USING WORKD,RC            RC=A(W/S)                                    
         LR    R9,R1                                                            
         USING LFMTEMP,R9          R9=A(TEMP W/S)                               
         L     R3,ATWA                                                          
         USING LFMSAVE,R3          R3=A(SAVE W/S)                               
         LR    R2,R3                                                            
         USING CTLFMFFD,R2         R2=A(TWA)                                    
         L     R4,AREC                                                          
         USING CTQREC,R4           R4=A(RECORD)                                 
         EJECT                                                                  
*              VALIDATE KEY FIELDS                                              
*                                                                               
KEYVAL   XC    CTQKEY,CTQKEY                                                    
         MVI   CTQKTYP,CTQKTEQU                                                 
*                                  VALIDATE SOURCE CODE                         
         GOTO1 AFVAL,DEMSRCH                                                    
         BZ    EXIT                                                             
         ZIC   R1,FLDH+5                                                        
         BCTR  R1,0                                                             
         LA    RE,SRCTAB           LOOK-UP SOURCE IN TABLE                      
KEYV2    CLI   0(RE),0                                                          
         BE    EIIF                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),FLD                                                      
         BE    *+12                                                             
         LA    RE,L'SRCTAB(RE)                                                  
         B     KEYV2                                                            
         MVC   CTQKSRC,L'SRCTAB-1(RE)                                           
         CLC   DEMSRC(L'SRCTAB-1),0(RE)                                         
         BE    *+14                                                             
         MVC   DEMSRC(L'SRCTAB-1),0(RE)                                         
         OI    DEMSRCH+6,X'80'                                                  
*                                  VALIDATE MEDIA (SUB-FILE)                    
         GOTO1 AFVAL,DEMSUBFH                                                   
         BZ    EXIT                                                             
         ZIC   R1,FLDH+5                                                        
         BCTR  R1,0                                                             
         LA    RE,MEDTAB                                                        
KEYV3    CLI   0(RE),0             TEST E-O-T                                   
         BE    EIIF                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   1(0,RE),FLD                                                      
         BE    *+12                                                             
         LA    RE,L'MEDTAB(RE)                                                  
         B     KEYV3                                                            
         CLC   DEMSUBF,1(RE)                                                    
         BE    *+14                                                             
         MVC   DEMSUBF,1(RE)                                                    
         OI    DEMSUBFH+6,X'80'                                                 
         MVC   CTQKMED,0(RE)                                                    
*                                  ENSURE SOURCE/MEDIA IS VALID                 
         LA    RE,SMTAB                                                         
KEYV3A   CLI   0(RE),0                                                          
         BE    EIIF                                                             
         CLC   0(2,RE),CTQKSRC                                                  
         BE    *+12                                                             
         LA    RE,L'SMTAB(RE)                                                   
         B     KEYV3A                                                           
*                                  VALIDATE AGENCY CODE                         
         MVC   CTQKAGY,=X'FFFF'                                                 
         GOTO1 AFVAL,DEMAGYH                                                    
         BZ    KEYV6                                                            
         CLI   FLDH+5,2                                                         
         BNE   EIIF                                                             
         MVC   CTQKAGY,FLD                                                      
*                                  VALIDATE LOOKUP CODE                         
KEYV6    MVI   CTQKCODE,X'FF'                                                   
         GOTO1 AFVAL,DEMCODEH                                                   
         BZ    *+10                                                             
         MVC   CTQKCODE,FLD                                                     
*                                  VALIDATE CLIENT CODE                         
         MVC   CTQKCLI,=X'FFFFFF'                                               
         GOTO1 AFVAL,DEMCLIH                                                    
         BZ    KEYV8                                                            
         CLI   FLDH+5,2                                                         
         BL    EFTS                                                             
         CLC   CTQKAGY,=X'FFFF'                                                 
         BE    EIIF                                                             
         MVC   CTQKCLI,FLD                                                      
*                                  VALIDATE START BOOK                          
KEYV8    MVC   BOOKMARK,=X'FFFF'   INITIALIZE BOOKMARK                          
         GOTO1 AFVAL,DEMBOOKH                                                   
         BNZ   KEYV8A                                                           
         CLI   ACTN,DISPLAY        FIELD NOT REQUIRED IF ACTN=DISPLAY           
         BE    KEYV8C                                                           
         B     EXIT                                                             
KEYV8A   GOTO1 VDATVAL,DMCB,(2,FLD),WORK                                        
         OC    0(4,R1),0(R1)                                                    
         BZ    EIDF                                                             
         PACK  DUB,WORK(2)                                                      
         CVB   R1,DUB                                                           
         STC   R1,CTQKBOOK                                                      
         PACK  DUB,WORK+2(2)                                                    
         CVB   R1,DUB                                                           
         STC   R1,CTQKBOOK+1                                                    
         XC    CTQKBOOK,=X'FFFF'                                                
         MVC   BOOKMARK,CTQKBOOK   HOLD ONTO BOOK INPUTTED                      
*                                  VALIDATE HUT BOOK                            
KEYV8C   XC    HUTMARK,HUTMARK     INITIALIZE HUT                               
         GOTO1 AFVAL,DEMHUTH                                                    
         BNZ   KEYV8E                                                           
         CLI   ACTN,DISPLAY        FIELD NOT REQUIRED IF ACTN=DISPLAY           
         BE    KEYVD                                                            
         B     EXIT                                                             
*                                                                               
KEYV8E   LA    RE,HUTTAB                                                        
         CLI   CTQKMED,C'C'                                                     
         BNE   *+8                                                              
         LA    RE,HUTTABC                                                       
*                                                                               
         XC    WORK(2),WORK                                                     
         CLI   FLDH+5,1                                                         
         BH    *+14                                                             
         MVC   WORK(1),FLD                                                      
         B     KEYV9                                                            
         TM    FLDH+4,X'08'        TEST IF INPUT NUMERIC                        
         BZ    EFNN                                                             
         OC    FLDH(4),FLDH                                                     
         BZ    EIIF                                                             
         MVC   WORK+1(1),FLDH+3                                                 
*                                  LOOK-UP HUT TABLE FOR SOURCE/CODE            
KEYV9    CLI   0(RE),0                                                          
         BE    EIIF                                                             
         CLC   0(1,RE),CTQKSRC                                                  
         BNE   KEYVB                                                            
         CLI   WORK,0                                                           
         BE    KEYVA                                                            
         CLC   1(1,RE),WORK        MATCH ON HUT FILE CODE                       
         BE    KEYVC                                                            
         B     KEYVB                                                            
KEYVA    CLC   2(1,RE),WORK+1      MATCH ON YEAR                                
         BE    KEYVC                                                            
         B     KEYVB                                                            
KEYVB    LA    RE,L'HUTTAB(RE)                                                  
         B     KEYV9                                                            
KEYVC    MVC   CTQKHUT,1(RE)                                                    
         MVC   SVIVALS,3(RE)       SAVE VALID SVI LIST FROM TABLE               
         MVC   HUTMARK,CTQKHUT     HOLD ONTO HUT INPUT                          
*                                                                               
KEYVD    MVC   KEY,CTQKEY                                                       
         MVC   KEYNEXT,KEY                                                      
         LA    R1,DEMSRCH                                                       
         ST    R1,FADR                                                          
*                                                                               
         CLI   ACTN,DISPLAY        IF USER CHOOSES ACTN=DISPLAY,                
         BE    KEYVDIS              IT GETS PROCESSED SPECIALLY                 
         CLI   ACTN,CHANGE         IF ACTN=CHANGE AND KEY NEQ LKEY              
         BNE   *+18                SET ACTION TO DISPLAY                        
         CLC   KEY,LKEY                                                         
         BE    *+8                                                              
         MVI   ACTN,DISPLAY                                                     
         CLI   ACTN,DISPLAY                                                     
         BE    *+8                                                              
         MVI   UPDATE,C'Y'         SET RFU FOR UPDATABLE ACTIONS                
         GOTO1 AREAD                                                            
         BZ    EXIT                                                             
*                                                                               
         TM    DMCB+8,X'10'        CHECK FOR NOT FOUND                          
         BZ    *+16                                                             
         CLI   ACTN,ADD            ONLY VALID FOR ADD                           
         BE    DATAVAL                                                          
         B     ERNF                                                             
         CLI   ACTN,ADD            FOUND NOT VALID FOR ADD                      
         BE    ERAE                                                             
         TM    DMCB+8,X'02'        DELETED RECORDS MAY ONLY BE DSPLYD           
         BZ    *+12                                                             
         CLI   ACTN,DISPLAY                                                     
         BNE   ERNF                                                             
         CLI   ACTN,CHANGE                                                      
         BE    DATAVAL                                                          
         B     DISPREC                                                          
*                                                                               
* WHEN USER CHOOSES ACTN=DISPLAY, WE WILL NOT REQUIRE THE KEY FIELDS            
*  BOOK AND HUT TO HAVE ANY OR THE CORRECT INPUT.  INSTEAD, WE WILL             
*  DISPLAY THE RECORD WITH THE CORRESPONDING SRC/MED/AGY/CODE/CLNT,             
*  AS INPUTTED BY THE USER, WITH A MATCHING OR THE NEXT BOOK/HUT                
*  COMBINATION AS COMPARED WITH THE USER'S INPUT.  IN THE COMMENTS              
*  NEXT TO THE CODE BELOW, "PRIMARY" KEY WILL REFER TO SRC/MED/AGY/             
*  CODE/CLNT PART OF THE KEY                                                    
*                                                                               
KEYVDIS  MVC   LKEY,KEY            LKEY=KEY W/ INPUT VALUES                     
         XC    CTQKBOOK,CTQKBOOK   READHI FOR 1ST BOOK                          
         XC    CTQKHUT,CTQKHUT                                                  
         MVC   KEY(L'CTQKEY),CTQKEY                                             
         GOTO1 AREADHI                                                          
         CLC   KEY(CTQKBOOK-CTQKEY),CTQKEY    MATCH ON PRIMARY KEYS?            
         BNE   ERNF                 NO, BAD PRIMARY KEY INPUTTED                
         CLC   CTQKEY,LKEY         IF ENTIRE KEY MATCHES,                       
         BE    DISPREC              THEN GO DISPLAY RECORD                      
         MVC   KEY(L'CTQKEY),CTQKEY                                             
*                                                                               
KEYVDISA CLC   CTQKBOOK,BOOKMARK   LOOK FOR BEST MATCH ON BOOKS                 
         BH    KEYVDISB             (BOOK KEY FIELD WAS XC W/ =X'FFFF')         
         BL    *+14                                                             
         MVC   KEY+(CTQKBOOK-CTQKEY),CTQKBOOK                                   
         B     KEYVDISB                                                         
         SPACE 1                                                                
         MVC   KEY(L'CTQKEY),CTQKEY                                             
         GOTO1 ARSEQ                                                            
         BZ    EXIT                                                             
         CLC   KEY(CTQKBOOK-CTQKEY),CTQKEY    MATCH ON PRIMARY KEYS?            
         BNE   KEYVDISB             NO, GO DISPLAY A RECORD W/                  
         B     KEYVDISA              THE LATEST BOOK                            
*                                                                               
KEYVDISB MVI   KEY+(CTQKHUT-CTQKEY),0                                           
         GOTO1 AREADHI                                                          
         CLC   KEY(CTQKHUT-CTQKEY),CTQKEY   EVERYTHING IN THE KEY               
         BE    KEYVDISC             EXCEPT FOR THE HUT VALUE HAD                
         DC    H'0'                 BETTER MATCH                                
*                                                                               
KEYVDISC CLC   CTQKHUT,HUTMARK     GET RECORD WITH MATCHING OR                  
         BNL   KEYVDISX             NEXT HUT VALUE                              
         MVC   KEY(L'CTQKEY),CTQKEY                                             
         GOTO1 ARSEQ                                                            
         CLC   KEY(CTQKHUT-CTQKEY),CTQKEY                                       
         BE    KEYVDISC                                                         
         GOTO1 AREAD               GET PREVIOUS RECORD INTO REC                 
         BNZ   *+6                                                              
         DC    H'0'                                                             
         TM    DMCB+8,X'10'        CHECK FOR NOT FOUND                          
         BZ    *+6                                                              
         DC    H'0'                 IT HAD BETTER BE FOUND                      
*                                                                               
* DISPLAY BOOK AND HUT VALUES, AREC=A(RECORD TO BE DISPLAYED)                   
*                                                                               
KEYVDISX XC    DEMBOOK,DEMBOOK                                                  
         XC    DEMHUT,DEMHUT                                                    
*                                                                               
** BOOK                                                                         
*                                                                               
         XC    DUB,DUB                                                          
         MVC   DUB(L'CTQKBOOK),CTQKBOOK                                         
         XC    DUB(L'CTQKBOOK),=X'FFFF'                                         
         GOTO1 VDATCON,DMCB,(X'83',DUB),(6,WORK),0                              
         ZIC   RE,DMCB+4           R1=L(OUTPUT)                                 
         LA    RF,L'DEMBOOK                                                     
         CR    RE,RF                                                            
         BNH   *+6                                                              
         LR    RE,RF                                                            
         BCTR  RE,0                                                             
         EXMVC RE,DEMBOOK,WORK                                                  
         OI    DEMBOOKH+6,X'80'                                                 
*                                                                               
** HUT                                                                          
*                                                                               
         MVC   DEMHUT(L'CTQKHUT),CTQKHUT                                        
         OI    DEMHUTH+6,X'80'                                                  
         B     DISPREC                                                          
         EJECT                                                                  
*              DISPLAY RECORD                                                   
*                                                                               
DISPREC  TWAXC DEMMODRH                                                         
         LA    R5,CTQDATA          R5=A(ELEMENT)                                
*                                                                               
DISP2    CLI   0(R5),0             TEST E-O-R                                   
         BE    DISPEND                                                          
         CLI   0(R5),X'03'         MODIFIER RULES ELEMENT                       
         BE    DISP6                                                            
         CLI   0(R5),X'05'         INPUT LINES ELEMENT                          
         BE    DISP8                                                            
*                                                                               
DISP4    ZIC   R1,1(R5)            BUMP TO NEXT ELEMENT                         
         AR    R5,R1                                                            
         B     DISP2                                                            
*                                  FORMAT MODIFIER RULES INTO TABLE             
DISP6    LA    R1,SCANTBL          R1=A(OUTPUT ELEMENT)                         
         ZIC   R0,1(R5)                                                         
         SRL   R0,1                                                             
         BCTR  R0,0                R0=NUMBER OF ENTRIES (IN)                    
         LA    RE,2(R5)            RE=A(DATA)                                   
         SR    RF,RF               RF=NUMBER OF ENTRIES (OUT)                   
DISP6A   MVI   0(R1),C' '                                                       
         MVC   1(L'SCANTBL-1,R1),0(R1)                                          
         MVC   0(1,R1),0(RE)                                                    
         MVI   1(R1),C'='                                                       
         MVI   2(R1),C'O'                                                       
         CLI   1(RE),0                                                          
         BE    *+8                                                              
         MVI   2(R1),C'D'                                                       
         LA    R1,L'SCANTBL(R1)                                                 
         LA    RE,2(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,DISP6A                                                        
         LR    R0,RF               FORMAT DATA INTO TWA                         
         GOTO1 =V(SCINKEY),DMCB,(2,DEMMODRH),(32,SCANTBL),(R0),RR=RB            
         B     DISP4                                                            
*                                  FORMAT INPUT LINES ELEMENT                   
DISP8    ZIC   R1,2(R5)            R1=FIELD SEQUENCE NUMBER                     
         LA    RE,DEMADJRH                                                      
         SR    RF,RF                                                            
         TM    1(RE),X'20'         LOOP DOWN TWA FOR OUTPUT FIELD               
         BZ    *+14                                                             
         IC    RF,0(RE)                                                         
         AR    RE,RF                                                            
         B     *-14                                                             
         BCT   R1,*-10                                                          
         ZIC   R1,1(R5)                                                         
         SH    R1,=H'4'            R1=L'OUTPUT FIELD-1                          
         EX    R1,*+8                                                           
         B     DISP4                                                            
         MVC   8(0,RE),3(R5)                                                    
*                                  SET NEXT ACTION & EXIT                       
DISPEND  LA    R1,DEMMODRH                                                      
         ST    R1,FADR                                                          
         TM    CTQSTAT,X'80'                                                    
         BO    *+12                                                             
         MVI   NACTN,OKDEL+OKCHA                                                
         B     *+8                                                              
         MVI   NACTN,OKRES                                                      
         MVI   FERN,X'FF'                                                       
         MVI   FNDX,0                                                           
         B     EXIT                                                             
         EJECT                                                                  
*              ADD/CHANGE RECORD                                                
*                                                                               
DATAVAL  MVI   TEMP,0              BUILD VIRGIN RECORD & ACTIVITY ELEM.         
         GOTO1 ABLDREC                                                          
         GOTO1 ABLDACT                                                          
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
*                                  VALIDATE MODIFIER RULES FIELDS               
         XC    TEMP,TEMP                                                        
         MVI   TEMP,X'03'                                                       
         MVI   TEMP+1,2                                                         
         MVI   OVERFLAG,0                                                       
         LA    R6,DEMMODRH         R6=A(FIRST INPUT FIELD)                      
         LA    R7,2                R7=N'INPUT FIELDS                            
*                                                                               
DATAV2   LR    R1,R6                                                            
         GOTO1 AFVAL                                                            
         BZ    DATAV8                                                           
         GOTO1 VSCANNER,DMCB,(R6),(20,SCANTBL)                                  
         CLI   4(R1),0                                                          
         BE    EIIF                                                             
         MVC   NLINES,4(R1)        SAVE NUMBER OF LINES INPUT                   
         MVI   FNDX,1                                                           
         LA    R8,SCANTBL          R8=A(SCAN TABLE)                             
*                                                                               
DATAV4   CLC   FNDX,NLINES                                                      
         BH    DATAV8                                                           
         CLI   0(R8),1             L'LHS                                        
         BNE   EIIF                                                             
         TM    2(R8),X'40'         V'LHS (ALPHA)                                
         BZ    EIIF                                                             
         CLI   1(R8),1             L'RHS                                        
         BNE   EIIF                                                             
         MVC   WORK(1),12(R8)                                                   
         MVI   WORK+1,0                                                         
         CLI   22(R8),C'O'         RHS MUST BE O OR D                           
         BE    *+16                                                             
         OI    WORK+1,X'01'                                                     
         CLI   22(R8),C'D'                                                      
         BNE   EIIF                                                             
         LA    R1,TEMP             TEST IF RULE ALREADY DEFINED                 
         ZIC   RE,1(R1)                                                         
         LA    R0,2(RE)                                                         
         LA    R1,2(R1)                                                         
         SH    RE,=H'2'                                                         
         BZ    DATAV6                                                           
         SRL   RE,1                                                             
         CLC   0(1,R1),WORK                                                     
         BE    EDIF                                                             
         LA    R1,2(R1)                                                         
         BCT   RE,*-14                                                          
*                                                                               
DATAV6   CLI   WORK+1,0                                                         
         BNE   *+8                                                              
         OI    OVERFLAG,X'80'      SET FLAG IF OVERRIDE RULE FOUND              
         MVC   0(2,R1),WORK        MOVE DATA TO ELEMENT                         
         STC   R0,TEMP+1                                                        
         ZIC   R1,FNDX                                                          
         LA    R1,1(R1)                                                         
         STC   R1,FNDX                                                          
         LA    R8,L'SCANTBL(R8)                                                 
         B     DATAV4                                                           
*                                  BUMP TO NEXT INPUT FIELD                     
DATAV8   ZIC   R1,0(R6)                                                         
         AR    R6,R1                                                            
         TM    1(R6),X'20'         SKIP PROTS                                   
         BO    DATAV8                                                           
         BCT   R7,DATAV2                                                        
         LA    R1,DEMMODRH                                                      
         ST    R1,FADR                                                          
         CLI   TEMP+1,2                                                         
         BE    EMIF                                                             
*                                  ADD RULES ELEMENT                            
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
*                                  VALIDATE HUT ADJUSTMENT VALUES               
         XC    TEMP2,TEMP2                                                      
         LA    R6,DEMADJRH         R6=A(FIRST INPUT FIELD)                      
         LA    R8,5                R5=N'INPUT FIELDS                            
         MVI   SEQNO,0             INPUT LINE SEQUENCE NUMBER                   
*                                                                               
DATAV10  LR    R1,R6                                                            
         GOTO1 AFVAL                                                            
         BZ    DATAV22                                                          
         MVI   FERN,0                                                           
         GOTO1 VSCANNER,DMCB,(R6),(20,SCANTBL)                                  
         CLI   4(R1),0                                                          
         BE    EIIF                                                             
         MVC   NLINES,4(R1)                                                     
         MVI   FNDX,1                                                           
         LA    R7,SCANTBL          R7=A(SCAN TABLE)                             
*                                                                               
DATAV12  CLC   FNDX,NLINES                                                      
         BH    DATAV20                                                          
         CLI   0(R7),0             L'LHS                                        
         BE    EIIF                                                             
         CLI   0(R7),7             L'LHS                                        
         BH    EIIF                                                             
         XC    DUB(16),DUB         BUILD DUMMY FIELD FOR LHS SCAN               
         MVI   DUB,16                                                           
         MVC   DUB+5(1),0(R7)                                                   
         MVC   DUB+8(8),12(R7)                                                  
         GOTO1 VSCANNER,DMCB,DUB,(2,WORK),X'6B7E0060'                           
         CLI   4(R1),1                                                          
         BNE   EIIF                                                             
         CLI   WORK,0              L'LHS                                        
         BE    EIIF                                                             
         CLI   WORK,3              L'LHS                                        
         BH    EIIF                                                             
         TM    WORK+2,X'80'        V'LHS (NUMERIC)                              
         BO    DATAV14                                                          
         CLI   WORK+1,0                                                         
         BNE   EIIF                                                             
         CLC   WORK+12(3),=C'ALL'  TEST FOR ALL DEMOS                           
         BNE   EIIF                                                             
         MVI   LODEM,1                                                          
         MVI   HIDEM,255                                                        
         B     DATAV16                                                          
*                                  VALIDATE NNN OR NNN-MMM                      
DATAV14  OC    WORK+4(4),WORK+4                                                 
         BZ    EIIF                                                             
         OC    WORK+4(3),WORK+4                                                 
         BNZ   EIIF                                                             
         MVC   LODEM,WORK+7                                                     
         CLI   WORK+1,0                                                         
         BNE   *+14                                                             
         MVC   HIDEM,LODEM                                                      
         B     DATAV16                                                          
         OC    WORK+8(4),WORK+8                                                 
         BZ    EIIF                                                             
         OC    WORK+8(3),WORK+8                                                 
         BNZ   EIIF                                                             
         MVC   HIDEM,WORK+11                                                    
         CLC   LODEM,HIDEM                                                      
         BH    EIIF                                                             
*                                  VALIDATE SVI CODE                            
DATAV16  TM    3(R7),X'80'                                                      
         BZ    EFNN                                                             
         OC    8(4,R7),8(R7)                                                    
         BZ    EIIF                                                             
         OC    8(3,R7),8(R7)                                                    
         BNZ   EIIF                                                             
         LA    RE,SVIVALS          LOOK-UP SVI CODE IN TABLE                    
DATAV18  CLI   0(RE),0                                                          
         BE    EIIF                                                             
         CLC   0(1,RE),11(R7)                                                   
         BE    *+12                                                             
         LA    RE,1(RE)                                                         
         B     DATAV18                                                          
*                                  SET SVI CODE IN HUT LIST                     
         ZIC   R0,11(R7)           R0=SVI CODE                                  
         ZIC   RF,HIDEM                                                         
         ZIC   RE,LODEM                                                         
         SR    RF,RE                                                            
         LA    RF,1(RF)            RF=NUMBER OF DEMOS                           
         LA    RE,TEMP2(RE)        RE=STARTING DEMO                             
         STC   R0,0(RE)                                                         
         LA    RE,1(RE)                                                         
         BCT   RF,*-8                                                           
*                                  BUMP TO NEXT TABLE ENTRY                     
         LA    R7,L'SCANTBL(R7)                                                 
         ZIC   R1,FNDX                                                          
         LA    R1,1(R1)                                                         
         STC   R1,FNDX                                                          
         B     DATAV12                                                          
*                                  BUILD INPUT LINES ELEMENT                    
DATAV20  XC    TEMP,TEMP                                                        
         MVI   TEMP,X'05'                                                       
         ZIC   R1,FLDH+5                                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TEMP+3(0),FLD                                                    
         LA    R1,3(R1)                                                         
         STC   R1,TEMP+1                                                        
         IC    R1,SEQNO            BUMP SEQUENCE NUMBER                         
         LA    R1,1(R1)                                                         
         STC   R1,SEQNO                                                         
         MVC   TEMP+2(1),SEQNO                                                  
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
*                                  BUMP TO NEXT INPUT LINE                      
DATAV22  ZIC   R1,0(R6)                                                         
         AR    R6,R1                                                            
         TM    1(R6),X'20'         IGNORE PROT FIELDS                           
         BO    *-10                                                             
         BCT   R8,DATAV10                                                       
         LA    R1,DEMADJRH                                                      
         ST    R1,FADR                                                          
         MVI   FNDX,0                                                           
         OC    TEMP2,TEMP2                                                      
         BNZ   *+12                                                             
         TM    OVERFLAG,X'80'                                                   
         BO    EMIF                                                             
*                                  BUILD HUT RULES ELEMENTS                     
DATAV24  XC    TEMP,TEMP                                                        
         MVI   TEMP,X'04'                                                       
         MVI   TEMP+1,131                                                       
         MVI   TEMP+2,1                                                         
         MVC   TEMP+3(128),TEMP2                                                
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
         MVI   TEMP+2,2                                                         
         MVC   TEMP+3(128),TEMP2+128                                            
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
DATAVEND LA    R1,DEMSRCH                                                       
         ST    R1,FADR                                                          
         MVI   FERN,X'FF'                                                       
         L     RF,AADD                                                          
         CLI   ACTN,ADD                                                         
         BE    *+8                                                              
         L     RF,AWRITE                                                        
         BASR  RE,RF                                                            
         CLI   DMCB+8,0                                                         
         BNE   EIIO                                                             
         MVI   NACTN,OKDEL+OKCHA                                                
         LA    R1,BASACTNH         SET CURSOR & EXIT                            
         ST    R1,FADR                                                          
         B     EXIT                                                             
         EJECT                                                                  
* CTLFMERRS                                                                     
       ++INCLUDE CTLFMERRS                                                      
         EJECT                                                                  
*              LITERALS ETC.                                                    
*                                                                               
         LTORG                                                                  
*                                  TABLE OF VALID SOURCES                       
SRCTAB   DS    0CL9                                                             
         DC    C'NSI     ',C'N'                                                 
         DC    C'ARB     ',C'A'                                                 
         DC    X'00'                                                            
*                                  TABLE OF VALID SUB-FILES                     
MEDTAB   DS    0CL9                                                             
         DC    C'T',CL8'USTV'                                                   
         DC    C'C',CL8'CANTV'                                                  
         DC    X'00'                                                            
*                                  TABLE OF VALID SOURCE/MEDIA COMBOS           
SMTAB    DS    0CL2                                                             
         DC    C'AT'                                                            
         DC    C'NT'                                                            
         DC    C'AC'                                                            
         DC    C'NC'                                                            
         DC    X'00'                                                            
*                                  TABLE OF HUT CODES/SVI CODES                 
HUTTAB   DS    0CL20                                                            
         DC    C'N0',AL1(80,1,2,3,4,7),12X'00'                                  
         DC    C'N1',AL1(81,1,2,3,4,5,6,7,8,9,10,11,12),5X'00'                  
         DC    C'N2',AL1(82,1,2,3,4,5,6,7,8,9,10,11,12),5X'00'                  
         DC    C'N3',AL1(83,1,2,3,4,5,6,7,8,9,10,11,12),5X'00'                  
         DC    C'N5',AL1(85,1,2,3,4,5,6,7,8,9,10,11,12),5X'00'                  
         DC    C'N6',AL1(86,1,2,3,4,5,6,7,8,9,10,11,12),5X'00'                  
         DC    C'N7',AL1(87,1,2,3,4,5,6,7,8,9,10,11,12),5X'00'                  
         DC    C'N8',AL1(88,1,2,3,4,5,6,7,8,9,10,11,12),5X'00'                  
         DC    C'N9',AL1(90,1,2,3,4,5,6,7,8,9,10,11,12),5X'00'                  
         DC    C'NB',AL1(00,1,2,3,4,7),12X'00'                                  
         DC    C'ND',AL1(00,1,2,3,4,7),12X'00'                                  
         DC    C'NE',AL1(00,1),16X'00'      CAMPBELL SOUP 1987                  
         DC    C'NF',AL1(00,1),16X'00'      CAMPBELL SOUP 1988                  
         DC    C'NG',AL1(00,1),16X'00'      CAMPBELL SOUP 1989                  
         DC    C'NH',AL1(00,1,2,3,4,5,6,7,8,9,10,11,12),5X'00'                  
         DC    C'NM',AL1(00,1,2,3,4,7),12X'00'                                  
         DC    C'NJ',AL1(00,1,2,3,4,5,6,7,8,9,10,11,12),5X'00'                  
         DC    C'NN',AL1(91,1,2,3,4,5,6,7,8,9,10,11,12),5X'00'                  
         DC    C'NO',AL1(92,1,2,3,4,5,6,7,8,9,10,11,12),5X'00' 92               
         DC    C'NP',AL1(00,1,3,9,13),13X'00' PEPSI PORT/SEAT                   
         DC    C'NQ',AL1(92,1,2,3,4,5,6,7,8,9,10,11,12),5X'00' 93               
         DC    C'NR',AL1(94,1,2,3,4,5,6,7,8,9,10,11,12),5X'00' 94/5             
         DC    C'NT',AL1(99,1,2,3,4,5,6,7,8,9,10,11,12),5X'00' 98/99            
         DC    C'A0',AL1(80,1,2,3,4,6,7,9),10X'00'                              
         DC    C'A1',AL1(81,1,2,3,4,6,7,9),10X'00'                              
         DC    C'A2',AL1(82,1,2,3,4,6,7,9),10X'00'                              
         DC    C'A3',AL1(83,1,2,3,4,6,7,9),10X'00'                              
         DC    C'A5',AL1(85,1,2,3,4,6,7,9),10X'00'                              
         DC    C'A6',AL1(86,1,2,3,4,6,7,9),10X'00'                              
         DC    C'A8',AL1(78,1,2,3,4,6,7,9),10X'00'                              
         DC    C'A9',AL1(79,1,2,3,4,6,7,9),10X'00'                              
         DC    C'AB',AL1(00,1,2,4,6,7),12X'00'                                  
         DC    C'AC',AL1(00,1,2,3,4,5,6,7,9),9X'00'                             
         DC    C'AD',AL1(00,1,2,3,4,6,7,9),10X'00'                              
         DC    C'AG',AL1(00,1,2,3,4,6,7,9),10X'00'                              
         DC    C'AH',AL1(92,1,2,3,4,6,7,9),10X'00'    1992 SVI'S                
         DC    C'AJ',AL1(00,1,2,3,4,6,7,9),10X'00'                              
         DC    C'AK',AL1(00,1,2,3,4,6,7,9),10X'00'    PE FEB/92 SPCL            
         DC    C'AL',AL1(00,1,2,3,4,6,7,9,13),9X'00'  PE FEB/92 A1849           
         DC    C'AP',AL1(00,1,2,4,6,7),12X'00'                                  
         DC    C'AR',AL1(00,1,2,3,4,6,7,9),10X'00'                              
         DC    C'AS',AL1(00,1,2,3,4,6,7,9),10X'00'    1987 SVI'S                
         DC    C'AT',AL1(00,1,2,3,4,6,7,9),10X'00'    1988 SVI'S                
         DC    C'AU',AL1(00,1,2,3,4,6,7,9),10X'00'    1989 SVI'S                
         DC    C'AV',AL1(00,1,2,3,4,6,7,9),10X'00'    1990 SVI'S                
         DC    C'AW',AL1(91,1,2,3,4,6,7,9),10X'00'    1991 SVI'S                
         DC    X'00'                                                            
*                                  TABLE OF HUT CODES/SVI CODES                 
HUTTABC  DS    0CL20                 (CANADIAN)                                 
         DC    C'NA',AL1(00,1,0,0,0,0),12X'00'                                  
         DC    X'00'                                                            
         EJECT                                                                  
*              DSECT TO COVER TEMP W/S                                          
*                                                                               
WORKD    DSECT                                                                  
OVERFLAG DS    X                                                                
LODEM    DS    X                                                                
HIDEM    DS    X                                                                
SEQNO    DS    X                                                                
NLINES   DS    X                                                                
SVIVALS  DS    XL17                                                             
TEMP2    DS    CL256                                                            
SCANTBL  DS    40CL32                                                           
BOOKMARK DS    CL(L'CTQKBOOK)                                                   
HUTMARK  DS    CL(L'CTQKHUT)                                                    
WORKX    EQU   *                                                                
         SPACE 1                                                                
* CTLFMACTNS                                                                    
         PRINT OFF                                                              
       ++INCLUDE CTLFMACTNS                                                     
         PRINT ON                                                               
* CTLFMDSECT                                                                    
       ++INCLUDE CTLFMDSECT                                                     
* FADSECTS                                                                      
       ++INCLUDE FADSECTS                                                       
         EJECT                                                                  
* CTLFMTWA                                                                      
       ++INCLUDE CTLFMTWA                                                       
* CTLFME6D                                                                      
       ++INCLUDE CTLFME6D                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'035CTLFM19   05/01/02'                                      
         END                                                                    
