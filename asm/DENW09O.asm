*          DATA SET DENW09O    AT LEVEL 041 AS OF 04/17/09                      
*PHASE DENW09OA                                                                 
*INCLUDE DEBKUPDT                                                               
**********************************************************************          
* NAD - FAST WEEKLY NAD CONVERSION                                              
* IPHASE: DENW09I                                                               
* OPHASE: DENW09O                                                               
**********************************************************************          
         TITLE 'DEMCON - FAST WEEKLY NAD CONVERSION - OUTPUT PHASE'             
DENW09O  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 NADWRKX-NADWRKD,DENW09O                                          
         USING NADWRKD,RC          RC=A(TEMP W/S)                               
         LA    R7,2048(RB)                                                      
         LA    R7,2048(R7)                                                      
         USING DENW09O+4096,R7                                                  
         USING DEMCOND,R8          R8=A(GLOBAL W/S)                             
         L     R9,ASREC                                                         
         USING INTERD,R9           R9=A(INTERIM RECORD)                         
         L     RA,ACOMFACS                                                      
         USING COMFACSD,RA         RA=A(COMMON FACILITIES)                      
         B     *+4(R1)                                                          
         B     CNV2                PROCESS A RECORD                             
         B     CNV30               LAST TIME HOOK                               
*                                                                               
CNV2     CLI   FRSTREC,YES         FIRST RECORD TO OPHASE                       
         BNE   CNV2A                                                            
         TM    FLAGS1,CREATE_NETCTREC     NETCTREC=Y                            
         BNO   CNV2A                                                            
         GOTO1 =V(DEBKUPDT),DMCB,,INITQ   INITIALIZE DEBKUPDT                   
CNV2A    MVI   FRSTREC,NO                                                       
*                                                                               
         CLI   INTRTYP,PRCODEQU    USAGE RECDS  -P- (TIME PERIOD)               
         BE    CNV4                                                             
         CLI   INTRTYP,PMCODEQU    PROGRAM RECS -Q-                             
         BE    CNV4                                                             
*                                                                               
CNVX     XMOD1 1                                                                
*                                                                               
XIT      XIT1                                                                   
*                                                                               
         EJECT                                                                  
*********************************************************************           
* BUILD DEMOGRAPHIC RECORDS - FIRST THE KEYS                                    
*********************************************************************           
*                                                                               
CNV4     MVI   PRINTSW,0                                                        
         L     R1,ACOMWRK          ADDRESS OF COMMON WORK AREA                  
         MVC   AVGWKS,0(R1)        NMBER OF WKS IN AVG                          
         MVC   STDATE,1(R1)        START DATE OF AVG RECD                       
         LA    R6,THISKEY                                                       
         XC    THISKEY,THISKEY                                                  
         CLI   INTRTYP,PMCODEQU    PROGRAM RECS -Q- ?                           
         BE    QCNV5               GO BUILD Q RECD KEY                          
         CLI   INTRTYP,PRCODEQU    USAGE   RECS -P- ?                           
         BE    PCNV5               GO BUILD P RECD KEY                          
         SPACE 3                                                                
*                                                                               
         USING PMKEY,R6            BUILD -Q- RECD PROGRAM KEY                   
QCNV5    MVI   PMCODE,PMCODEQU                                                  
         MVC   PMMEDIA,INTKEY+1    MEDIA FROM INTKEY                            
         MVC   PMSRC,OUTSRC                                                     
         MVC   PMBOOK,INTBOOK                                                   
         MVC   PMSTAT,INTSTA                                                    
         MVC   PMSTYP,INTSTYP                                                   
         MVC   PMPNUM,INTPNUM                                                   
         MVC   PMBTYP,THISBOOK                                                  
         CLC   PREVKEY,THISKEY     IF SAME KEY AS LAST RECD,                    
         BE    CNV20               MERGE THE RECORDS                            
         MVC   THISBOOK,INTBTYP                                                 
         MVC   PMBTYP,INTBTYP                                                   
*                                                                               
QCNV6    OC    PREVKEY,PREVKEY                                                  
         BZ    QCNV7                                                            
         BAS   RE,PUTTAPE          PUT OUT PREV RECD                            
         CLC   PREVKEY(8),=C'PWNHUT N'                                          
         BE    QCNV6A                                                           
         CLC   PREVKEY(8),=C'PNNUUUUN'   SAVE HUT UNIVERSE                      
         BE    QCNV6A                                                           
         CLC   PREVKEY(8),=C'PNNHUT N'                                          
         BNE   QCNV7                                                            
         CLI   AVGWKS,4            FOR SPECIAL TIME PERD TAPE, SAVE             
         BL    QCNV7               4/5 WK AVG HUT DATA IN WKLY YY00 REC         
QCNV6A   BAS   RE,SVHUT                                                         
*                                                                               
QCNV7    MVC   THISSQH,INTSQH                                                   
         MVC   THISDW,INTDAYWK                                                  
         MVC   THISDBIT,INTDYBIT                                                
         MVC   PREVKEY,THISKEY     SAVE KEY VALUES                              
         MVC   PREVSQH,THISSQH                                                  
         MVC   PREVDW,THISDW                                                    
         MVC   PREVDBIT,THISDBIT                                                
*        CLI   THISBOOK,0                                                       
*        BNE   *+8                                                              
         BAS   RE,GENN                                                          
         MVC   THISKEY,PREVKEY     RESTORE KEY VALUES                           
         GOTO1 ABLDREC,DMCB,PMKEY  BUILD -Q- RECD                               
         B     BLDELEM             BUILD ELEMENTS                               
         EJECT                                                                  
*                                                                               
         USING PRKEY,R6                                                         
PCNV5    MVI   PRCODE,PRCODEQU     BUILD -P- RECD KEY                           
         MVC   PRMEDIA,INTKEY+1    MEDIA FROM INTKEY                            
         MVC   PRSRC,OUTSRC                                                     
         MVC   PRSTAT,INTSTA                                                    
         MVC   PRSTYP,INTSTYP                                                   
         MVC   PRBOOK,INTBOOK                                                   
         MVC   PRBTYP,THISBOOK                                                  
         MVC   PRSTIM,INTSQH                                                    
         MVC   PRDW,INTDAYWK                                                    
         MVC   THISSQH,PRSTIM                                                   
         MVC   THISDW,PRDW                                                      
         CLC   PREVKEY,THISKEY     IF SAME KEY AS LAST RECD,                    
         BNE   PCNV5A              NO, NEW BKTYPE                               
         LA    R1,INTKEY                                                        
         CLC   PRVMKT,PRBTYP-PRKEY+4(R1)                                        
         BNE   CNV20               DIFF MKT BRKS, MERGE THE RECORDS             
         ZIC   R1,THISBOOK         SAME TIME PERIOD, BUMP BKTYP CTR             
         LA    R1,1(R1)                                                         
         STC   R1,THISBOOK                                                      
         MVC   PRBTYP,THISBOOK     NEW RECD, DIFF BKTYP                         
         B     PCNV6                                                            
*                                                                               
PCNV5A   MVC   THISBOOK,INTBTYP    RESET BKTYP FIELD                            
         MVC   PRBTYP,INTBTYP                                                   
*                                                                               
PCNV6    OC    PREVKEY,PREVKEY                                                  
         BZ    PCNV7                                                            
         BAS   RE,PUTTAPE          PUT OUT PREV RECD                            
         CLC   PREVKEY(8),=C'PWNHUT N'                                          
         BE    PCNV6A                                                           
         CLC   PREVKEY(8),=C'PNNUUUUN'   SAVE HUT UNIVERSE                      
         BE    PCNV6A                                                           
         CLC   PREVKEY(8),=C'PNNHUT N'                                          
         BNE   PCNV7                                                            
         CLI   AVGWKS,4                                                         
         BL    PCNV7                                                            
PCNV6A   BAS   RE,SVHUT                                                         
*                                                                               
PCNV7    MVC   THISSQH,INTSQH                                                   
         MVC   THISDW,INTDAYWK                                                  
         LA    R1,INTKEY                                                        
         MVC   PRVMKT,PRBTYP-PRKEY+4(R1)   MKT NUMBER                           
         MVC   PREVKEY,THISKEY     SAVE KEY VALUES                              
         MVC   PREVSQH,THISSQH                                                  
         MVC   PREVDW,THISDW                                                    
         MVC   THISKEY,PREVKEY     RESTORE KEY VALUES                           
         GOTO1 ABLDREC,DMCB,PRKEY  BUILD -P- RECD                               
         B     BLDELEM             BUILD ELEMENTS                               
         EJECT                                                                  
**********************************************************************          
*BLDELEM - BUILD ELEMENTS FOR EACH OF TH  -P- AND -Q- RECS                      
**********************************************************************          
*                                                                               
BLDELEM  DS    0H                                                               
         LA    R6,TEMP             BUILD MARKET TYPE ELEMENT                    
         XC    TEMP,TEMP                                                        
         USING MARELEM,R6                                                       
         MVI   MARCODE,MARCODEQ                                                 
         MVI   MARELN,MARLNEQ                                                   
         MVC   MARNO,INTMRKT                                                    
         MVC   MARTYPE,INTMTYP                                                  
         MVI   MARSTYP,0                                                        
         MVC   MARDATE,TODAYB                                                   
         GOTO1 APUTEL,MARELEM                                                   
*                                                                               
         CLC   INTSTA(4),=C'UUUU'  UNIVERSE P-RECD?                             
         BE    BLDEL25             DEMOS ELEMENTS ONLY                          
*                                                                               
         CLC   INTBOOK,=AL2(JAN_06)                                             
         BL    BLDEL1                                                           
         L     RF,=A(BLDDT)                                                     
         BRAS  RE,BLDDT                                                         
         GOTO1 APUTEL,TEMP                                                      
*                                                                               
         USING PHTELEM,R6          BUILD NETWORK PROGRAM TYPE ELEMENT           
BLDEL1   XC    TEMP,TEMP           CLEAR ELEMENT AREA                           
         MVI   PHTCODE,PHTCODEQ                                                 
         MVI   PHTLEN,PHTLNEQ                                                   
         MVC   PHTDTYP,INTDTYP                                                  
         MVC   PHTPTYP,INTPTYP                                                  
         MVC   PHTDPT,INTDPT                                                    
         MVC   PHTPREM,INTPREM                                                  
         MVC   PHTRCH,INTAUD                                                    
         MVC   PHTSCNT,INTSTAC                                                  
         MVC   PHTCOVR,INTCOV                                                   
         MVC   PHTNTI,INTPNTI      ORIG NTI PRG NUMBER                          
         MVI   PHTRSF,0                                                         
         CLI   INTSTYP,C'B'                                                     
         BNE   *+8                                                              
         MVI   PHTRSF,X'01'        MARK AS A BREAKOUT                           
         CLI   INTRTYP,C'P'        PRG 1/2HR RECDS                              
         BNE   *+10                                                             
         MVC   PHTDDS(2),INTVAR+2  PROGRAM NUMBER                               
         GOTO1 APUTEL,PHTELEM                                                   
*                                                                               
         USING PHELEM,R6           BUILD DAY/QTR HOUR ELEMENT                   
         XC    TEMP,TEMP           CLEAR ELEMENT AREA                           
         MVI   PHCODE,PHCODEQ                                                   
         MVI   PHELN,PHELNEQ                                                    
         MVC   PHPNUM,INTPNUM                                                   
         MVC   PHDUR,INTDUR        QUARTER HOUR DURATION                        
         MVC   PHDTYPE,INTDTYP                                                  
         MVC   PHDWKS,INTDAYWK                                                  
         NI    PHDWKS,X'0F'                                                     
         MVC   PHDBOOK,INTBOOK                                                  
         GOTO1 APUTEL,PHELEM                                                    
*                                                                               
         USING PPNELEM,R6          BUILD PROGRAM NAME ELEMENT                   
         MVI   PPNCODE,PPNCODEQ                                                 
         MVC   PPNNME(L'INTPNAME),INTPNAME                                      
         LA    R5,PPNNME+L'INTPNAME-1                                           
         LA    R1,L'INTPNAME-1                                                  
         CLI   0(R5),C' '          FIND RIGHTMOST NON-BLANK                     
         BNE   *+10                                                             
         BCTR  R5,0                                                             
         BCT   R1,*-10                                                          
         LA    R1,3(R1)                                                         
         STC   R1,PPNELN                                                        
         GOTO1 APUTEL,PPNELEM                                                   
*                                                                               
         CLI   THISKEY,PMCODEQU    ----P-NET   AND  Q ONLY -----------          
         BE    BLDEL3              ACCEPT ALL Q-RECDS                           
         LA    RE,NETMKTS          TEST NETWORK ON P-RECDS                      
BLDEL2   CLI   0(RE),X'FF'         PRG 1/2HR RECS FOR NETS IN THIS TBL          
         BE    BLDEL25             BYPASS                                       
         CLC   0(3,RE),INTSTA                                                   
         BE    BLDEL3              GOOD STATION                                 
         LA    RE,L'NETMKTS(RE)                                                 
         B     BLDEL2                                                           
*                                                                               
BLDEL3   DS    0H                                                               
         USING NTELEM,R6           NO, BLD NETWORK PROG RUN/TIME ELEM           
         XC    0(NTLENEQ2,R6),0(R6) CLEAR ELEMENT AREA                          
         MVI   NTCODE,NTCODEQU                                                  
         MVI   NTLEN,NTLENEQ                                                    
         MVC   NTSQH,INTSQH                                                     
         MVC   NTEQH,INTEQH                                                     
         MVC   NTDUR,INTDURM       DURATION IN MINUTES                          
*                                                                               
         ZIC   R0,INTDURM          IF 2-BYTE DURATION DIFFERENT THAN            
         SR    R1,R1               THE 1-BYTE VALUE,                            
         ICM   R1,3,INTDURM2                                                    
         CR    R0,R1                                                            
         BE    *+14                                                             
         MVC   NTDUR2,INTDURM2     ADD LONG DURATION TO ELEMENT                 
         MVI   NTLEN,NTLENEQ2                                                   
*                                                                               
         MVC   NTSTIM,INTSTIM                                                   
         MVC   NTETIM,INTETIM                                                   
         MVC   NTDAY,INTDYBIT      DAY CODE'S BIT SETTING                       
         GOTO1 APUTEL,NTELEM                                                    
*                                                                               
         CLI   INTDAYWK,X'00'      BUILD VAR RUN/TIME ELEMENTS. M-F?            
         BE    *+20                                                             
         CLI   INTDAYWK,X'80'      M-S?                                         
         BE    *+12                                                             
         CLI   INTDAYWK,X'90'      VAR                                          
         BNE   BLDEL25                                                          
         LA    R5,INTVAR                                                        
         USING INTVARD,R5                                                       
         LA    R2,7                7 DAYS                                       
         LA    R3,X'40'            START WITH MONDAY                            
BLDEL5   CLI   0(R5),0                                                          
         BE    BLDEL10                                                          
         XC    0(NTLENEQ2,R6),0(R6) CLEAR ELEMENT AREA                          
         MVI   NTCODE,NTCODEQU                                                  
         MVI   NTLEN,NTLENEQ                                                    
         MVC   NTSQH,INTVSQ        START QH                                     
         MVC   NTEQH,INTVEQ        END QH                                       
         MVC   NTDUR,INTVSDUR      SHORT DURATION IN MINUTES (MAX 240)          
*                                                                               
         ZIC   R0,INTVSDUR         IF 2-BYTE DURATION DIFFERENTS THAN           
         SR    R1,R1               THE 1-BYTE VALUE,                            
         ICM   R1,3,INTVLDUR                                                    
         CR    R0,R1                                                            
         BE    *+14                                                             
         MVC   NTDUR2,INTVLDUR     ADD LONG DURATION TO ELEMENT                 
         MVI   NTLEN,NTLENEQ2                                                   
*                                                                               
         MVC   NTSTIM,INTVSTIM     START TIME                                   
         MVC   NTETIM,INTVETIM     END TIME                                     
         STC   R3,NTELEM           INDIVIDUAL DAY CODE                          
         GOTO1 APUTEL,NTELEM                                                    
BLDEL10  LA    R5,INTVARLQ(R5)     NEXT DAY IN INTVAR                           
         SRL   R3,1                NEXT INDIVIDUAL DAY                          
         BCT   R2,BLDEL5                                                        
         DROP  R5                                                               
*                                                                               
BLDEL25  XC    TEMP,TEMP           BUILD SECTION LEAD ELEM                      
         USING SLELEM,R6                                                        
         MVI   SLCODE,SLCODEQ                                                   
         MVI   SLLEN,3                                                          
         LA    RE,INTKEY                                                        
         CLI   0(RE),PMCODEQU       Q RECORD?                                   
         BNE   *+14                                                             
         MVC   SLSECT,PMBTYP-PMKEY+3(RE)   MKT NUMBER ON -Q- RECD               
         B     *+10                                                             
         MVC   SLSECT,PRBTYP-PRKEY+4(RE)   MKT NUMBER ON -P- RECD               
         GOTO1 APUTEL,SLELEM                                                    
         CLC   INTSTA(4),=C'UUUU'  ONLY NEED MKT & SECTION LEAD ELMS            
         BE    BLDEL27                                                          
*                                                                               
         B     BLDEL27             NO EPISODE ELEM FOR NAD                      
*                                                                               
         USING PPTELEM,R6                                                       
         XC    TEMP,TEMP           BUILD EPISODE TITLE ELEM                     
         CLI   INTTITLE,C' '                                                    
         BNE   *+14                                                             
         CLC   INTTITLE(L'INTTITLE-1),INTTITLE+1                                
         BE    BLDEL27             BYPASS IF NO EPISODE PRESENT                 
         MVI   PPTCODE,PPTCODEQ                                                 
         MVC   PPTTITLE(L'INTTITLE),INTTITLE                                    
         LA    R5,PPTTITLE+L'INTTITLE-1                                         
         LA    R1,L'INTTITLE-1                                                  
         CLI   0(R5),C' '          FIND RIGHTMOST NON-BLANK                     
         BNE   *+10                                                             
         BCTR  R5,0                                                             
         BCT   R1,*-10                                                          
         LA    R1,3(R1)                                                         
         STC   R1,PPTELN                                                        
         GOTO1 APUTEL,PPTELEM                                                   
*                                  BUILD DEMO ELEMENTS                          
BLDEL27  LA    R5,DBLOCKA                                                       
         USING DBLOCKD,R5                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'NAD'                                                   
         L     RE,AOREC                                                         
         LA    RE,4(RE)                                                         
         ST    RE,DBAREC                                                        
         LA    RE,DRFRSTEL-DRKEY(RE)                                            
         ST    RE,DBAQUART                                                      
         MVC   DBSELMED,MEDIA      --MEDIA-- NOT INTKEY+1                       
         MVC   DBSELSRC,OUTSRC                                                  
         MVC   DBSELBK,INTIBOOK                                                 
         ST    RA,DBCOMFCS                                                      
         MVC   WORK(10),OFORMAT    SET UP OUTPUT FORMAT BLOCK                   
         MVC   WORK+7(2),INTIBOOK  TO FORCE BOOK ON X'5E' ELEMENT               
         MOVE  (CONDWORK,1100),INTACCS                                          
*        GOTO1 CDEMEL,DMCB,(C'C',WORK),DBLOCKD,CONDWORK                         
         LA    R1,DMCB                                                          
         LA    RE,WORK                                                          
         ST    RE,0(R1)                                                         
         MVI   0(R1),C'C'                                                       
         LA    RE,DBLOCKD                                                       
         ST    RE,4(R1)                                                         
         LA    RE,CONDWORK                                                      
         ST    RE,8(R1)                                                         
         L     RF,CDEMEL                                                        
         BASR  RE,RF                                                            
         CLI   DBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         OC    CONDWORK(2),CONDWORK   TEST FOR ANY DEMO ELEMENTS                
         BZ    BLDEL35             NONE-PUT RECORD TO TAPE                      
         LA    R1,CONDWORK+2                                                    
         SR    R0,R0                                                            
*                                  ADD DEMO ELEMENTS TO RECORD                  
BLDEL30  CLI   0(R1),0                                                          
         BE    BLDEL35                                                          
         GOTO1 APUTEL,(R1)                                                      
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     BLDEL30                                                          
*                                  PUT RECORD TO TAPE & SAVE VALUES             
BLDEL35  MVC   PREVKEY,THISKEY                                                  
         MVC   PREVSQH,THISSQH                                                  
         MVC   PREVDW,THISDW                                                    
         B     CNVX                                                             
         EJECT                                                                  
***********************************************************************         
*SVHUT   - SAVE WEEKLY HUT IN BUFFER FOR YEARLY HUT RECORD FOR ALL WKS          
*          ONLY SAVE TOTAL SAMPLE HUT.                                          
*          M-F  DATA IS NOT SAVED FOR PRIME TIME.                               
*          NOTE: UPON ENTRY, AOREC HAS THE LAST WKLY HUT RECD.                  
*                AND PREVKEY CONTAINS IT'S KEY                                  
*        - SPECIAL HANDLING FOR SPECIAL TIME PERIOD TAPE WITH 4/5 WK            
*          AVG DATA ONLY.  STDATE WILL CONTAIN START DATE OF THE AVG            
*          SVHUT WILL EXPLODE COPY AVG INTO EACH WK'S BUCKET.                   
***********************************************************************         
*                                                                               
SVHUT    NTR1                                                                   
         MVI   TEST,0                                                           
         LA    R6,PREVKEY          SAVE AWAY WEEKLY HUTS IN BUFFER              
         USING PRKEY,R6            TO LATER OUTPUT TO YEARLY HUT RECD           
         CLC   PREVKEY(8),=C'PNNUUUUN'   GET HOMES UNIV FROM RECD               
         BE    SVHUT10                                                          
*                                                                               
         TM    PRDW,X'0F'          NORMAL DAYS ONLY (00,10,..70)                
         BNZ   SVHUTX                                                           
         CLI   PRDW,X'80'          M-S DATA?                                    
         BE    SVHUTX              YES BYPASS ALL OF IT                         
         CLI   PRDW,X'00'          M-F DATA?                                    
         BNE   SVHUT10             NO, SAVE ALL INDIV DAY DATA                  
         CLI   PREVSQH,48          SAVE M-F FOR NON-PRIME DATA ONLY             
         BL    SVHUT10                                                          
         CLI   PREVSQH,68                                                       
         BL    SVHUTX                                                           
*                                                                               
SVHUT10  LA    R5,DBLOCKA          LOOK UP HUT - PLACE IT IN DUB                
         MVI   TEST,1                                                           
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'NTI'                                                   
         ST    RA,DBCOMFCS                                                      
         L     RE,AOREC            LAST PWN HUT RECORD BUILT                    
         LA    RE,4(RE)                                                         
         ST    RE,DBAREC                                                        
         LA    RE,PRFRSTEL-PRKEY(RE)                                            
         ST    RE,DBAQUART                                                      
         GOTO1 CDEMOUT,DMCB,(C'L',DEMLIST),DBLOCKD,DUB                          
         CLI   DBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         OC    DUB(8),DUB                                                       
         BZ    SVHUTX              HOMES AND/OR UNIVHOMES NOT ON RECD           
*                                                                               
         CLC   PREVKEY(8),=C'PNNUUUUN'   GET HOMES UNIV FROM RECD               
         BNE   *+14                                                             
         MVC   UNIVHUT,DUB+4       SAVE AWAY UNIV HUT                           
         B     SVHUTX                                                           
*                                                                               
*CALC HUT RATING                                                                
         OC    UNIVHUT,UNIVHUT                                                  
         BNZ   *+6                 NO UNIV PRESENT                              
         DC    H'0'                                                             
         SR    R0,R0                                                            
         L     R1,DUB              HOMES IMPRESSIONS                            
         M     R0,=F'10000'                                                     
         L     RE,UNIVHUT                                                       
         DR    R0,RE                                                            
         AH    R1,=H'5'            R1 HAS THE RESULT OF DIVISION                
         SR    R0,R0                                                            
         D     R0,=F'10'           ROUNDING                                     
         ST    R1,DUB              SAVE HUT RATING                              
*                                                                               
*ADD BOOK (WEEK) TO BOOKLIST                                                    
         CLI   AVGWKS,0            SPECIAL TIME PERIOD TAPE?                    
         BE    SVHUT18             NO                                           
         XC    BOOKLIST,BOOKLIST                                                
         LA    R5,BOOKLIST         YES. SET UP ALL 4/5 BOOKS IN BKLST           
         ZIC   R2,AVGWKS           GET 4/5 NET-WEEKS FROM STDATE                
         B     SVHUT13                                                          
*                                                                               
SVHUT12  GOTO1 VADDAY,DMCB,STDATE,STDATE,7                                      
SVHUT13  GOTO1 VNETWEEK,DMCB,STDATE,VGETDAY,VADDAY                              
         MVC   0(1,R5),4(R1)       YEAR                                         
         MVC   1(1,R5),8(R1)       WEEK                                         
         LA    R5,L'PRBOOK(R5)                                                  
         BCT   R2,SVHUT12                                                       
*                                                                               
         MVC   NBOOKS,AVGWKS                                                    
         ZIC   R2,AVGWKS                                                        
         LR    R0,R2                                                            
         SR    R2,R2                                                            
         B     SVHUT35                                                          
*                                                                               
SVHUT18  LA    R0,BOOKS            MAX # BOOKS EXPECTED (5 WEEKS)               
         LA    RE,BOOKLIST         RE=BOOK LIST= ACTUAL WEEK #                  
         SR    R2,R2               R2=INDEX REGISTER                            
SVHUT20  OC    0(L'PRBOOK,RE),0(RE)                                             
         BZ    SVHUT25                                                          
         CLC   PRBOOK,0(RE)        SEE IF WEEK IS ALREADY IN LIST               
         BE    SVHUT35             IT IS                                        
         LA    R2,1(R2)                                                         
         LA    RE,L'PRBOOK(RE)                                                  
         BCT   R0,SVHUT20                                                       
         DC    H'0'                BLOWN BOOK LIMIT-TOO MANY WKS INPUT          
*                                                                               
SVHUT25  MVC   0(L'PRBOOK,RE),PRBOOK   ADD WEEK TO LIST                         
         LA    R1,1(R2)            N'BOOKS(WEEKS)=INDEX+1                       
         STC   R1,NBOOKS                                                        
*                                                                               
SVHUT30  LA    R0,1                LOOP ONLY ONCE FOR INDIV WK DATA             
SVHUT35  STC   R2,SVBOOK           SAVE BOOK BUCKET IN BOOKLIST                 
         MH    R2,BOOKLEN          INDEX TO BOOK DATA IN HUT BUFFER             
         A     R2,VPUTBUFF         HUTS SAVED IN PUTBUFF                        
         LR    R5,R2               SAVE BOOK DISPLACEMENT INTO BUFF             
         ZIC   R1,PRDW             GET DAY CODE                                 
         SRL   R1,4                SHIFT TO LOW ORDER NIBBLE                    
         CLI   PRDW,X'00'          M-F DATA?                                    
         BNE   SVHUT35A            YES, DON'T DIV DAY DATA FOR SUMS             
         CLI   PREVSQH,48          USE M-F AVG FOR NON PRIME-TIME               
         BL    SVHUT35A             BEFORE 6PM AND AFTER 11PM                   
         CLI   PREVSQH,68                                                       
         BH    SVHUT35A                                                         
         B     SVHUTX              DON'T FILL PRIME TIME BUCKET                 
*                                                                               
SVHUT35A BAS   RE,SVHUT40          SAVE DAY IN BUFFER                           
         CLI   PRDW,X'00'          M-F DATA?                                    
         BE    SVHUT37             YES --DONE                                   
         LR    R2,R5               CONTRIBUTE TO M-S AVG BUCKET                 
         LA    R1,X'08'            R1= M-S  BUCKET IN BUFFER                    
         BAS   RE,SVHUT40          SUM INDIV DAYS TO GENERATE M-S DATA          
*                                                                               
*                                  UPDATE M-F BUCKET FOR QHRS:48-68             
SVHUT36  CLI   PRDW,X'70'          SUN DATA?                                    
         BE    SVHUT37                                                          
         CLI   PRDW,X'60'          SAT DATA?                                    
         BE    SVHUT37                                                          
         LR    R2,R5               ELSE, SUM M-F FOR PRIME TIME DATA            
         LA    R1,X'00'            R1= M-F (SUM INIV DAYS IN M-F BUF)           
         BAS   RE,SVHUT40                                                       
*                                                                               
SVHUT37  LR    R2,R5               RESTORE R2                                   
         CLI   AVGWKS,0            FOR REG WKLY RECD, DONE                      
         BE    SVHUTX                                                           
         ZIC   R2,SVBOOK           RESTORE BOOKLIST BUCKET                      
         LA    R2,1(R2)                                                         
         STC   R2,SVBOOK           BUMP TO NEXT BOOK IN LIST                    
         BCT   R0,SVHUT35          ELSE FOR SP. TIME TAPE-EXPLODE WKS           
         B     SVHUTX                                                           
*                                                                               
         SPACE 2                                                                
SVHUT40  DS    0H                  MINOR SUB-ROUTINE TO POST HUT TO BUF         
         MH    R1,DAYLEN           R1=DAYCODE * L'ONE DAY'S DATA                
         LA    R2,0(R1,R2)                                                      
         ZIC   R1,PRSTIM                                                        
         SRL   R1,1                CONVERT QH TO HALF HOUR                      
         MH    R1,=H'5'            5BYTE HUT BUCKETS                            
         LA    R2,0(R1,R2)         R2=POINTER TO ENTRY                          
*                                                                               
         ZIC   R1,0(R2)            R1=HOW MANY HUTS IN THIS BUCKET              
         LA    R1,1(R1)            INCREMENT COUNT OF HUTS                      
         STC   R1,0(R2)            --USED FOR M-F PRIME TIME AVG                
         ICM   R1,15,1(R2)         GET HUT VALUE SUM                            
         A     R1,DUB              ADD CURRENT HUT TO PREV HUT                  
         STCM  R1,15,1(R2)         SAVE AWAY IN BUCKET                          
         BR    RE                  RETURN                                       
*                                                                               
SVHUTX   DS    0H                                                               
SVHUTXX  B     XIT                                                              
         EJECT                                                                  
*********************************************************************           
*GENN -  GENEREREATE THE -N- RECS                                               
*********************************************************************           
GENN     NTR1                                                                   
         ZIC   R1,PREVDW           FIND DAY/WEEK IN KEY                         
         SRL   R1,4                                                             
         STC   R1,HALF                                                          
         LA    R3,DAYTAB           DAYBITS TABLE. START AT MONDAY               
*                                                                               
         LA    R2,5                LOOP 5-DAYS IF M-F RECD                      
         CLI   HALF,0              DAYS FOR M-F OR M-SUN RECS (X'00')           
         BE    GENN2               ALSO ADDING PASSIVE RECS UNDER M-F           
         LA    R2,7                LOOP SEVEN DAYS IF M-SUN                     
         CLI   HALF,8              TEST FOR M-SUN (X'80')                       
         BE    GENN2                                                            
         CLI   HALF,9              TEST FOR VAR START TIMES (X'90')             
         BE    GENN2                                                            
         LA    R2,1                ELSE, INDIV DAY RECD. LOOP ONCE              
         MVI   HALF,C'Y'           SET SWITCH FOR SINGLE DAY                    
*                                                                               
GENN2    LA    R6,THISKEY          BUILD 'N' RECORD KEY                         
         USING PNKEY,R6                                                         
         XC    THISKEY,THISKEY                                                  
         MVI   PNCODE,PNCODEQU                                                  
         MVC   PNMEDIA,INTKEY+1    MEDIA FROM INTKEY                            
*        MVC   PNMEDIA,MEDIA                                                    
         MVC   PNSRC,OUTSRC                                                     
         MVC   PNBOOK,INTBOOK                                                   
         MVC   PNSTAT,INTSTA                                                    
         MVC   PNBTYP,THISBOOK     CHANGED FROM INTBTYP                         
         MVC   PNBSIND,INTSTYP                                                  
         MVC   PNDW,PREVDW                                                      
         CLI   HALF,C'Y'           SINGLE DAY                                   
         BE    GENN4               SAVE PREVDW IN KEY OF N-REC                  
*                                                                               
         CLI   HALF,9              VARIOUS DAYS/TIMES? (X'90' SHIFTED)          
         BNE   GENN3               YES                                          
         MVC   DUB(1),PREVDBIT     ACTUAL DAYS RAN                              
         NC    DUB(1),1(R3)        AND WITH CURRENT DAY ON TABLE                
         BZ    GENN5               DIDN'T AIR THIS DAY--NEXT DAY                
*                                                                               
GENN3    MVC   PNDW,0(R3)          SAVE DAY CODE FROM TABLE                     
         MVC   DUB(1),PREVDW                                                    
         NI    DUB,X'0F'           ISOLATE WEEK NIBBLE                          
         OC    PNDW,DUB            COMBINE DAY AND WEEK NIBBLES                 
*                                                                               
GENN4    MVC   PNSTIM,INTSQH                                                    
         MVC   PNPNUM,INTPNUM                                                   
         MVC   PNACTDAY,PREVDW     ACTUAL DAY                                   
         CLI   HALF,9              VAR?                                         
         BNE   *+10                                                             
         MVC   PNACTDAY,0(R3)      SAVE DAY CODE FROM TABLE                     
         MVC   PNACTDUR,INTDURM    DURATION IN MINUTES                          
         GOTO1 ABLDREC,DMCB,(C'P',PNKEY)                                        
         GOTO1 APUTTAPE                                                         
GENN5    LA    R3,L'DAYTAB(R3)     BUMP TO NEXT DAY CODE                        
         BCT   R2,GENN2                                                         
         CLI   HALF,C'Y'           TEST FOR SINGLE DAY                          
         BE    CNVX                ALL DONE                                     
         CLI   HALF,9              IF VAR, DONE                                 
         BE    CNVX                                                             
         LA    R2,1                                                             
         MVI   HALF,C'Y'           NOW ADD THE M-F OR M-SUN REC                 
         B     GENN2                                                            
         EJECT                                                                  
**********************************************************************          
*CNV20 - MERGE RECORDS WITH SAME KEYS                                           
**********************************************************************          
CNV20    DS    0H                                                               
         LA    R5,DBLOCKA                                                       
         USING DBLOCKD,R5                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'NAD'                                                   
         L     RE,AOREC            POSITION TO END OF RECORD                    
         LA    RE,4(RE)                                                         
         ST    RE,DBAREC                                                        
         L     RE,AOREC                                                         
         SR    RF,RF                                                            
         ICM   RF,3,0(RE)          MILDLY SIMPLER THAN ABOVE                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
* SEE IF REC TOO BIG                                                            
         LA    R0,1644    NOT ENOUGH ROOM IN REC FOR MKT (MAXREC=2000)          
         CR    RF,R0                                                            
         BL    CNV25                                                            
         ZIC   R1,THISBOOK                                                      
         LA    R1,1(R1)                                                         
         STC   R1,THISBOOK                                                      
         LA    RF,THISKEY                                                       
         CLI   THISKEY,PMCODEQU    -Q- RECORD?                                  
         BNE   CNV22               NO, IT'S A P-REC                             
         USING PMKEY,RF            BUILD  PROGRAM KEY -Q-                       
         MVC   PMBTYP,THISBOOK                                                  
         B     QCNV6                                                            
         DROP  RF                                                               
*                                                                               
         USING PRKEY,RF            BUILD USAGE RECD KEY -P-                     
CNV22    MVC   PRBTYP,THISBOOK                                                  
         B     PCNV6                                                            
         DROP  RF                                                               
*                                                                               
CNV25    AR    RE,RF                                                            
         ST    RE,DBAQUART         AND SAVE AS A(QTR HR)                        
         XC    0(60,RE),0(RE)                                                   
         OI    PRINTSW,X'80'       SUPPRESS PRINTING                            
*                                                                               
         LA    R6,TEMP                                                          
         XC    TEMP,TEMP           BUILD SECTION LEAD ELEM                      
         USING SLELEM,R6                                                        
         MVI   SLCODE,SLCODEQ                                                   
         MVI   SLLEN,3                                                          
         LA    RE,INTKEY                                                        
         CLI   0(RE),PMCODEQU       Q RECORD?                                   
         BNE   *+14                                                             
         MVC   SLSECT,PMBTYP-PMKEY+3(RE)   MKT NUMBER ON -Q- RECD               
         B     *+10                                                             
         MVC   SLSECT,PRBTYP-PRKEY+4(RE)   MKT NUMBER ON -P- RECD               
         GOTO1 APUTEL,SLELEM                                                    
         B     BLDEL27             BUILD DEMOS ELEMENTS                         
         EJECT                                                                  
* ********************************************************************          
* CNV30- LAST TIME HOOK TO RELEASE LAST RECORD                                  
* ********************************************************************          
CNV30    L     R1,ACOMWRK          ADDRESS OF COMMON WORK AREA                  
         MVC   AVGWKS,0(R1)        NMBER OF WKS IN AVG                          
         MVC   STDATE,1(R1)        START DATE OF AVG RECD                       
         OC    PREVKEY,PREVKEY                                                  
         BZ    CNV50                                                            
         CLC   PREVKEY(8),=C'PWNHUT N'  SAVE WEEKLY HUTS IN BUFFER              
         BE    CNV45                                                            
         CLC   PREVKEY(8),=C'PNNHUT N'                                          
         BNE   CNV46                                                            
         CLI   AVGWKS,4            FOR SPECIAL TIME PERD TAPE, SAVE             
         BL    CNV46               4/5 WK AVG HUT DATA IN WKLY YY00 REC         
CNV45    BAS   RE,SVHUT                                                         
CNV46    BAS   RE,PUTTAPE                                                       
*                                                                               
*--LAST TIME HOOK TO REL YEARLY HUT SUMMARY RECORDS FROM PUTBUFF                
CNV50    DS    0H                                                               
         OC    BOOKLIST(2),BOOKLIST   ANY HUT BOOKS SAVED?                      
         BZ    CNV390                 NO-(NO TIME PRD RECDS PROCESSED)          
*                                                                               
CNV51    XC    HUTVALS(HUTVALLN),HUTVALS                                        
         CLI   HUTPRINT,C'S'                                                    
         BE    CNV320                                                           
         OI    PRINTSW,X'80'                                                    
*                                                                               
CNV320   ZIC   R2,HUTHALF          GET HALF HOUR VALUE                          
         SLL   R2,1                CONV. TO QH                                  
         STC   R2,DUB              CONV. QH TO MIL TIME                         
         GOTO1 VQHTOHR,DMCB,DUB,HUTTIME                                         
         SR    R1,R1               ADD 1 MINUTE TO START TIME                   
         ICM   R1,3,HUTTIME                                                     
         LA    R1,1(R1)                                                         
         STCM  R1,3,HUTTIME+2                                                   
*                                                                               
         LA    R2,DAYS             R2=COUNTER OF DAYS                           
         LA    R3,DAYTAB           R3=A(DAY TABLE)                              
         MVI   CHGYR,C'N'          INIT CHANGE IN YEAR FLAG                     
         LA    RE,BOOKLIST                                                      
         ST    RE,SVBKLST          1ST BOOK IN LIST                             
CNV340   MVC   HUTDW(2),0(R3)                                                   
         BAS   RE,GETSUM           GET SUMMARY RECORD                           
         BAS   RE,UPSUM            UPDATE IT WITH 1/2 HR HUT                    
         CLI   SORTSW,0                                                         
         BNE   CNV350                                                           
         GOTO1 VSORTER,DMCB,SORTCARD,RECCARD,0                                  
         MVI   SORTSW,1                                                         
*                                                                               
CNV350   GOTO1 VSORTER,DMCB,=C'PUT',AOREC                                       
         CLI   CHGYR,C'Y'          DOES BOOKLIST HAVE DIFF YEARS?               
         BE    CNV340              YES, OUTPUT ANOTHER RECD W/DIFF YEAR         
         LA    R3,L'DAYTAB(R3)     NEXT DAY                                     
         BCT   R2,CNV340                                                        
*                                                                               
         ZIC   R1,HUTHALF          INCREMENT HALF HOUR                          
         LA    R1,1(R1)                                                         
         CH    R1,=H'48'           TEST IF ALL HALF-HOURS DONE                  
         BNL   CNV380              YES                                          
         STC   R1,HUTHALF                                                       
         B     CNV320              DO NEXT HALF-HOUR                            
*                                                                               
CNV380   GOTO1 VSORTER,DMCB,=C'GET'                                             
         ICM   RE,15,4(R1)         RE=A(SORT RECORD)                            
         BZ    CNV390              EOF                                          
         LH    R1,0(RE)                                                         
         L     RF,AOREC                                                         
         MOVE  ((RF),(R1)),(RE)                                                 
         BAS   RE,PUTTAPE          PUT YEARLY HUT RECORD                        
         B     CNV380                                                           
*                                                                               
CNV390   DS    0H                  RELEASE NEW BITMAP FOR NETWORK               
         GOTO1 VNTIPRG,DMCB,=C'WRTM',(0,VBITMAP1),0                             
         DS    0H                  RELEASE NEW BITMAP FOR SYN                   
         GOTO1 VNTIPRG,DMCB,=C'WRTM',(0,VBITMAP2),0                             
         DS    0H                  GENERATE J-RECD PTRS                         
         GOTO1 VNTIPRG,DMCB,=C'JREC',0,0                                        
         MVI   SORTSW,0                                                         
*                                                                               
         TM    FLAGS1,CREATE_NETCTREC   RELEASE CONTROL RECORDS                 
         BNO   CNV410                                                           
         XC    DBUPARMS,DBUPARMS        NETCTREC=Y                              
         LA    RE,DBUPARMS                                                      
         USING DBUPARMD,RE                                                      
         MVC   DBUACOMS,ACOMFACS                                                
         MVI   DBURTYP,DBURNETQ                                                 
         DROP  RE                                                               
         GOTO1 =V(DEBKUPDT),DMCB,DBUPARMS,RELQ   RELEASE                        
*                                                                               
CNV410   B     CNVX                                                             
         EJECT                                                                  
* ********************************************************************          
* SUB-ROUTINE TO READ HUT SUMMARY RECORDS FROM FILE. IF A HUT SUMMARY           
* RECORD DOES NOT EXIST FOR DAY/HALF-HOUR, IT WILL BE BUILT BY ROUTINE          
*                                                                               
* ON ENTRY - HUTTIME =MILITARY START/END TIME FOR HALF-HOUR                     
*            HUTDW   =DAY CODE                                                  
*            HUTDAY  =DAY BITS                                                  
*            HUTHALF =HALF HOUR (0-47)                                          
* ********************************************************************          
GETSUM   NTR1                                                                   
         LA    R5,DBLOCKA                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'NTI'                                                   
         L     RE,AOREC                                                         
         LA    RE,4(RE)                                                         
         ST    RE,DBAREC                                                        
         ST    RA,DBCOMFCS                                                      
         MVC   DBSELMED,MEDIA                                                   
         MVC   DBSELSRC,OUTSRC                                                  
         MVC   DBSELAGY,=C'11'                                                  
         MVC   DBSELSTA,=C'HUT N'                                               
         L     R1,SVBKLST          PTS TO BOOK IN BOOKLIST                      
         MVC   DBSELBK(1),0(R1)      EXTRACT YEAR TO UPDATE                     
         MVC   DBSELDAY,HUTDAY                                                  
         MVC   DBSELTIM,HUTTIME                                                 
         MVI   DBFUNCT,DBGETDEM                                                 
         GOTO1 CDEMAND,DMCB,DBLOCK,0                                            
         OC    DBDIVSOR,DBDIVSOR   TEST IF RECORD FOUND                         
         BZ    GETSUM2             NO                                           
         CLI   DBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RE,DBAREC           SET SORT RECORD LEN AT AOREC                 
         L     RF,AOREC                                                         
         LH    R1,PRRLEN-PRKEY(RE) GET REC LEN                                  
         LA    R1,4(R1)            + REC HEADER LEN                             
         XC    0(4,RF),0(RF)                                                    
         STH   R1,0(RF)                                                         
         B     GETSUMX                                                          
*                                                                               
GETSUM2  L     R1,SVBKLST          PTS TO BOOK IN BOOKLIST                      
         CLI   1(R1),1             TEST FOR FIRST WEEK IN YEAR                  
         BE    *+6                                                              
         DC    H'0'                NO-CANNOT BE MISSING RECORD                  
*                                                                               
         EJECT                                                                  
GETSUM4  LA    R6,THISKEY          BUILD A KEY-INITIALIZE RECORD                
         XC    THISKEY,THISKEY                                                  
         USING PRKEY,R6                                                         
         MVI   PRCODE,PRCODEQU                                                  
         MVC   PRMEDIA,MEDIA       YEARLY HUT SUMARY OUTPUT WITH 'N'            
         MVC   PRSRC,OUTSRC                                                     
         MVC   PRSTAT,=C'HUT N'                                                 
         L     R1,SVBKLST          PTS TO BOOK IN BOOKLIST                      
         MVC   PRBOOK(1),0(R1)     EXTRACT YEAR                                 
         ZIC   R1,HUTHALF                                                       
         SLL   R1,1                                                             
         STC   R1,PRSTIM                                                        
         MVC   PRDW,HUTDW                                                       
         GOTO1 ABLDREC,DMCB,PRKEY                                               
*                                                                               
         LA    R6,TEMP                                                          
         XC    TEMP,TEMP                                                        
         USING PHELEM,R6                                                        
         MVI   PHCODE,PHCODEQ      BUILD QHR ELEMENT                            
         MVI   PHELN,PHELNEQ                                                    
         MVI   PHDUR,2             HALF-HOUR                                    
         OI    PHDWKS,X'0F'                                                     
         L     R1,SVBKLST          PTS TO BOOK IN BOOKLIST                      
         MVC   PHDBOOK(1),0(R1)                                                 
         GOTO1 APUTEL,PHELEM                                                    
*                                                                               
         XC    TEMP,TEMP                                                        
         MVI   0(R6),X'90'         ADD SKELETAL HUT ELEMENT                     
         MVI   1(R6),108           53 WEEKS @ 2 BYTES EACH +LN+CODE             
         GOTO1 APUTEL,TEMP                                                      
*                                                                               
GETSUMX  B     CNVX                                                             
         DROP  R6                                                               
         EJECT                                                                  
* *******************************************************************           
* SUB-ROUTINE TO GET HUT VALUE FOR DAY/HALF HOUR AND TO UPDATE                  
* SUMMARY RECORD                                                                
* *******************************************************************           
UPSUM    NTR1                                                                   
         LA    R5,DBLOCKA          RE-ESTABLISH DBLOCK ADDRESSABILITY           
         GOTO1 CHELLO,DMCB,(C'G',PAVFIL),(X'90',DBAREC),0                       
         ZIC   R2,NBOOKS           R2=COUNTER                                   
         LA    R3,BOOKLIST                                                      
         CLI   12(R1),0            MUST FIND HUT ELEMENT                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AHUTEL,12(R1)       SAVE A(HUT ELEMENT)                          
         SR    R1,R1               CLEAR BOOK INDEX FOR GETBUFF                 
         CLI   CHGYR,C'Y'                                                       
         BNE   UPSUM1                                                           
         ZIC   R1,SVINDX           SAVED R1 INDEX INTO BUFFER                   
         L     R3,SVBKLST          SAVED PTR INTO BOOKLIST                      
         ZIC   R2,SVNBKS           SAVED NBOOKS COUNTER                         
         MVI   CHGYR,C'N'          RESET YEAR AND ASSOC VARS                    
         LA    R0,BOOKLIST                                                      
         ST    R0,SVBKLST                                                       
*                                                                               
UPSUM1   MVC   LASTYR,0(R3)        SAVE YEAR OF LAST BOOK                       
UPSUM2   CLC   LASTYR,0(R3)        SAME YEAR?                                   
         BNE   UPSUM5                                                           
         BAS   RE,GETBUFF          GET HUT ENTRY FROM BUFFER                    
         L     RF,DUB              A(HPT ENTRY)                                 
         L     RE,AHUTEL           HUT ELEMENT IN RECD                          
         LA    RE,2(RE)            POINT TO FIRST HUT VALUE                     
         ZIC   R0,1(R3)            GET WEEK NUMBER FROM BOOK                    
         BCTR  R0,0                                                             
         SLL   R0,1                INDEX INTO HUT VALUE LIST                    
         AR    RE,R0                                                            
         STCM  RF,3,0(RE)          SLOT VALUE INTO POSITION IN RECD             
         LA    R1,1(R1)            INCREMENT BOOK INDEX                         
         LA    R3,L'INTBOOK(R3)    NEXT BOOK                                    
         BCT   R2,UPSUM2                                                        
         B     UPSUMX                                                           
*                                                                               
UPSUM5   MVI   CHGYR,C'Y'          DIFFERENT YEAR IN BOOKLIST                   
         STC   R1,SVINDX           SAVE R1 INDEX INTO BUFFER                    
         ST    R3,SVBKLST          SAVE PTR TO BOOKLIST ENTRY                   
         STC   R2,SVNBKS           SAVE NBOOKS COUNTER                          
*                                                                               
UPSUMX   B     CNVX                                                             
         EJECT                                                                  
* ********************************************************************          
* SUB-ROUTINE TO GET AN ENTRY FROM THE HUT BUFFER                               
*                                                                               
* ON ENTRY, R1      = BOOK INDEX (RELATIVE TO ZERO)                             
*           R4      = DAY CODE (HUTDW)                                          
*           HUTHALF = HALF HOUR NUMBER                                          
* ON EXIT, DUB CONTAINS ENTRY                                                   
* ********************************************************************          
GETBUFF  NTR1                                                                   
         ZIC   R4,HUTDW            LOAD DAY CODE                                
         SRL   R4,4                SHIFT IT INTO LOWER NIBBLE                   
         MH    R4,DAYLEN           GET DAY DISP INTO BUFFER                     
         MH    R1,BOOKLEN          GET BOOK DISP INTO BUFFER                    
         LA    R1,0(R4,R1)         R1=BOOK AND DAY DISP                         
         A     R1,VPUTBUFF         ADD BUFF START ADDR TO DISPMNT               
         ZIC   RE,HUTHALF          1/2HR CODE                                   
         MH    RE,=H'5'            5 BYTE HUT BUCKETS                           
         LA    R1,0(RE,R1)         R1=PTS TO HUT BUCKET                         
         ICM   RF,15,1(R1)         RE=HUT VALUE                                 
         CLI   0(R1),2             IS HUT BUCKET WEIGHT < 2 ?                   
         BL    GETBUFFX            YES, JUST SAVE, DON'T DIVIDE                 
         ZIC   R4,0(R1)            R4=HUT WEIGHT (#DAYS SUMMED OVER)            
         MH    RF,=H'10'           *10 FOR ROUNDING                             
         SR    RE,RE                                                            
         DR    RE,R4               DIVIDE SUM BY # DAYS IN AVG                  
         AH    RF,=H'5'            ROUNDING                                     
         SR    RE,RE                                                            
         D     RE,=F'10'                                                        
*                                                                               
GETBUFFX ST    RF,DUB              SAVE HUT IN DUB                              
         B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
*PUTTAPE: ROUTINE TO PUT RECORD TO OUTPUT TAPE                                  
*         THIS ROUTINE ALSO CALLS DEBKUPDT TO ADD A BOOK FOR EVERY              
*         ACTIVE RECORD RELEASED TO TAPE.                                       
**********************************************************************          
*                                                                               
PUTTAPE  NTR1                                                                   
*                                                                               
         GOTO1 APUTTAPE                                                         
*                                                                               
         TM    FLAGS1,CREATE_NETCTREC   NETCTREC=Y                              
         BNO   PUTTAPEX                                                         
*                                                                               
* CALL DEBKUPDT FOR EACH P AND Q REC CREATED. DEBKUPDT KEEPS TRACK OF           
* THE BOOKS BEING LOADED AND WILL END UP CREATING CONTROL RECORDS WHEN          
* CALLED WITH RELEASE MODE.                                                     
*                                                                               
PUTTP05  XC    DBUPARMS,DBUPARMS                                                
         LA    R3,DBUPARMS                                                      
         USING DBUPARMD,R3                                                      
*                                                                               
         MVI   DBUPSTYP,DBUPNETQ   POSTING TYPE. DEFAULT IS NETWORK             
*                                                                               
         L     RE,AOREC                                                         
         LA    RE,4(RE)            POINT TO DEMO RECORD TO BE RELEASED          
         USING PRKEY,RE                                                         
         CLI   PRCODE,PRCODEQU     P RECORDS                                    
         BNE   PUTTP10                                                          
         MVC   DBUBOOK,PRBOOK      BOOK FOR P RECORDS                           
         CLI   PRSTAT+4,C'M'                                                    
         BNE   *+8                                                              
         MVI   DBUPSTYP,DBUPSYNQ   POSTING TYPE FOR SYNDICATION                 
         B     PUTTP20                                                          
         DROP  RE                                                               
*                                                                               
         USING PMKEY,RE                                                         
PUTTP10  CLI   PMCODE,PMCODEQU     Q RECORDS                                    
         BNE   PUTTAPEX            EXIT FOR ALL OTHER RECORD TYPES              
         MVC   DBUBOOK,PMBOOK      BOOK FOR Q RECORDS                           
         CLI   PMSTAT+4,C'M'                                                    
         BNE   *+8                                                              
         MVI   DBUPSTYP,DBUPSYNQ   POSTING TYPE FOR SYNDICATION                 
         DROP  RE                                                               
*                                                                               
PUTTP20  CLI   DBUBOOK+1,0         SKIP YEARLY HUTS                             
         BE    PUTTAPEX                                                         
         MVC   DBUACOMS,ACOMFACS                                                
         MVI   DBURTYP,DBURNETQ                                                 
         MVI   DBUBKFLG,DBUBKWQ    WEEKLY BOOK                                  
         DROP  R3                                                               
*                                                                               
         GOTO1 =V(DEBKUPDT),DMCB,DBUPARMS,ADDQ    ADD DEMO UPDATE BOOK          
*                                                                               
PUTTAPEX B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------------------            
* LITERALS, VARIABLES, TABLES, ETC.                                             
*-------------------------------------------------------------------            
         LTORG                                                                  
         SPACE 2                                                                
*                                  THIS TIME VALUES                             
THISKEY  DC    XL20'00'                                                         
THISSQH  DC    X'00'                                                            
THISDW   DC    X'00'                                                            
THISBOOK DC    X'00'                                                            
THISDBIT DS    X'00'                                                            
*                                  LAST TIME VALUES                             
PREVKEY  DC    XL20'00'                                                         
PREVSQH  DC    X'00'                                                            
PREVDW   DC    X'00'                                                            
PREVDBIT DC    X'00'                                                            
PRVMKT   DC    X'00'               PREV MKT BREAK                               
BOOKSW   DC    X'00'                                                            
SVUNIV   DC    X'00'                                                            
UNIVHUT  DC    F'00'                                                            
*                                                                               
CHGYR    DC    X'00'                                                            
SVINDX   DC    X'00'                                                            
LASTYR   DC    X'00'                                                            
SVNBKS   DS    X'00'                                                            
SVBKLST  DS    A                                                                
TEST     DC    X'00'                                                            
*                                                                               
FRSTREC  DC    AL1(YES)            FIRST RECORD TO OPHASE                       
*                                                                               
AVGWKS   DS    X                                                                
STDATE   DS    CL6                                                              
*                                                                               
*                                                                               
OFORMAT  DS    0CL10                                                            
         DC    C'NADNNNN',3X'00'                                                
*                                                                               
DEMLIST  DC    X'01',C'Y',X'01',X'01',C'K',X'01',X'FF'                          
PAVFIL   DC    C'PAVFIL'           HELLO FILE NAME                              
SORTCARD DC    C'SORT FIELDS=(5,20,A),FORMAT=BI,WORK=1 '                        
RECCARD  DC    C'RECORD TYPE=V,LENGTH=(1500,,,,) '                              
*                                                                               
         DS    0F                                                               
HUTBUFF  DC    A(BOOKS*DAYS*48*5)  LENGTH OF HUT BUFFER                         
BOOKLEN  DC    Y(DAYS*48*5)        DISP TO NEXT BOOK (WEEK) IN HUTBUF           
DAYLEN   DC    Y(48*5)             DISP TO NEXT DAY CATEGORY IN HUTBUF          
*                                                                               
*                                                                               
HUTVALS  DS    0C                                                               
HUTHALF  DC    X'00'                                                            
HUTTIME  DC    XL4'00'                                                          
HUTDW    DC    X'00'                                                            
HUTDAY   DC    X'00'                                                            
HUTVALLN EQU   *-HUTVALS                                                        
*                                                                               
SVBOOK   DC    X'00'                                                            
NBOOKS   DC    X'00'                                                            
BOOKLIST DC    (BOOKS*L'INTBOOK)X'00'                                           
         SPACE 2                                                                
**********************************************                                  
* TABLE OF DAY CODES AND THEIR BIT CODES                                        
**********************************************                                  
DAYTAB   DS    0CL2                                                             
         DC    X'10',X'40'         MON                                          
         DC    X'20',X'20'         TUE                                          
         DC    X'30',X'10'         WED                                          
         DC    X'40',X'08'         THU                                          
         DC    X'50',X'04'         FRI                                          
         DC    X'60',X'02'         SAT                                          
         DC    X'70',X'01'         SUN                                          
         DC    X'80',X'7F'         M-SUN                                        
         DC    X'00',X'7C'         M-F                                          
DAYS     EQU   (*-DAYTAB)/L'DAYTAB                                              
         SPACE 2                                                                
**********************************************                                  
*NETMKTS -     PRG 1/2HRS AVAIL FOR THESE NETS                                  
**********************************************                                  
NETMKTS  DS    0CL3                                                             
         DC    CL3'ABC'                                                         
         DC    CL3'CBS'                                                         
         DC    CL3'NBC'                                                         
         DC    CL3'FOX'                                                         
         DC    CL3'TEL'                                                         
         DC    CL3'UNI'                                                         
         DC    X'FF'                                                            
NETMKTQ  EQU   (*-NETMKTS)/L'NETMKTS                                            
         SPACE 2                                                                
* EQUATES                                                                       
*                                                                               
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
BOOKS    EQU   5                   UP TO 5 WKS MAY COME IN                      
         EJECT                                                                  
*-------------------------------------------------------------------            
* BUILD THE DATA TYPE ELEMENT                                                   
* USING INTKSRC, X'08' ELEMENT PASSED BACK IN TEMP                              
*-------------------------------------------------------------------            
BLDDT    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R6,TEMP                                                          
         XC    TEMP,TEMP                                                        
         USING DTELEM,R6           BUILD DEMO DATA TYPE ELEMENT                 
         MVI   DTCODE,DTCODEQ      X'08'                                        
*                                                                               
         ICM   RF,15,CDEMTABS                                                   
         GOTO1 (RF),DMCB,VWDESCTB                                               
         ICM   R1,15,DMCB                                                       
*                                                                               
BDT10    CLC   INTKSRC,0(R1)                                                    
         BE    BDT20                                                            
         CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         LA    R1,VWDESCL(R1)                                                   
         B     BDT10                                                            
*                                                                               
BDT20    ZIC   RE,1(R1)                                                         
         SHI   RE,1                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   DTTYPE(0),2(R1)                                                  
         AHI   RE,DTLENQ+1         DESC LENGTH + ELEM LEN + 1 FOR EX            
         STC   RE,DTLEN                                                         
         DROP  R6                                                               
*                                                                               
BDTX     XIT1                                                                   
         EJECT                                                                  
* DSECT TO COVER TEMP W/S                                                       
*                                                                               
NADWRKD  DSECT                                                                  
SAVEREG  DS    A                                                                
AHUTEL   DS    A                                                                
*                                                                               
*                                                                               
CONDWORK DS    2000C                                                            
*                                                                               
DBUPARMS DS    XL(DBUPARML)        PARAMETER AREA FOR DEBKUPDT                  
*                                                                               
NADWRKX  EQU   *                                                                
         SPACE 2                                                                
       ++INCLUDE DEINTD                                                         
       ++INCLUDE DEINTNT3DL                                                     
         SPACE 2                                                                
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
         SPACE 2                                                                
* DEDBLOCK                                                                      
         PRINT OFF                                                              
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
         PRINT ON                                                               
         SPACE 2                                                                
* DEDEMFILE                                                                     
       ++INCLUDE DEDEMFILE                                                      
         SPACE 2                                                                
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 2                                                                
* DEDEMCNVD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DEDEMCNVD                                                      
         PRINT ON                                                               
         SPACE 2                                                                
* DEDEMTABD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DEDEMTABD                                                      
       ++INCLUDE DDMONYREQU                                                     
       ++INCLUDE DBUPARMSD                                                      
         PRINT ON                                                               
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'041DENW09O   04/17/09'                                      
         END                                                                    
