*          DATA SET ACREPZ902  AT LEVEL 044 AS OF 05/01/02                      
*PHASE ACZP02A                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE SORTER                                                                 
*INCLUDE ADDAY                                                                  
*INCLUDE ACSLRY                                                                 
*INCLUDE XSORT                                                                  
*--------------------------------------------------------------------*          
*    ************** DO NOT DELETE THIS MODULE ****************                  
*    REWRITE - US VERSION                                                       
*                                                                               
* 1) CREATE NEW PERSON RECORDS (RECORD TYPE '0F') FROM EXISTING                 
*    1R FILE - TWO ADJUSTMENTS YOU MUST MAKE BEFORE USING -                     
*                                                                               
*    A) DSECT FOR THE SORTRECORD MUST BE ADJUSTED TO CORRESPOND WITH            
*       THE COMPANIES 1R LEDGER STRUCTURE  - THE FOUR FIELDS ARE                
*         1)  SRTOFF                                                            
*         2)  SRTDEP                                                            
*         3)  SRTSUB                                                            
*         4)  SRTEMP                                                            
*                                                                               
*    B) THE SORTCARD FOR SORTER MUST BE ADJUSTED ACCORDINGLY SO THAT            
*       THE PRIMARY SORT IS THE DISPLACEMENT TO, AND LENGTH OF, SRTEMP          
*                                                                               
*                                                                               
* 2) CREATE NEW SALARY HISTORY RECORDS (RECORD TYEP '3E05') FROM                
*    EXISTING 1R FILE USING INFORMATION FROM '52' ELEMENTS                      
*                                                                               
*--------------------------------------------------------------------*          
         TITLE 'CREATE PERSON/SALARY RECORDS'                                   
ACZP02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACZP**,R9                                                    
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACZPD,RC                                                         
         L     R8,VBIGPRNT                                                      
         USING BIGPRNTD,R8                                                      
         EJECT                                                                  
*------------------------------------------------------------------*            
*        REPORT INITIALIZATION                                                  
*------------------------------------------------------------------*            
RQF10    DS    0H                                                               
         CLI   MODE,REQFRST                                                     
         BNE   PACC100                                                          
         L     R2,=A(IO1)                                                       
         ST    R2,AIO1                                                          
         L     R2,=A(IO2)                                                       
         ST    R2,AIO2                                                          
         L     R2,=A(IO3)                                                       
         ST    R2,AIO3                                                          
         L     R2,=A(IO4)                                                       
         ST    R2,AIO4                                                          
         L     R2,=A(EMPTAB)                                                    
         ST    R2,AEMPTAB                                                       
         L     R2,=A(MYXP)                                                      
         ST    R2,AMYXP                                                         
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         ZAP   EMPTCNT,=P'0'        COUNT FOR ENTRIES IN EMP TABLE              
                                                                                
         L     R2,AMYXP                                                         
         LA    R3,30                                                            
RQF20    MVC   0(198,R2),XSPACES                                                
         LA    R2,198(R2)                                                       
         BCT   R3,RQF20                                                         
                                                                                
         LA    R1,SORTLEN           RECORD LENGTH FOR TIME DETAILS              
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  RECCARD+22(4),DUB+5(3)                                           
         LA    R1,SRTEMPL+1         DISP. TO KEY FOR TIME RECORD                
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SORTCARD+19(2),DUB+6(2)                                          
         LA    R1,L'SRTEMP          LENGTH OF KEY FOR TIME RECORD               
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SORTCARD+22(2),DUB+6(2)                                          
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD                                     
                                                                                
         CLI   QOPT1,C'T'           AM I CREATING TAPE AS OUTPUT                
         BNE   RQF100                                                           
         OPEN  (TAPEOUT,OUTPUT)                                                 
                                                                                
RQF100   B     EXIT                                                             
         EJECT                                                                  
*------------------------------------------------------------------*            
*        SEND OUT ALL SORT RECORDS                                              
*------------------------------------------------------------------*            
PACC100  CLI   MODE,PROCACC                                                     
         BNE   RQL100                                                           
         USING ACTRECD,R2                                                       
         L     R2,ADACC                                                         
         CLC   SOFFLEN(L'SRTOFF,R2),NINES         EXCLUDE ALL                   
         BE    EXIT                               OVERHEAD ACCOUNTS             
         CLC   SDEPLEN(L'SRTDEP,R2),NINES                                       
         BE    EXIT                                                             
         CLC   SEMPLEN(3,R2),NINES                                              
         BE    EXIT                                                             
                                                                                
         SR    R5,R5                                                            
         L     R2,ADACC              MOVE RECORD TO SORTAREA AND                
         ICM   R3,3,ACTRLEN                                                     
         LA    R4,SORTREC                                                       
         MVC   0(1,R4),PERRECS       TYPE ONE RECORDS ARE FOR                   
         LA    R4,1(R4)              PERSON CREATION                            
         LA    R5,SORTLEN                                                       
         MVCL  R4,R2                                                            
         BAS   RE,PUTSORT                                                       
                                                                                
         CLI   QOPT2,C'Y'            AND I GOING TO CREATE SALARY               
         BNE   EXIT                  HISTORY RECORDS                            
         LA    R4,SORTREC                                                       
         MVC   0(1,R4),SALRECS       TYPE TWO RECORDS ARE FOR SALARY            
         BAS   RE,PUTSORT                                                       
         B     EXIT                                                             
         EJECT                                                                  
*------------------------------------------------------------------*            
*        GET RECORDS FROM SORTER AND PRINT REPORT                               
*------------------------------------------------------------------*            
RQL100   CLI   MODE,REQLAST                                                     
         BNE   EXIT                                                             
         MVI   CNTRLSW,0             MARK FIRST TIME                            
         MVI   ERRSW,0               INITIALIZE ERROR SWITCH                    
                                                                                
RQL120   BAS   RE,GETSORT                                                       
         OC    ASORT,ASORT           END OF SORT FILE                           
         BZ    RQL900                                                           
         CLC   SORTTYPE(1),PERRECS   TYPE ONE IS TO CREATE PERSON               
         BNE   RQL130                RECORDS                                    
         BAS   RE,PERMAIN                                                       
         B     RQL120                                                           
                                                                                
RQL130   CLC   SORTSAVE(1),PERRECS   IS THIS FIRST SALARY HISTORY               
         BNE   *+8                   RECORD - THEN PROCESS FINAL                
         BAS   RE,PERMAIN            PERSON RECORD                              
         BAS   RE,SALMAIN            THEN PROCESS FIRST SALARY HISTORY          
         B     RQL120                THEN GET NEXT SORT RECORD                  
                                                                                
RQL900   DS    0H                                                               
         GOTO1 SORTER,DMCB,=C'END'   CLOSE SORT                                 
         CLI   QOPT2,C'Y'                                                       
         BNE   RQL910                                                           
         USING PRNTD,R2                                                         
         LA    R2,XP                                                            
         MVC   PCHKDAT(16),=C'**REPORT TOTAL**'                                 
         EDIT  (P10,REPSTOT),(14,PSALAMT),2                                     
         GOTO1 ACREPORT                                                         
         ZAP   PERSTOT,=P'0'                                                    
         ZAP   REPSTOT,=P'0'                                                    
         B     RQL915                                                           
                                                                                
RQL910   MVC   SORTTYPE(1),SALRECS        FORCE PROCESSING OF LAST              
         BAS   RE,PERMAIN                 PERSON                                
RQL915   CLI   QOPT1,C'T'                                                       
         BNE   EXIT                                                             
         CLOSE (TAPEOUT)                                                        
         B     EXIT                                                             
         EJECT                                                                  
*------------------------------------------------------------------*            
*        BUILD LAST PERSON RECORD AND PRINT                                     
*------------------------------------------------------------------*            
PERMAIN  NTR1                                                                   
         CLC   SORTTYPE(1),SALRECS   AM I PROCESSING LAST PERSON                
         BE    PERM52                                                           
         TM    CNTRLSW,NOTFRST       FIRST TIME SKIP STRAIGHT TO                
         BZ    PERM50                ELEMENT BUILDING                           
         CLC   SRTEMP,SAVEEMP        IS IT SAME EMPLOYEE NUMBER AS              
         BNE   PERM50                LAST RECORD - IF YES COMPARE NAME          
         BAS   RE,COMPNAME           FROM 20 ELEMENT                            
         TM    ERRSW,DUPNUM          IF DIFFERENT                               
         BNO   PERM20                KICK OUT AS ERROR                          
                                                                                
         USING PRNTD,R2              PRINT ERROR MSG AND GO GET NEXT            
         LA    R2,XP                 SORTRECORD                                 
         CLC   SORTSAV2,SORTSAVE                                                
         BE    PERM10                                                           
         MVC   PACTNEW(11),=C'** ERROR **'                                      
         MVC   PACTOLD,SORTSAVE+2                                               
         MVC   PLNAME(L'SAVE20-2),SAVE20+2                                      
         MVC   PACTION(11),=C'** ERROR **'                                      
         LA    R2,XPSECOND                                                      
PERM10   MVC   PACTNEW(11),=C'** ERROR **'                                      
         MVC   PACTOLD,SORTUNIT                                                 
         MVC   PLNAME(L'CURR20-2),CURR20+2                                      
         MVC   PACTION(11),=C'** ERROR **'                                      
         MVI   SPACING,2                                                        
         GOTO1 ACREPORT                                                         
         NI    ERRSW,X'FF'-DUPNUM                                               
         MVC   SORTSAV2,SORTKEY                                                 
         B     PERMXIT                                                          
                                                                                
PERM20   BAS   RE,BLDHIS             KEEP MOST RECENT 56 EL TO ADD              
         BAS   RE,BLDSTFL            IF SAME ADD ANOTHER 83 ELEMENT AND         
         B     PERMXIT               GET NEXT SORTREC                           
                                                                                
PERM50   MVC   SAVEEMP,SRTEMP        SAVE THIS EMPLOYEE NUMBER                  
         TM    CNTRLSW,NOTFRST       IS FIRST TIME - NOTHING TO ADD             
         BZ    PERM70                                                           
         TM    ERRSW,NOADD                                                      
         BNO   PERM52                                                           
         NI    ERRSW,X'FF'-NOADD                                                
         B     PERM70                                                           
                                                                                
PERM52   BAS   RE,BLDLEND                                                       
         TM    ERRSW,OVERLAP                                                    
         BNO   PERM53                                                           
         NI    ERRSW,X'FF'-OVERLAP                                              
         B     PERM70                                                           
                                                                                
PERM53   DS    0H                                                               
         USING ACTRECD,R2                                                       
         GOTO1 DATAMGR,DMCB,(X'08',DMREAD),=C'ACCDIR',AIO1,AIO2                 
         TM    DMCB+8,X'10'        IF RECORD WAS NOT FOUND ADD IT               
         BO    PERM54                                                           
         L     R2,AIO2             IF FOUND SAVE OFF DISK ADDRESS               
         MVC   DSKADR,ACTKDA                                                    
         TM    DMCB+8,X'02'        IF DIRECTORY WAS MARKED DELETED              
         BZ    PERM53J             RESET DELETE BIT TO ZERO AND WRITE           
         NI    ACTKSTAT,X'FF'-ACTSDELT                                          
         CLI   RCWRITE,C'N'                                                     
         BE    PERM53J                                                          
         GOTO1 DATAMGR,DMCB,DMWRT,=C'ACCDIR',AIO2,AIO2                          
         B     PERM53J                                                          
                                                                                
PERM53J  GOTO1 DATAMGR,DMCB,=CL8'GETREC',=C'ACCMST',DSKADR,AIO2,ADWRK           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,AIO1                                                          
         MVC   ACTRSTA-ACTRECD(L'ACTRSTA,R3),ACTRSTA                            
         NI    ACTRSTAT,X'FF'-ACTSDELT                                          
         CLI   RCWRITE,C'N'                                                     
         BE    PERM53P                                                          
         GOTO1 DATAMGR,DMCB,=CL8'PUTREC',=CL8'ACCMST',DSKADR,AIO1,ADWRK         
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         USING PRNTD,R2                                                         
PERM53P  DS    0H                                                               
         LA    R2,XP                                                            
         MVC   PACTION(6),=C'UPDATE'                                            
         B     PERM60                                                           
                                                                                
PERM54   DS    0H                                                               
         CLI   RCWRITE,C'N'                                                     
         BE    PERM54A                                                          
         L     R2,AIO1                                                          
         GOTO1 DATAMGR,DMCB,=CL8'ADDREC',=CL8'ACCMST',FULL,(R2),ADWRK           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PRNTD,R2                                                         
PERM54A  DS    0H                                                               
         LA    R2,XP                                                            
         MVC   PACTION(3),=C'ADD'                                               
                                                                                
PERM60   L     R2,AMYXP                                                         
         LA    R3,30                                                            
         CLC   0(198,R2),XSPACES                                                
         BE    PERM65                                                           
         GOTO1 ACREPORT                                                         
PERM62   CLC   0(198,R2),XSPACES                                                
         BE    PERM65                                                           
         MVC   XP,0(R2)                                                         
         MVC   0(198,R2),XSPACES                                                
         GOTO1 ACREPORT                                                         
         LA    R2,198(R2)                                                       
         BCT   R3,PERM62                                                        
         B     *+8                                                              
                                                                                
PERM65   MVI   SPACING,2                                                        
         GOTO1 ACREPORT                                                         
         BAS   RE,ADDEMP             ADD THIS EMPLOYEE NUMBER TO TABLE          
                                                                                
         CLI   QOPT1,C'T'            AM I WRITING RECORDS TO TAPE               
         BNE   *+12                                                             
         L     R2,AIO1                                                          
         BAS   RE,PUTTAPE                                                       
                                                                                
PERM70   OI    CNTRLSW,NOTFRST                                                  
         CLC   SORTTYPE(1),SALRECS   AM I PROCESSING LAST PERSON                
         BE    PERMXIT               RECORD                                     
         BAS   RE,CLRIO1             CLEAR IO AREA                              
         BAS   RE,BLDKEY             BUILD NEW RECORD KEY                       
         BAS   RE,BLDHIS             SAVE MOST CURRENT 56 ELEMENT               
         BAS   RE,BLDNAM             BUILD NEW NAME ELEMENTS                    
         BAS   RE,BLDSTFL            BUILD EMPLOYEE LOCATION ELEMENT            
PERMXIT  B     EXIT                                                             
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        MAINT CONTROL FOR SALARY HISTORY CREATION                              
*-------------------------------------------------------------------*           
SALMAIN  NTR1                                                                   
         TM    CNTRLSW,FRSTSAL                                                  
         BZ    SALM10                                                           
         CLC   SRTEMP,SORTSAVE+SEMPLEN+1                                        
         BE    SALM10                                                           
**       CP    PERSTOT,=P'0'         IF LAST 1R REC HAD NO '52' ELS ON          
**       BE    SALM10                IT THEM NO SAL HIST INFO TO PRINT          
         USING PRNTD,R2                                                         
         LA    R2,XP                                                            
         MVC   PCHKDAT(16),=C'**PERSON TOTAL**'                                 
         EDIT  (P10,PERSTOT),(14,PSALAMT),2                                     
         MVI   SPACING,2                                                        
         GOTO1 ACREPORT                                                         
         AP    REPSTOT,PERSTOT                                                  
         ZAP   PERSTOT,=P'0'                                                    
SALM10   BAS   RE,SALHIST            CREATE AND PRINT SALARY HISTORY            
         B     EXIT                                                             
         EJECT                                                                  
*------------------------------------------------------------------*            
*        BUILD PERSON RECORD KEY                                                
*------------------------------------------------------------------*            
BLDKEY   NTR1                                                                   
         USING ACTRECD,R2                                                       
         LA    R2,SORTCOMP                                                      
         USING PERRECD,R3                                                       
         L     R3,AIO1                                                          
         MVC   PERKEY(L'PERKEY),SPACES                                          
         MVI   PERKTYP,PERKTYPQ                                                 
         MVC   PERKCPY,ACTKCPY                                                  
         MVC   PERKCODE(L'SRTEMP),ACTKEY+ACTKEND-L'SRTEMP                       
         MVC   PERRLEN,=X'0039'                                                 
         B     EXIT                                                             
         EJECT                                                                  
*------------------------------------------------------------------*            
*        COMPARE 56 ELEMENTS AND SAVE ONE WITH MOST RECENT HIRE                 
*        DATE - SAVE LATEST (OR NONEXISTENT) TERMINATION DATE -                 
*        ELSE DEFAULT TO BBF DATE FROM 30 ELEM                                  
*        PUT TO NEW '83' ELEMS                                                  
*------------------------------------------------------------------*            
BLDHIS   NTR1                                                                   
         XC    CURR56,CURR56                                                    
         USING PRNTD,R2                                                         
         LA    R2,XP                                                            
         USING PERRECD,R3                                                       
         L     R3,AIO1                                                          
         MVI   ELCODE,EMPELQ         GET ADDRESS OF EMP HISTORY ELEM            
         LA    R4,SORTCOMP           FROM RECORD FROM SORT                      
         BAS   RE,GETEL                                                         
         BE    BLDH050                                                          
         CLI   PERRFST,EMPELQ                                                   
         BE    BLDH100                                                          
         USING EMPELD,R5                                                        
         LA    R5,CURR56                                                        
         MVI   EMPEL,EMPELQ                                                     
         MVI   EMPLN,EMPLNQ                                                     
         B     BLDH075                                                          
                                                                                
BLDH050  MVC   CURR56,0(R4)                                                     
         CLI   PERRFST,EMPELQ                                                   
         BE    BLDH100                                                          
BLDH075  DS    0H                                                               
         LA    R5,CURR56                                                        
         GOTO1 ADDEL,DMCB,(R3),(R5)                                             
         MVC   PERRFST(L'CURR56),CURR56                                         
         GOTO1 DATCON,DMCB,(1,EMPHIR),(5,PHIRE)                                 
         GOTO1 DATCON,DMCB,(1,EMPTRM),(5,PTERM)                                 
         B     BLDHXIT                                                          
                                                                                
BLDH100  LA    R4,PERRFST                                                       
         LA    R5,CURR56                                                        
         OC    EMPTRM-EMPELD(L'EMPTRM,R4),EMPTRM-EMPELD(R4)                     
         BZ    BLDH110                                                          
         CLC   EMPHIR-EMPELD(L'EMPHIR,R4),EMPHIR-EMPELD(R5)                     
         BL    BLDH120                                                          
BLDH110  MVC   EMPHIR-EMPELD(L'EMPHIR,R4),EMPHIR-EMPELD(R5)                     
         GOTO1 DATCON,DMCB,(1,EMPHIR),(5,PHIRE)                                 
         DROP  R5                                                               
                                                                                
         USING EMPELD,R4                                                        
BLDH120  OC    EMPTRM-EMPELD(L'EMPTRM,R4),EMPTRM-EMPELD(R4)                     
         BZ    BLDHXIT                                                          
         OC    EMPTRM-EMPELD(L'EMPTRM,R5),EMPTRM-EMPELD(R5)                     
         BNZ   BLDH125                                                          
         XC    EMPTRM,EMPTRM                                                    
         MVC   PTERM,SPACES                                                     
         B     BLDHXIT                                                          
BLDH125  CLC   EMPTRM-EMPELD(L'EMPTRM,R4),EMPTRM-EMPELD(R5)                     
         BH    BLDHXIT                                                          
         MVC   EMPTRM-EMPELD(L'EMPTRM,R4),EMPTRM-EMPELD(R5)                     
         GOTO1 DATCON,DMCB,(1,EMPTRM),(5,PTERM)                                 
BLDHXIT  B     EXIT                                                             
         EJECT                                                                  
         DROP  R4                                                               
*------------------------------------------------------------------*            
*        BUILD GENERAL NAME ELEMENTS                                            
*------------------------------------------------------------------*            
BLDNAM   NTR1                                                                   
         XC    SVLNAME,SVLNAME                                                  
         XC    SVFNAME,SVFNAME                                                  
         XC    SAVE20,SAVE20                                                    
         USING PERRECD,R3                                                       
         L     R3,AIO1                                                          
         USING NAMEL,R4                                                         
         MVI   ELCODE,NAMELQ       GET ADDRESS OF OLD NAME ELEMENT              
         LA    R4,SORTCOMP         FROM RECORD FROM SORT                        
         BAS   RE,GETEL                                                         
         BNE   EXIT                                                             
         MVC   BYTE,NAMLN          STORE OLD ELEMENT LENGTH                     
         ZIC   R5,NAMLN            SAVE OFF NAME ELEMENT FOR LATER              
         BCTR  R5,0                COMPARISON                                   
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   SAVE20(0),0(R4)     SAVE FOR COMPARE ON NEXT SORTREC             
         LA    R4,2(R4)                                                         
                                                                                
         XC    BLDELS,BLDELS                                                    
         ZIC   R6,BYTE             SAVE OFF NAME ELEMENT FOR LATER              
         SH    R6,=H'2'                                                         
         LR    R5,R6               SAVE LENGTH                                  
                                                                                
         LR    RF,R4               SAVE ADDRESS OF NAME                         
         SR    R7,R7               COUNTER                                      
BLDN05   CLI   0(RF),C','          DEFINITE LAST NAME                           
         BE    BLDLNM                                                           
         LA    RF,1(RF)                                                         
         LA    R7,1(R7)                                                         
         BCT   R5,BLDN05                                                        
                                                                                
BLDN06   LR    RF,R4               SAVE ADDRESS OF NAME                         
         LR    R5,R6               SAVE LENGTH                                  
         SR    R7,R7               COUNTER                                      
BLDN07   CLI   0(RF),C' '          DEFINITE FIRST NAME                          
         BE    BLDFNM                                                           
         LA    RF,1(RF)                                                         
         LA    R7,1(R7)                                                         
         BCT   R5,BLDN07                                                        
         B     BLDLNM                                                           
                                                                                
BLDFNM   LR    RE,R4                                                            
         AR    RE,R6                                                            
         BCTR  RE,0                POINTS TO LAST CHAR IN NAME                  
         SR    R8,R8               LAST NAME COUNTER                            
BLDFNM1  CLI   0(RE),C','          FIRST SPACE BACKWARDS SEPARATE LAST          
         BE    BLDFNM3                                                          
         CLI   0(RE),C' '                                                       
         BE    BLDFNM3                                                          
         BCTR  RE,0                PREV CHAR                                    
         LA    R8,1(R8)                                                         
         B     BLDFNM1                                                          
                                                                                
BLDFNM3  STC   R8,SVLNAMEL                                                      
         BCTR  R8,0                                                             
         EX    R8,*+8                                                           
         B     *+10                                                             
         MVC   SVLNAME(0),1(RE)                                                 
         LA    R8,2(R8)            GET RID OF SPACE FOR FIRST                   
         SR    R6,R8                                                            
         STC   R6,SVFNAMEL                                                      
         BCTR  R6,0                                                             
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   SVFNAME(0),0(R4)                                                 
         B     BLDN10                                                           
                                                                                
BLDLNM   STC   R7,SVLNAMEL                                                      
         BCTR  R7,0                                                             
         EX    R7,*+8                                                           
         B     *+10                                                             
         MVC   SVLNAME(0),0(R4)    SAVE LAST NAME                               
         LTR   R5,R5                                                            
         BZ    BLDN10                                                           
         LA    RF,1(RF)            SKIP THE COMMA                               
         SR    R6,R7                                                            
         BCTR  R6,0                                                             
         CLI   0(RF),C' '          TAKE OUT SPACE AFTER COMMA                   
         BNE   *+10                                                             
         BCTR  R6,0                                                             
         LA    RF,1(RF)                                                         
                                                                                
         STC   R6,SVFNAMEL         SHOULD BE FIRST NAME                         
         BCTR  R6,0                                                             
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   SVFNAME(0),0(RF)                                                 
                                                                                
         USING GPNELD,R5           BUILD NEW NAME ELEMENTS                      
BLDN10   LA    R5,BLDELS                                                        
         MVI   GPNEL,GPNELQ        ELEMENT CODE                                 
         MVI   GPNTYP,GPNTLST      ELEMENT TYPE                                 
         ZIC   R1,SVLNAMEL         COPY LAST NAME TO ELEMENT                    
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   GPNNME(0),SVLNAME                                                
         LA    R1,GPNLNQ(R1)                                                    
         LA    R1,1(R1)                                                         
         STC   R1,GPNLN                                                         
         L     R2,AIO1                                                          
         LA    R3,BLDELS                                                        
         GOTO1 ADDEL,DMCB,(R2),(R3)                                             
                                                                                
         OC    SVFNAMEL,SVFNAMEL                                                
         BZ    BLDNXIT                                                          
                                                                                
         XC    BLDELS,BLDELS                                                    
         LA    R5,BLDELS                                                        
         MVI   GPNEL,GPNELQ          ELEMENT CODE                               
         MVI   GPNTYP,GPNTFST        ELEMENT TYPE                               
         ZIC   R1,SVFNAMEL           COPY FIRST NAME TO ELEMENT                 
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   GPNNME(0),SVFNAME                                                
         LA    R1,GPNLNQ(R1)                                                    
         LA    R1,1(R1)                                                         
         STC   R1,GPNLN                                                         
         L     R2,AIO1                                                          
         LA    R3,BLDELS                                                        
         GOTO1 ADDEL,DMCB,(R2),(R3)                                             
                                                                                
BLDNXIT  B     EXIT                                                             
         EJECT                                                                  
*------------------------------------------------------------------*            
*        BUILD STAFF LOCATION ELEMENT                                           
*------------------------------------------------------------------*            
BLDSTFL  NTR1                                                                   
         USING ACTRECD,R2                                                       
         LA    R2,SORTCOMP                                                      
         USING PERRECD,R3                                                       
         L     R3,AIO1                                                          
                                                                                
         XC    BLDELS,BLDELS                                                    
         USING LOCELD,R5                                                        
         LA    R5,BLDELS                                                        
         MVI   LOCEL,LOCELQ                                                     
         MVI   LOCLN,LOCLNQ                                                     
         MVC   LOCOFF(L'SRTOFF),SRTOFF                                          
         OC    LOCOFF,SPACES                                                    
         MVC   LOCDEPT(L'SRTDEP),SRTDEP                                         
         OC    LOCDEPT,SPACES                                                   
         MVC   LOCSUB(L'SRTSUB),SRTSUB                                          
         OC    LOCSUB,SPACES                                                    
                                                                                
         USING EMPELD,R4                                                        
         MVI   ELCODE,EMPELQ         GET ADDRESS OF OLD EMP HIST ELEM           
         LA    R4,SORTCOMP           FROM RECORD FROM SORT                      
         BAS   RE,GETEL                                                         
         BNE   BLDS25                                                           
         MVC   LOCSTART,EMPHIR                                                  
         MVC   LOCEND,EMPTRM                                                    
         MVC   LOCATTR,EMPSTAT       COPY THIS STATUS BYTE AND KEEP             
         NI    LOCATTR,X'FF'-LOCALOCK      EVERYTHING BUT LOCALOCK              
         MVC   LOCSALKD,EMPSALKD                                                
                                                                                
         USING RSTELD,R4                                                        
BLDS25   MVI   ELCODE,RSTELQ         GET ADDRESS OF OLD STATUS ELEM             
         LA    R4,SORTCOMP           FROM RECORD FROM SORT                      
         BAS   RE,GETEL                                                         
         BNE   BLDS50                                                           
         OC    LOCSTART,LOCSTART                                                
         BNZ   *+10                                                             
         MVC   LOCSTART,RSTBDATE                                                
         TM    RSTSTAT1,RSTSACIL                                                
         BZ    *+8                                                              
         OI    LOCATTR,LOCALOCK                                                 
                                                                                
BLDS50   DS    0H                                                               
         L     R2,AIO1                                                          
         LA    R3,BLDELS                                                        
         GOTO1 ADDEL,DMCB,(R2),(R3)                                             
                                                                                
BLDSXIT  B     EXIT                                                             
         EJECT                                                                  
         DROP  R5                                                               
*------------------------------------------------------------------*            
*        PUT LOCATION END INTO ALL '83' ELEMENTS                                
*        AND MOVE TO PRINT LINES                                                
*------------------------------------------------------------------*            
BLDLEND  NTR1                                                                   
         USING PRNTD,R2                                                         
         LA    R2,XP                                                            
         L     R3,AIO1                  ADDRESS OF RECORD                       
         MVC   PACTNEW,2(R3)                                                    
         MVC   PLNAME,SVLNAME                                                   
         MVC   PFNAME,SVFNAME                                                   
                                                                                
         USING ACCRECD,R3                                                       
         L     R3,AIO1                  ADDRESS OF RECORD                       
         LA    R4,ACCRFST               ADDRESS OF FIRST ELEMENT                
         MVI   ELCODE,LOCELQ            GET FIRST '83' ELEMENT                  
         BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         USING LOCELD,R6                                                        
         LR    R6,R4                    KEEP IN R4 ADDR OF FIRST '83'           
         MVI   BYTE,0                                                           
         XC    SAVLEND,SAVLEND                                                  
         MVC   SAVHIRD,LOCSTART                                                 
         GOTO1 DATCON,DMCB,(1,LOCSTART),(5,PHIRE)                               
BLDL130  GOTO1 DATCON,DMCB,(1,LOCSTART),(5,PLOCSTR)                             
         OC    LOCEND,LOCEND                                                    
         BZ    BLDL140                                                          
         GOTO1 DATCON,DMCB,(1,LOCEND),(5,PLOCEND)                               
         MVC   SAVLEND,LOCEND                                                   
BLDL140  LR    R6,R4                    KEEP IN R4 ADDR OF FIRST '83'           
         LA    R5,LOCLNQ                ELEMENT - IN R6 ADDR OF NEXT            
         AR    R6,R5                                                            
         CLI   0(R6),0                  IF END OF REC LEAVE LAST END            
         BE    BLDL180                  DATE ZEROS                              
         CLI   0(R6),LOCELQ             OR IF NO MORE '83' ELS                  
         BNE   BLDL180                                                          
         GOTO1 DATCON,DMCB,(1,LOCSTART),(0,TEMPSTR)  TAKE NEXT LOCATION         
         DROP  R6                                    DATE,                      
         SR    R7,R7                                                            
         SH    R7,=H'1'                              SUBTRACT ONE DAY,          
         GOTO1 ADDAY,DMCB,TEMPSTR,TEMPEND,(R7)                                  
         USING LOCELD,R4                              AND STORE IN END          
         OC    LOCEND,LOCEND                                                    
         BNZ   BLDL170                                                          
         GOTO1 DATCON,DMCB,(0,TEMPEND),(1,LOCEND)    DATE OF PREVIOUS           
         MVC   SAVLEND,LOCEND                                                   
         GOTO1 DATCON,DMCB,(0,TEMPEND),(5,PLOCEND)   '83' ELEM AND PUT          
BLDL170  MVC   PACTOLD(L'SRTOFF),LOCOFF                                         
         MVC   PACTOLD+L'SRTOFF(L'SRTDEP),LOCDEPT                               
         MVC   PACTOLD+L'SRTOFF+L'SRTDEP(L'SRTSUB),LOCSUB                       
         MVC   PACTOLD+L'SRTOFF+L'SRTDEP+L'SRTSUB(L'SRTEMP),2(R3)               
         MVI   LOCSTAT,3              MARK THIS LOCATION TRANSFERRED            
         BAS   RE,TSLOCK                                                        
         OC    LOCLOCK,LOCLOCK                                                  
         BZ    BLDL172                                                          
         GOTO1 DATCON,DMCB,(1,LOCLOCK),(5,PTSLOCK)                              
                                                                                
BLDL172  ZIC   R7,BYTE                                                          
         LA    R7,1(R7)                                                         
         STC   R7,BYTE                                                          
         CLI   BYTE,4                                                           
         BNE   BLDL175                                                          
         L     R2,AMYXP                                                         
         B     *+8                                                              
                                                                                
BLDL175  LA    R2,L'XP(R2)                                                      
         LR    R4,R6                    BUMP AND DO OVER FOR NEXT PAIR          
         B     BLDL130                                                          
                                                                                
BLDL180  DS    0H                                                               
         CLI   0(R4),LOCELQ                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   PACTOLD(L'SRTOFF),LOCOFF                                         
         MVC   PACTOLD+L'SRTOFF(L'SRTDEP),LOCDEPT                               
         MVC   PACTOLD+L'SRTOFF+L'SRTDEP(L'SRTSUB),LOCSUB                       
         MVC   PACTOLD+L'SRTOFF+L'SRTDEP+L'SRTSUB(L'SRTEMP),2(R3)               
         MVI   LOCSTAT,0                MARK THIS LOCATION ACTIVE               
         OC    LOCEND,LOCEND            IF AN END DATE IS PRESENT               
         BZ    *+8                      MARK THIS LOCATION TERMINATED           
         MVI   LOCSTAT,1                                                        
                                                                                
BLDL200  L     R3,AIO1                  ADDRESS OF RECORD                       
         LA    R6,ACCRFST               ADDRESS OF FIRST ELEMENT                
         CLI   0(R6),EMPELQ                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         USING EMPELD,R6                                                        
         MVC   EMPHIR,SAVHIRD                                                   
         OC    EMPTRM,EMPTRM                                                    
         BZ    BLDLXIT                                                          
         MVC   LOCEND,EMPTRM                                                    
         GOTO1 DATCON,DMCB,(1,LOCEND),(5,PLOCEND)    '83' ELEM AND PUT          
         BAS   RE,TSLOCK                                                        
         OC    LOCLOCK,LOCLOCK                                                  
         BZ    BLDLXIT                                                          
         GOTO1 DATCON,DMCB,(1,LOCLOCK),(5,PTSLOCK)                              
         DROP  R6                                    TO PRINT LINE              
         DROP  R4                                    TO PRINT LINE              
                                                                                
BLDLXIT  DS    0H                                                               
         L     R3,AIO1                  ADDRESS OF RECORD                       
         LA    R4,ACCRFST               ADDRESS OF FIRST ELEMENT                
         MVI   ELCODE,LOCELQ            GET FIRST '83' ELEMENT                  
         BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    R6,R4                                                            
BLDLX10  BAS   RE,NEXTEL                                                        
         BNE   BLDLX20                                                          
         CLC   LOCSTART-LOCEL(L'LOCSTART,R6),LOCEND-LOCEL(R6)                   
         BH    BLDLX15                                                          
         OC    LOCEND-LOCEL(L'LOCEND,R4),LOCEND-LOCEL(R4)                       
         BZ    BLDLX13                                                          
         CLC   LOCSTART-LOCEL(L'LOCSTART,R4),LOCEND-LOCEL(R4)                   
         BH    BLDLX15                                                          
BLDLX13  CLC   LOCSTART-LOCEL(L'LOCSTART,R4),LOCEND-LOCEL(R6)                   
         BH    BLDLX10                                                          
BLDLX15  DS    0H                                                               
         CLI   QOPT7,C'Y'                                                       
         BE    *+8                                                              
         OI    ERRSW,OVERLAP                                                    
         B     BLDLX10                                                          
                                                                                
BLDLX20  TM    ERRSW,OVERLAP                                                    
         BZ    EXIT                                                             
         GOTO1 ACREPORT                                                         
         LA    R2,XP                                                            
         MVC   PACTNEW(11),=C'** ERROR **'                                      
         MVC   PFNAME(26),=C'OVERLAPPING LOCATION DATES'                        
         MVC   PACTION(11),=C'** ERROR **'                                      
         MVI   SPACING,2                                                        
         GOTO1 ACREPORT                                                         
         B     EXIT                                                             
         EJECT                                                                  
*------------------------------------------------------------------*            
*        CHECK LOCATION END AGAINST TIMESHEET TOTAL HOURS RECORD                
*------------------------------------------------------------------*            
TSLOCK   NTR1                                                                   
         USING LOCELD,R4                                                        
         OC    LOCEND,LOCEND                                                    
         BZ    TSLXIT                                                           
         ST    R4,FULL                 SAVE ADDR OF LOCATION EL I'M             
                                                                                
         USING TTHRECD,R3              CURRENTLY PROCESSING                     
         L     R3,AIO4                 BUILD KEY OF TIME TOTAL HOURS            
         XC    0(56,R3),0(R3)          RECORD                                   
         MVI   TTHKTYP,TTHKTYPQ                                                 
         MVI   TTHKSUB,TTHKSUBQ                                                 
         MVC   TTHKCPY,SORTCOMP                                                 
         MVI   TTHKUNT,C'1'                                                     
         MVI   TTHKLDG,C'R'                                                     
         LA    R2,TTHKACT                                                       
         MVC   0(L'LOCOFF,R2),LOCOFF                                            
TSL10    LA    R2,1(R2)                                                         
         CLI   0(R2),C' '                                                       
         BH    TSL10                                                            
         MVC   0(L'LOCDEPT,R2),LOCDEPT                                          
TSL15    LA    R2,1(R2)                                                         
         CLI   0(R2),C' '                                                       
         BH    TSL15                                                            
         MVC   0(L'LOCSUB,R2),LOCSUB                                            
TSL20    LA    R2,1(R2)                                                         
         CLI   0(R2),C' '                                                       
         BH    TSL20                                                            
         L     R3,AIO1                                                          
         MVC   0(L'SRTEMP,R2),2(R3)                                             
         L     R3,AIO4                                                          
         XC    DIRREAD,DIRREAD                                                  
         MVC   DIRREAD,0(R3)                                                    
                                                                                
         GOTO1 DATAMGR,DMCB,DMRDHI,=C'ACCDIR',AIO4,AIO4                         
         TM    DMCB+8,X'10'                                                     
         BO    TSLXIT                                                           
         L     R3,AIO4                                                          
         CLC   DIRREAD,0(R3)        DID I FIND TIME TOTAL RECORD FOR            
         BNE   TSLXIT               THIS 1R ACCOUNT                             
         MVC   DSKADR,TTHKDA                                                    
         GOTO1 DATAMGR,DMCB,=CL8'GETREC',=C'ACCMST',DSKADR,AIO4,ADWRK           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R3,AIO4              BUILD KEY OF TIME TOTAL HOURS               
         LA    R5,TTHRFST                                                       
         LR    R4,R5                KEEP IN R5 ADDRESS OF LAST '8D'             
TSL50    ZIC   R6,1(R4)             ELEMENT FOR TIMESHEET LOCK                  
         AR    R4,R6                                                            
         CLI   0(R4),0                                                          
         BE    TSL60                                                            
         CLI   0(R4),PTHELQ                                                     
         BNE   TSL60                                                            
         LR    R5,R4                                                            
         B     TSL50                                                            
                                                                                
         USING LOCELD,R4                                                        
TSL60    L     R4,FULL                                                          
         USING PTHELD,R5                                                        
         CLC   LOCEND,PTHEDT                                                    
         BE    TSLXIT                                                           
         BH    TSLXIT                                                           
         MVC   LOCLOCK,PTHEDT                                                   
                                                                                
TSLXIT   B     EXIT                                                             
         EJECT                                                                  
*------------------------------------------------------------------*            
*        BUILD SALARY HISTORY RECORDS FROM SORTRECORD                           
*------------------------------------------------------------------*            
SALHIST  NTR1                                                                   
         MVI   RCSUBPRG,1           INITIALIZE                                  
         XC    SAVDATES,SAVDATES                                                
         TM    CNTRLSW,FRSTSAL                                                  
         BO    SALH020                                                          
         MVI   FORCEHED,C'Y'                                                    
         OI    CNTRLSW,FRSTSAL                                                  
                                                                                
         USING ACTRECD,R2                                                       
SALH020  SR    R3,R3                SORT ALL THE '52' SALARY ELS                
         LA    R4,SORTCOMP          IN DESCENDING ORDER BY DATE,                
         MVI   ELCODE,MSAELQ        THEN TYPE                                   
         BAS   RE,GETEL                                                         
         BNE   SALHXIT                                                          
         ST    R4,REG4TEMP          SAVE THIS ADDRESS FOR XSORT                 
SALH050  LA    R3,1(R3)                                                         
         BAS   RE,NEXTEL                                                        
         BE    SALH050                                                          
         L     R4,REG4TEMP          RESTORE THIS ADDRESS                        
         GOTO1 XSORT,DMCB,(0,(R4)),(R3),MSALNQ,5,8                              
                                                                                
         USING MSAELD,R4                                                        
         LA    R4,SORTCOMP                                                      
         MVI   ELCODE,MSAELQ        GET '52' ELEMENT                            
         BAS   RE,GETEL                                                         
         ST    R4,REG4TEMP          SAVE ADDR OF ELEMENT I'M PROCESSING         
SALH051  CLI   MSATYPE,X'50'                                                    
         BE    SALH052                                                          
         BAS   RE,NEXTEL                                                        
         BNE   SALHXIT                                                          
         B     SALH051                                                          
                                                                                
SALH052  BAS   RE,CLRIO3            CLEAR THE IO AREA                           
         MVC   SV52TYP,MSATYPE      SAVE ORIGINAL TYPE                          
         MVC   START,MSABEG         SET ACSLRY START AND END DATE               
         MVC   END,MSABEG           TO THIS ELEMENTS START DATE                 
         LA    R2,SORTCOMP          POINT TO ACTUAL 1R RECORD                   
         USING PHIRECD,R3           BUILD NEW SALARY HISTORY RECORD             
         L     R3,AIO3              IN IO3                                      
         MVC   0(L'PHIKEY,R3),SPACES                                            
         XC    L'PHIKEY(PHIRFST-PHIRLEN,R3),L'PHIKEY(R3)                        
         MVI   PHIKTYP,PHIKTYPQ              BUILD RECORD KEY                   
         MVI   PHIKSUB,PHIKSUBQ                                                 
         MVC   PHIKCPY,SORTCOMP                                                 
         MVC   PHIKOFC(L'SRTOFF),SRTOFF                                         
         MVC   PHIKDPT(L'SRTDEP),SRTDEP                                         
         MVC   PHIKSBD(L'SRTSUB),SRTSUB                                         
         MVC   PHIKPER(L'SRTEMP),SRTEMP                                         
         MVI   PHIKSEQ,0                                                        
         LA    R5,PHIRFST           R5 WILL POINT TO WHERE I'M UP               
         ST    R5,REG5TEMP          TO AT BUILDING 86 ELEMENTS                  
         B     SALH105              SKIP TO PROCESSING                          
                                                                                
SALH100  DS    0H                                                               
         L     R4,REG4TEMP          RESTORE ADDRESS OF PREVIOUSLY               
         MVI   ELCODE,MSAELQ        PROCESSED '52' ELEM AND GET                 
         BAS   RE,NEXTEL            NEXT ONE                                    
         BNE   SALH900                                                          
         ST    R4,REG4TEMP                                                      
         CLC   START,MSABEG         IF DATES HAVE CHANGED THEN ADD              
         BE    SALH103              THIS SALARY RECORD                          
                                                                                
         BAS   RE,LOOKEMP                                                       
         TM    ERRSW,NOEMP                                                      
         BO    SALH101                                                          
         BAS   RE,LOOKCHK                                                       
         TM    ERRSW,CHKOUT                                                     
         BO    SALH101                                                          
                                                                                
         L     R3,AIO3                                                          
         CLC   PHIRLEN,=X'0039'                                                 
         BE    SALH103                                                          
         BAS   RE,ADDSALH                                                       
SALH101  BAS   RE,PRNTSALH                                                      
SALH103  MVC   SV52TYP,MSATYPE      SAVE ORIGINAL TYPE                          
         MVC   START,MSABEG         SET ACSLRY START AND END DATE               
         MVC   END,MSABEG           TO THIS ELEMENTS START DATE                 
                                                                                
SALH105  DS    0H                                                               
         L     R3,AIO3                                                          
         SR    R1,R1                                                            
         ICM   R1,3,MSABEG          THIS WILL STORE DATE IN KEY TO              
         LNR   R1,R1                KEEP DATES IN DESCENDING ORDER              
         STCM  R1,3,PHIKMOA                                                     
                                                                                
         BAS   RE,SALREAD           READ KEY BUILT TO SEE IF RECORD             
         CLI   WRTSW,C'A'           ALREADY EXISTS - A INDICATES AN             
         BNE   SALH125              ADD WILL BE PERFORMED                       
         MVI   PHIRLEN+1,X'39'      SET REC LEN AND POINT TO FIRST              
         LA    R5,PHIRFST           ELEMENT                                     
         ST    R5,REG5TEMP          STORE WORKING ADDRESS                       
         B     SALH140                                                          
                                                                                
SALH125  L     R3,AIO3                                                          
         SR    R4,R4                                                            
         ICM   R4,3,PHIRLEN                                                     
         AR    R3,R4                                                            
         BCTR  R3,0                                                             
         ST    R3,REG5TEMP                                                      
                                                                                
SALH140  DS    0H                                                               
         L     R4,REG4TEMP                                                      
         MVC   SAVSTART,MSABEG      SAVE THIS ELEMENTS START AND END            
         MVC   SAVEND,MSAEND        FOR NEXT PASS                               
                                                                                
         LA    R2,TYPETAB           ONLY WANT TO CONVERT '52' ELEM              
SALH145  CLI   0(R2),X'FF'          TYPES INCLUDED IN TABLE                     
         BE    SALH500                                                          
         CLC   0(1,R2),MSATYPE                                                  
         BE    SALH148                                                          
         LA    R2,TYPENT(R2)                                                    
         B     SALH145                                                          
SALH148  MVC   SV86TYP,1(R2)        SAVE CONVERTED TYPE                         
         ST    R2,REG2TEMP                                                      
                                                                                
SALH150  L     R4,AIO3                                                          
         LA    R4,56(R4)                                                        
         MVI   ELCODE,PDEELQ                                                    
         CLI   0(R4),PDEELQ                                                     
         BNE   SALH250                                                          
SALH180  CLC   PDEDTE-PDEELD(2,R4),START                                        
         BNE   SALH185                                                          
         CLC   PDENUM-PDEELD(1,R4),SV86TYP                                      
         BE    SALH500                                                          
SALH185  BAS   RE,NEXTEL                                                        
         BE    SALH180                                                          
                                                                                
SALH250  DS    0H                                                               
         USING PDEELD,R5            START BUILDING NEW '86'                     
         L     R5,REG5TEMP                                                      
         XC    0(PDELNQ,R5),0(R5)                                               
         MVI   PDEEL,PDEELQ                                                     
         MVI   PDELN,PDELNQ                                                     
         MVC   PDEDTE(L'START),START                                            
         LA    R2,MONTAB            SET CORRECT LAST DAY OF MONTH               
SALH300  CLI   0(R2),X'FF'          FROM TABLE                                  
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   PDEDTE+1(1),0(R2)                                                
         BE    SALH320                                                          
         LA    R2,MTABLEN(R2)                                                   
         B     SALH300                                                          
SALH320  MVC   PDEDTE+2(1),2(R2)                                                
         MVC   SAVPDTE,PDEDTE                                                   
         MVC   PDENUM,SV86TYP                                                   
         BAS   RE,SALAMT                                                        
         MVC   PDEAMT,SV86AMT                                                   
         ZAP   PDEADJ,=P'0'                                                     
                                                                                
         LA    R2,PDELNQ                                                        
         AR    R5,R2                BUMP ADDRESS FOR NEXT '86' EL               
         ST    R5,REG5TEMP                                                      
         MVI   0(R5),0              MARK EOR INCASE IT IS LAST 52 EL            
         SR    R4,R4                                                            
         L     R3,AIO3                                                          
         ICM   R4,3,PHIRLEN                                                     
         AR    R4,R2                ADD LENGTH OF NEW ELEM TO RECORD            
         STCM  R4,3,PHIRLEN         LENGTH                                      
                                                                                
SALH500  CLC   START,SAVEND         GONE THROUGH ENTIRE RANGE OF MONTHS         
         BE    SALH100                                                          
                                                                                
SALH520  LA    R2,MONTAB                                                        
SALH560  CLI   0(R2),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   START+1(1),0(R2)                                                 
         BE    SALH580                                                          
         LA    R2,MTABLEN(R2)                                                   
         B     SALH560                                                          
SALH580  MVC   START+1(1),1(R2)                                                 
         CLC   START+1(1),X'01'                                                 
         BNE   SALH530                                                          
         SR    R2,R2                                                            
         IC    R2,START                                                         
         LA    R2,1(R2)                                                         
         STC   R2,START                                                         
SALH530  MVC   END,START                                                        
                                                                                
         L     R3,AIO3                                                          
         SR    R1,R1                                                            
         ICM   R1,3,MSABEG          THIS WILL STORE DATE IN KEY TO              
         LNR   R1,R1                KEEP DATES IN DESCENDING ORDER              
         STCM  R1,3,PHIKMOA                                                     
                                                                                
         BAS   RE,SALREAD           READ KEY BUILT TO SEE IF RECORD             
         CLI   WRTSW,C'A'           ALREADY EXISTS - A INDICATES AN             
         BNE   SALH525              ADD WILL BE PERFORMED                       
         MVI   PHIRLEN+1,X'39'      SET REC LEN AND POINT TO FIRST              
         LA    R5,PHIRFST           ELEMENT                                     
         ST    R5,REG5TEMP          STORE WORKING ADDRESS                       
         B     SALH250                                                          
                                                                                
SALH525  L     R3,AIO3                                                          
         SR    R4,R4                                                            
         ICM   R4,3,PHIRLEN                                                     
         AR    R3,R4                                                            
         BCTR  R3,0                                                             
         ST    R3,REG5TEMP                                                      
         B     SALH250                                                          
                                                                                
SALH900  DS    0H                                                               
                                                                                
         BAS   RE,LOOKEMP                                                       
         TM    ERRSW,NOEMP                                                      
         BO    SALH910                                                          
         BAS   RE,LOOKCHK                                                       
         TM    ERRSW,CHKOUT                                                     
         BO    SALH910                                                          
         L     R3,AIO3                                                          
         CLC   PHIRLEN,=X'0039'                                                 
         BE    SALHXIT                                                          
                                                                                
         BAS   RE,ADDSALH                                                       
SALH910  BAS   RE,PRNTSALH                                                      
SALHXIT  B     EXIT                                                             
         EJECT                                                                  
*------------------------------------------------------------------*            
*        READ FOR RECORD FIRST, IF FOUND UPDATE THE RECORD                      
*        IF NOT FOUND ADD NEW RECORD                                            
*        ** THE PERCENTAGE CODE HAS NOT BEEN TESTED AS AYER                     
*        ** DOES NOT HAVE ANY '52' ELEMENTS SET UP THAT WAY                     
*------------------------------------------------------------------*            
SALAMT   NTR1                                                                   
         ZAP   SV86AMT,=P'0'                                                    
         LA    R5,SORTCOMP                                                      
         LA    R6,SALAREA                                                       
         GOTO1 =V(ACSLRY),DMCB,(R5),START,(R6)                                  
                                                                                
         USING SLRD,R6                                                          
         LA    R6,SALAREA                                                       
         LR    R3,R6                                                            
         L     R2,REG2TEMP                                                      
         SR    R5,R5                                                            
         IC    R5,2(R2)                                                         
         AR    R3,R5                                                            
         TM    L'SLRTOT(R3),X'20'          IS THIS A 2 DECIMAL                  
         BZ    SAMT050                     PERCENTAGE - THEN CALCULATE          
         ZAP   DUBDUB,SLRTOT               THE ACTUAL DOLLARS FROM              
         MP    DUBDUB,0(L'SLRTOT,R3)       THE TOTAL DOLLARS                    
         SRP   DUBDUB,57,5                                                      
         ZAP   SV86AMT,DUBDUB                                                   
         B     SAMTXIT                                                          
                                                                                
SAMT050  TM    L'SLRTOT(R3),X'10'          IS THIS A 5 DECIMAL                  
         BZ    SAMT100                     PERCENTAGE - THEN CALCULATE          
         ZAP   DUBDUB,SLRTOT               THE ACTUAL DOLLARS FROM THE          
         MP    DUBDUB,0(L'SLRTOT,R3)       TOTAL DOLLARS                        
         SRP   DUBDUB,60,5                                                      
         ZAP   SV86AMT,DUBDUB                                                   
         B     SAMTXIT                                                          
                                                                                
SAMT100  ZAP   SV86AMT,0(L'SLRTOT,R3)      SAVE FINAL CALCULATION               
SAMTXIT  B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*------------------------------------------------------------------*            
*        ADD EMPLOYEE NUMBER TO TABLE                                           
*------------------------------------------------------------------*            
         DROP  R3                                                               
         DROP  R4                                                               
         DROP  R5                                                               
ADDEMP   NTR1                                                                   
         CP    EMPTCNT,EMPTMAX                                                  
         BL    *+6                                                              
         DC    H'0'                  TABLE NEED TO BE MADE BIGGER               
                                                                                
         USING EMPTABD,R2                                                       
         L     R2,AEMPTAB                                                       
AEMP10   CLI   0(R2),X'FF'                                                      
         BE    AEMP50                                                           
         LA    R2,EMPENT(R2)                                                    
         B     AEMP10                                                           
AEMP50   DS    0H                                                               
         MVC   EMPNUM(L'SRTEMP),SORTSAVE+SEMPLEN+1                              
                                                                                
         USING EMPGEND,R3                                                       
         LA    R3,EMPOFC1                                                       
         ZAP   CNTLOC,=P'0'                                                     
         USING PERRECD,R4                                                       
         L     R4,AIO1                                                          
         LA    R4,PERRFST                                                       
         MVI   ELCODE,LOCELQ                                                    
         CLI   0(R4),LOCELQ                                                     
         BNE   AEMP080                                                          
         USING LOCELD,R4                                                        
AEMP060  DS    0H                                                               
         CP    CNTLOC,LOCMAX                                                    
         BL    *+6                                                              
         DC    H'0'                                                             
         MVC   EMPOFC,LOCOFF                                                    
         MVC   EMPDPT,LOCDEPT                                                   
         MVC   EMPSUB,LOCSUB                                                    
         MVC   EMPSTR,LOCSTART                                                  
         MVC   EMPEND,LOCEND                                                    
         LA    R3,EMPLOC(R3)                                                    
         AP    CNTLOC,=P'1'                                                     
AEMP080  BAS   RE,NEXTEL                                                        
         BE    AEMP060                                                          
                                                                                
AEMP100  LA    R2,EMPENT(R2)                                                    
         MVI   0(R2),X'FF'                                                      
         B     EXIT                                                             
         EJECT                                                                  
*------------------------------------------------------------------*            
*        LOOK FOR EMPLOYEE NUMBER IN TABLE                                      
*------------------------------------------------------------------*            
LOOKEMP  NTR1                                                                   
         NI    ERRSW,X'FF'-NOEMP                                                
         USING EMPTABD,R2                                                       
         L     R2,AEMPTAB                                                       
LEMP10   CLI   0(R2),X'FF'                                                      
         BE    LEMP40                                                           
         CLC   EMPNUM(L'SRTEMP),SRTEMP                                          
         BE    LEMP50                                                           
         LA    R2,EMPENT(R2)                                                    
         B     LEMP10                                                           
                                                                                
LEMP40   DS    0H                                                               
         CLI   QOPT7,C'Y'                                                       
         BE    *+8                                                              
         OI    ERRSW,NOEMP                                                      
LEMP50   DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
*------------------------------------------------------------------*            
*        IS CHECK DATE OUTSIDE LOCATION DATES                                   
*------------------------------------------------------------------*            
LOOKCHK  NTR1                                                                   
         NI    ERRSW,X'FF'-CHKOUT                                               
         USING EMPTABD,R2                                                       
         L     R2,AEMPTAB                                                       
LCHK010  CLI   0(R2),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   EMPNUM(L'SRTEMP),SRTEMP                                          
         BE    LCHK050                                                          
         LA    R2,EMPENT(R2)                                                    
         B     LCHK010                                                          
                                                                                
LCHK050  LA    R4,EMPNENTS          NUMBER OF LOCATION ENTRIES                  
         USING EMPGEND,R3                                                       
         LA    R3,EMPOFC1           STARTING ADDR OF LOCATION ENTRIES           
LCHK070  CLC   EMPOFC(L'SRTOFF),SRTOFF                                          
         BNE   LCHK080                                                          
         CLC   EMPDPT(L'SRTDEP),SRTDEP                                          
         BNE   LCHK080                                                          
         CLC   EMPSUB(L'SRTSUB),SRTSUB                                          
         BNE   LCHK080                                                          
         OC    EMPEND,EMPEND                                                    
         BZ    LCHKXIT                                                          
         B     LCHK100                                                          
LCHK080  LA    R3,EMPLOC(R3)                                                    
         BCT   R4,LCHK070                                                       
         DC    H'0'                 NEED TO INCREASE NUMBER OF POSSIBLE         
*                                   LOCATION ENTRIES (LOCMAX)                   
         USING PHIRECD,R4                                                       
LCHK100  L     R4,AIO3                                                          
         LA    R4,PHIRFST                                                       
         USING PDEELD,R4                                                        
         CLI   PDEEL,PDEELQ                                                     
         BNE   LCHK250                                                          
LCHK120  CLC   PDEDTE,EMPEND                                                    
         BNH   LCHK250                                                          
         CLC   EMPEND(2),PDEDTE                                                 
         BNE   LCHK200                                                          
         MVC   PDEDTE,EMPEND                                                    
         B     LCHK250                                                          
                                                                                
LCHK200  DS    0H                                                               
         CLI   QOPT7,C'Y'                                                       
         BE    *+8                                                              
         OI    ERRSW,CHKOUT                                                     
LCHK250  BAS   RE,NEXTEL                                                        
         BE    LCHK120                                                          
LCHKXIT  B     EXIT                                                             
         EJECT                                                                  
*------------------------------------------------------------------*            
*        READ FOR RECORD FIRST, IF FOUND UPDATE THE RECORD                      
*        IF NOT FOUND ADD NEW RECORD                                            
*------------------------------------------------------------------*            
SALREAD  NTR1                                                                   
         USING ACTRECD,R2                                                       
         MVI   WRTSW,C'A'                                                       
         GOTO1 DATAMGR,DMCB,(X'08',DMREAD),=C'ACCDIR',AIO3,AIO2                 
         TM    DMCB+8,X'10'                                                     
         BO    SALRXIT                                                          
         MVI   WRTSW,C'P'                                                       
         L     R2,AIO2                                                          
         MVC   DSKADR,ACTKDA                                                    
         TM    DMCB+8,X'02'                                                     
         BZ    SALR30                                                           
         NI    ACTKSTAT,X'FF'-ACTSDELT                                          
         CLI   RCWRITE,C'N'                                                     
         BE    SALR30                                                           
         GOTO1 DATAMGR,DMCB,DMWRT,=C'ACCDIR',AIO2,AIO2                          
         B     SALR30                                                           
                                                                                
SALR30   GOTO1 DATAMGR,DMCB,=CL8'GETREC',=C'ACCMST',DSKADR,AIO3,ADWRK           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO3                                                          
         NI    ACTRSTAT,X'FF'-ACTSDELT                                          
SALRXIT  B     EXIT                                                             
         EJECT                                                                  
*------------------------------------------------------------------*            
*              READ FOR RECORD FIRST, IF FOUND UPDATE THE RECORD                
*              IF NOT FOUND ADD NEW RECORD                                      
*------------------------------------------------------------------*            
ADDSALH  NTR1                                                                   
         CLI   WRTSW,C'A'                                                       
         BE    ASAL540                                                          
         CLI   WRTSW,C'P'                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   RCWRITE,C'N'                                                     
         BE    ASALXIT                                                          
         GOTO1 DATAMGR,DMCB,=CL8'PUTREC',=CL8'ACCMST',DSKADR,AIO3,ADWRK         
         CLI   DMCB+8,0                                                         
         BE    ASALXIT                                                          
         DC    H'0'                                                             
                                                                                
ASAL540  CLI   RCWRITE,C'N'                                                     
         BE    ASALXIT                                                          
         L     R2,AIO3                                                          
         GOTO1 DATAMGR,DMCB,=CL8'ADDREC',=CL8'ACCMST',FULL,(R2),ADWRK           
         CLI   DMCB+8,0                                                         
         BE    ASALXIT                                                          
         DC    H'0'                                                             
                                                                                
ASALXIT  DS    0H                                                               
         CLI   QOPT1,C'T'                                                       
         BNE   EXIT                                                             
         L     R2,AIO3               MOVE RECORD TO SORTAREA AND                
         BAS   RE,PUTTAPE                                                       
         B     EXIT                                                             
         EJECT                                                                  
*------------------------------------------------------------------*            
*        PRINT SALARY HISTORY INFORMATION JUST ADDED/UPDATED                    
*------------------------------------------------------------------*            
         DROP  R4                                                               
PRNTSALH NTR1                                                                   
         USING PRNTD,R2                                                         
         LA    R2,XP                                                            
         MVC   0(198,R2),XSPACES                                                
         USING PHIRECD,R3           BUILD NEW SALARY HISTORY RECORD             
         L     R3,AIO3                                                          
         MVC   PSALACC(PHIKMOA-PHIKOFC),PHIKOFC                                 
         SR    R1,R1                                                            
         ICM   R1,3,PHIKMOA                                                     
         LNR   R1,R1                                                            
         STCM  R1,3,WORK                                                        
         MVI   WORK+2,X'01'                                                     
         GOTO1 DATCON,DMCB,(1,WORK),(6,PSALDAT)                                 
         L     R4,AIO3                                                          
         LA    R4,56(R4)                                                        
         MVI   ELCODE,PDEELQ                                                    
         USING PDEELD,R4                                                        
         CLI   0(R4),PDEELQ                                                     
         BE    PSAL050                                                          
PSAL030  BAS   RE,NEXTEL                                                        
         BNE   PSALXIT                                                          
PSAL050  DS    0H                                                               
         AP    PERSTOT,PDEAMT                                                   
         EDIT  (P6,PDEAMT),(14,PSALAMT),2                                       
         GOTO1 DATCON,DMCB,(1,PDEDTE),(5,PCHKDAT)                               
         MVC   PSALTYP,PDENUM                                                   
         MVZ   PSALTYP,=X'F0'                                                   
         TM    ERRSW,NOEMP                                                      
         BZ    *+10                                                             
         MVC   PSALAMT+20(31),=C'**ERROR - NO EMPLOYEE RECORD **'               
         TM    ERRSW,CHKOUT                                                     
         BZ    *+10                                                             
         MVC   PSALAMT+20(36),=C'**ERROR-CHECK OUTSIDE LOCATION DATES'          
         GOTO1 ACREPORT                                                         
         B     PSAL030                                                          
                                                                                
PSALXIT  DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
*------------------------------------------------------------------*            
*        CLEAR IO AREA                                                          
*------------------------------------------------------------------*            
PUTTAPE  NTR1                                                                   
         USING ACTRECD,R2                                                       
         L     R4,AIO2                                                          
         XC    0(4,R4),0(R4)                                                    
         SR    R3,R3                                                            
         ICM   R3,3,ACTRLEN                                                     
         LA    R3,4(R3)                                                         
         STCM  R3,3,0(R4)                                                       
                                                                                
         LA    R4,4(R4)                                                         
         LA    R3,2000               PUT OUT TO SORT                            
         LA    R5,1996                                                          
         MVCL  R4,R2                                                            
                                                                                
         L     R4,AIO2                                                          
         PUT   TAPEOUT,(R4)                                                     
         B     EXIT                                                             
         EJECT                                                                  
*------------------------------------------------------------------*            
*        COMPARE EMPLOYEE NAMES ON DUPLICATE EMPLOYEE NUMBER                    
*------------------------------------------------------------------*            
COMPNAME NTR1                                                                   
         USING NAMEL,R4                                                         
         MVI   ELCODE,NAMELQ         GET ADDRESS OF OLD NAME ELEMENT            
         LA    R4,SORTCOMP           FROM RECORD FROM SORT                      
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         MVC   CURR20,SPACES                                                    
         ZIC   R5,NAMLN                                                         
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   CURR20(0),0(R4)       SAVE CURRENT 20 ELEMENT                    
                                                                                
         ZIC   R5,NAMLN              COMPARE CURRENT 20 ELEM TO SAVED           
         BCTR  R5,0                  20 ELEMENT                                 
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R4),SAVE20                                                   
         BE    EXIT                                                             
                                                                                
         CLI   QOPT7,C'Y'                                                       
         BE    *+8                                                              
         OI    ERRSW,DUPNUM+NOADD                                               
         B     EXIT                                                             
         EJECT                                                                  
*------------------------------------------------------------------*            
*        PUT RECORD TO SORT                                                     
*------------------------------------------------------------------*            
PUTSORT  NTR1                                                                   
         GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
         B     EXIT                                                             
*------------------------------------------------------------------*            
*        CLEAR IO AREA                                                          
*------------------------------------------------------------------*            
CLRIO1   NTR1                                                                   
*        L     R2,AIO1                                                          
*        MVI   1(R2),C' '                                                       
*        LA    R3,L'IO1                                                         
*        LA    R4,1(R2)                                                         
*        LA    R5,L'IO1-1                                                       
*        MVCL  R4,R2                                                            
         LA    R4,4                                                             
         L     R5,AIO1                                                          
CLR10    XC    0(250,R5),0(R5)                                                  
         BCT   R4,CLR10                                                         
         B     EXIT                                                             
         EJECT                                                                  
*------------------------------------------------------------------*            
*        CLEAR IO AREA                                                          
*------------------------------------------------------------------*            
CLRIO3   NTR1                                                                   
         LA    R4,4                                                             
         L     R5,AIO3                                                          
CLR310   XC    0(250,R5),0(R5)                                                  
         BCT   R4,CLR310                                                        
         B     EXIT                                                             
         EJECT                                                                  
*------------------------------------------------------------------*            
*        GET RECORD FROM SORT                                                   
*------------------------------------------------------------------*            
GETSORT  NTR1                                                                   
         XC    ASORT,ASORT        IF CLEAR IS END OF SORT                       
         GOTO1 SORTER,DMCB,=C'GET',SORTREC                                      
         L     R1,DMCB+4                                                        
         LTR   R1,R1                                                            
         BZ    EXIT                                                             
         ST    R1,ASORT                                                         
                                                                                
         LA    R2,SORTREC         MOVE SORTREC INTO SAVEAREA                    
         LA    R3,SORTLEN                                                       
         LA    R4,SORTSAVE                                                      
         LA    R5,SORTLEN                                                       
         MVCL  R4,R2                                                            
                                                                                
         L     R2,ASORT           MOVE CURRENT SORTREC INTO SORTAREA            
         LA    R3,SORTLEN                                                       
         LA    R4,SORTREC                                                       
         LA    R5,SORTLEN                                                       
         MVCL  R4,R2                                                            
                                                                                
         B     EXIT                                                             
         EJECT                                                                  
*-------------------------------------------------------------------*           
*              ROUTINE TO GET AN ELEMENT                                        
*              P1   BYTE 0    ELEMENT CODE                                      
*                   BYTE 1-3  A(RECORD)                                         
*              P2   BYTE 0    LENGTH OF SEARCH ARGUMENT                         
*                   BYTE 1-3  A(SEARCH ARGUMENT)                                
*-------------------------------------------------------------------*           
DELEL    NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         ZIC   R4,0(R1)                                                         
         ZIC   R5,4(R1)                                                         
         GOTO1 HELLO,DMCB,(C'D',=C'ACCMST  '),((R4),(R2)),((R5),(R3))           
         B     EXIT                                                             
*-------------------------------------------------------------------*           
*              ROUTINE TO ADD AN ELEMENT                                        
*              P1   A(RECORD)                                                   
*              P2   A(ELEMENT)                                                  
*-------------------------------------------------------------------*           
ADDEL    NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         GOTO1 HELLO,DMCB,(C'P',=C'ACCMST  '),(R2),(R3)                         
         CLI   DMCB+12,0                                                        
         BE    EXIT                                                             
         DC    H'0'                CAN'T ADD THE ELEMENT                        
         B     EXIT                                                             
*-------------------------------------------------------------------*           
*              ROUTINE TO GET AN ELEMENT                                        
*              P1   BYTE 0    ELEMENT CODE                                      
*                   BYTE 1-3  A(RECORD)                                         
*              P2   BYTE 0    LENGTH OF SEARCH ARGUMENT                         
*                   BYTE 1-3  A(SEARCH ARGUMENT)                                
*-------------------------------------------------------------------*           
GETELEM  NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         ZIC   R4,0(R1)                                                         
         ZIC   R5,4(R1)                                                         
         GOTO1 HELLO,DMCB,(C'G',=C'ACCMST  '),((R4),(R2)),((R5),(R3))           
         B     EXIT                                                             
                                                                                
         PRINT GEN                                                              
         GETEL R4,DATADISP,ELCODE                                               
         PRINT NOGEN                                                            
                                                                                
EXIT     XIT1                                                                   
         EJECT                                                                  
*------------------------------------------------------------------*            
*        CONSTANTS                                                              
*------------------------------------------------------------------*            
PRNTBL   DC    V(PRNTBL)                                                        
HELLO    DC    V(HELLO)                                                         
SORTER   DC    V(SORTER)                                                        
         DC    X'FF'                                                            
                                                                                
PERSTOT  DC    PL10'0'                                                          
REPSTOT  DC    PL10'0'                                                          
                                                                                
*        IF INCREASING EMPTMAX MUST ALSO INCREASE EMPTAB DEFINITION             
EMPTMAX  DC    PL3'5500'                                                        
*        IF INCREASING LOCMAX MUST ALSO INCREASE NUMBER OF LOCATION             
*        GROUP DEFINITIONS IN EMPTABD WHICH WILL AUTOMATICALLY INCREASE         
*        EMPNENTS                                                               
LOCMAX   DC    PL2'30'                                                          
CNTLOC   DC    PL2'0'                                                           
                                                                                
PERRECS  DC    CL1'1'                                                           
SALRECS  DC    CL1'2'                                                           
                                                                                
NINES    DC    CL12'999999999999'                                               
                                                                                
TYPETAB  DS    0F                                                               
         DC    X'5005',AL1(SLRSAL-SLRD)     SALARY                              
**       DC    X'1010',AL1(SLRBEN-SLRD)     SALARY                              
TYPENT   EQU   *-TYPETAB                                                        
         DC    X'FF'                                                            
                                                                                
MONTAB   DS    0C                                                               
         DC    X'010231'                                                        
MTABLEN  EQU   *-MONTAB                                                         
         DC    X'020328'                                                        
         DC    X'030431'                                                        
         DC    X'040530'                                                        
         DC    X'050631'                                                        
         DC    X'060730'                                                        
         DC    X'070831'                                                        
         DC    X'080931'                                                        
         DC    X'091030'                                                        
         DC    X'101131'                                                        
         DC    X'111230'                                                        
         DC    X'120131'                                                        
         DC    X'FF'                                                            
                                                                                
*SORTCARD DC    CL80'SORT FIELDS=(1,1,A,10,07,A),FORMAT=CH,WORK=1'              
*RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=(1001,,,,)'                           
                                                                                
SORTCARD DC    CL80'SORT FIELDS=(1,1,A,99,99,A),FORMAT=CH,WORK=1'               
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=(9999,,,,)'                            
                                                                                
TAPEOUT  DCB   DDNAME=TAPEOUT,DSORG=PS,MACRF=(PM),                     X        
               RECFM=VB,LRECL=4004,BLKSIZE=32760                                
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        WORKING STORAGE                                                        
*---------------------------------------------------------------------*         
ACZPD    DSECT                                                                  
ASORT    DS    F                                                                
DSKADR   DS    F                                                                
AIO1     DS    A                                                                
AIO2     DS    A                                                                
AIO3     DS    A                                                                
AIO4     DS    A                                                                
AEMPTAB  DS    A                                                                
AMYXP    DS    A                                                                
REG2TEMP DS    F                                                                
REG4TEMP DS    F                                                                
REG5TEMP DS    F                                                                
EMPTCNT  DS    F                                                                
                                                                                
         DS    0D                                                               
DUBDUB   DS    PL16                                                             
         DS    0D                                                               
SALAREA  DS    (SLRLEN)C                                                        
SALDATES DS    0CL4                                                             
START    DS    CL2                                                              
END      DS    CL2                                                              
SAVDATES DS    0CL4                                                             
SAVSTART DS    CL2                                                              
SAVEND   DS    CL2                                                              
SAVPDTE  DS    CL3                                                              
SAVEMP   DS    CL(L'SRTEMP)                                                     
SV52TYP  DS    CL1                                                              
SV86TYP  DS    CL1                                                              
SV86AMT  DS    PL(L'PDEAMT)                                                     
ELCOUNT  DS    PL2                                                              
WRTSW    DS    CL1                                                              
                                                                                
DIRREAD  DS    CL42                                                             
                                                                                
ELCODE   DS    CL1                                                              
CNTRLSW  DS    CL1                                                              
NOTFRST  EQU   X'80'                                                            
FRSTSAL  EQU   X'40'                                                            
ERRSW    DS    CL1                                                              
DUPNUM   EQU   X'80'                                                            
NOADD    EQU   X'40'                                                            
OVERLAP  EQU   X'20'                                                            
NOEMP    EQU   X'10'                                                            
CHKOUT   EQU   X'08'                                                            
                                                                                
SAVEEMP  DS    CL(L'SRTEMP)                                                     
CURR20   DS    CL50                                                             
SAVE20   DS    CL50                                                             
BLDELS   DS    CL256                                                            
CURR56   DS    CL(EMPLNQ)                                                       
                                                                                
SVLNAME  DS    CL36                                                             
SVLNAMEL DS    XL1                                                              
SVFNAME  DS    CL36                                                             
SVFNAMEL DS    XL1                                                              
                                                                                
SAVLSTR  DS    XL3                                                              
SAVLEND  DS    XL3                                                              
SAVHIRD  DS    XL3                                                              
TEMPSTR  DS    CL6                                                              
TEMPEND  DS    CL6                                                              
SORTSAV2 DS    CL16                                                             
ADWRK    DS    12D                                                              
                                                                                
SORTREC  DS    0CL1001                                                          
SORTKEY  DS    0CL16                                                            
SORTTYPE DS    CL1                                                              
SORTCOMP DS    CL1                                                              
SORTUNIT DS    CL1                                                              
SORTLEDG DS    CL1                                                              
SOFFLEN  EQU   *-SORTCOMP                                                       
SRTOFF   DS    CL1                                                              
SDEPLEN  EQU   *-SORTCOMP                                                       
SRTDEP   DS    CL2                                                              
SSUBLEN  EQU   *-SORTCOMP                                                       
SRTSUB   DS    CL2                                                              
SEMPLEN  EQU   *-SORTCOMP                                                       
SRTEMPL  EQU   *-SORTKEY                                                        
SRTEMP   DS    CL7                                                              
         DS    CL985                                                            
SORTLEN  EQU   *-SORTREC                                                        
                                                                                
SORTSAVE DS    CL(SORTLEN)                                                      
                                                                                
         EJECT                                                                  
                                                                                
EMPGEND  DSECT                                                                  
EMPOFC   DS    CL(L'SRTOFF)                                                     
EMPDPT   DS    CL(L'SRTDEP)                                                     
EMPSUB   DS    CL(L'SRTSUB)                                                     
EMPSTR   DS    XL3                                                              
EMPEND   DS    XL3                                                              
EMPLOC   EQU   *-EMPOFC                                                         
                                                                                
EMPTABD  DSECT                                                                  
EMPNUM   DS    CL(L'SRTEMP)                                                     
                                                                                
EMPOFC1  DS    CL(L'SRTOFF)                                                     
EMPDPT1  DS    CL(L'SRTDEP)                                                     
EMPSUB1  DS    CL(L'SRTSUB)                                                     
EMPSTR1  DS    XL3                                                              
EMPEND1  DS    XL3                                                              
EMPSINGL EQU   *-EMPOFC1                                                        
                                                                                
EMPOFC2  DS    29CL(EMPSINGL)                                                   
                                                                                
EMPENT   EQU   *-EMPTABD                                                        
EMPNENTS EQU   (*-EMPOFC1)/(EMPLOC)                                             
         EJECT                                                                  
                                                                                
PRNTD    DSECT                                                                  
PLINE    DS    0CL164                                                           
PACTNEW  DS    CL11                                                             
         DS    CL4                                                              
PACTOLD  DS    CL14                                                             
         DS    CL4                                                              
PLNAME   DS    CL31                                                             
         DS    CL2                                                              
PFNAME   DS    CL31                                                             
         DS    CL6                                                              
PHIRE    DS    CL8                                                              
         DS    CL4                                                              
PTERM    DS    CL8                                                              
         DS    CL4                                                              
PLOCSTR  DS    CL8                                                              
         DS    CL2                                                              
PLOCEND  DS    CL8                                                              
         DS    CL4                                                              
PACTION  DS    CL6                                                              
         DS    CL2                                                              
PTSLOCK  DS    CL8                                                              
                                                                                
         ORG   PACTNEW                                                          
PSALACC  DS    CL16                                                             
         DS    CL10                                                             
PSALDAT  DS    CL6                                                              
         DS    CL16                                                             
PSALTYP  DS    CL1                                                              
         DS    CL10                                                             
PCHKDAT  DS    CL6                                                              
         DS    CL14                                                             
PSALAMT  DS    CL14                                                             
         EJECT                                                                  
                                                                                
         CSECT                                                                  
         DS    F                                                                
IO1      DS    CL2000                                                           
                                                                                
         DS    F                                                                
IO2      DS    CL2000                                                           
                                                                                
         DS    F                                                                
IO3      DS    CL2000                                                           
                                                                                
         DS    F                                                                
IO4      DS    CL2000                                                           
         EJECT                                                                  
                                                                                
         CSECT                                                                  
EMPTAB   DS    0C                                                               
         DC    XL1'FF'                                                          
         DS    5500CL(EMPENT)                                                   
                                                                                
         CSECT                                                                  
MYXP     DS    0C                                                               
         DS    30CL198                                                          
                                                                                
       ++INCLUDE DDSLRD                                                         
SALLNQ   EQU   (SLRADMST-SLRSAL)/(SLROVT-SLRSAL)+1                              
         EJECT                                                                  
                                                                                
                                                                                
*  ACREPWORKD                                                                   
*  ACGENFILE                                                                    
*  ACGENMODES                                                                   
*  ACMASTD                                                                      
*  ACBIGPRNTD                                                                   
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACMASTD                                                        
       ++INCLUDE ACBIGPRNTD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'044ACREPZ902 05/01/02'                                      
         END                                                                    
