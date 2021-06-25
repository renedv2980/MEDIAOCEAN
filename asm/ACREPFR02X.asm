*          DATA SET ACREPFR02X AT LEVEL 037 AS OF 07/22/02                      
*PHASE ACFR02A                                                                  
*INCLUDE UNDERLIN                                                               
*INCLUDE ACSLRY                                                                 
*INCLUDE ACSALHST                                                               
*INCLUDE COVAIL                                                                 
*INCLUDE DLFLD                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE PRNTBL                                                                 
*                                                                               
**********************************************************************          
*                                                                    *          
*    TITLE       :    ACREPFR02 -- FEE REPORT                        *          
*    NOTES       :    1 THIS IS A TOYOTA SPECIAL FOR SAATCHI         *          
*                     2 USE SUPERLEDGER FR FOR DEPT CODES            *          
*    OPTIONS     :                                                   *          
*        QOPT1 = Y    INCLUDE GROUP REPORT                           *          
*        QOPT2 = S    SUPPRESS ALL REPORTS EXCEPT RECAPS             *          
*        QOPT3 = Y    RUN YTD BUT SUPPRESS NON REQUESTED MONTHS      *          
*        QOPT4 = A    DOWNLOAD OPTION - PERSON REPORT                *          
*              = B                      MUTLI LOCATION REPORT        *          
*              = C                      GROUP REPORT                 *          
*              = D                      GROUP RECAP                  *          
*              = E                      GROUP ANALYSIS               *          
*              = F                      RECAP                        *          
*              = G                      ANALYSIS                     *          
*        QOPT5 = S    SUPPRESS LOCKED ACCOUNTS                       *          
*        QOPT6 = S    SUPPRESS TIME & COST RECAP                     *          
*        QOPT7 = Y    DOWNLOAD                                       *          
*        QOPT8 = S    SUPPRESS TIME & COST QTR REPORT                *          
*                                                                    *          
*                                                                    *          
**********************************************************************          
         EJECT                                                                  
         TITLE 'ACFR02 -  FEE  REPORT'                                          
ACFR02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACFR**,R9                                                    
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING WORKD,RC                                                         
*                                                                               
         L     R8,VBIGPRNT                                                      
         USING BIGPRNTD,R8                                                      
*                                                                               
         L     R2,ADMASTC                                                       
         USING MASTD,R2                                                         
*                                                                               
         L     R4,MCBXAREA                                                      
         USING BOXD,R4                                                          
         MVC   BOXWIDTH,=F'198'    SET WIDTH FOR REPORT                         
         DROP  R2,R4                                                            
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
         CLI   MODE,LEDGFRST                                                    
         BE    LDGF                                                             
         CLI   MODE,LEVAFRST                                                    
         BE    LEVAF                                                            
         CLI   MODE,LEVBFRST                                                    
         BE    LEVBF                                                            
         CLI   MODE,LEVCFRST                                                    
         BE    LEVCF                                                            
         CLI   MODE,PROCACC                                                     
         BE    PACC                                                             
         CLI   MODE,SBACFRST                                                    
         BE    PSUB                                                             
         CLI   MODE,PROCHIST                                                    
         BE    PHST                                                             
         CLI   MODE,ACCLAST                                                     
         BE    ACCL                                                             
         CLI   MODE,REQLAST                                                     
         BE    REQL                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* RUN FIRST                                                          *          
**********************************************************************          
         SPACE 1                                                                
RUNF     DS    0H                                                               
*                                                                               
         MVC   VTYPES(VTYPLNQ),ADCONS                                           
*                                                                               
         L     R2,COMTAB           SET UP COMMON ENTRIES TO WORK NMODS          
         USING COMD,R2                                                          
RUNF20   CLI   COMRELO,EOT         END OF TABLE?                                
         BE    RUNF30                                                           
         SR    R0,R0                                                            
         IC    R0,COMNUMB          NUMBER OF ROUTINES                           
         SR    RE,RE                                                            
         ICM   RE,3,COMRELO        RE IS RELOCATED ADDR OF NMOD                 
         AR    RE,RC                                                            
         L     RE,0(RE)                                                         
         SR    RF,RF                                                            
         ICM   RF,3,COMENT                                                      
         AR    RF,RC                                                            
         LA    RF,0(RF)            RF ADDR OF 1ST ENTERABLE ROUT                
         SR    R1,R1                                                            
         ST    RE,0(RF)            ADDRESS OF COMMON                            
         STC   R1,0(RF)            SET ROUTINE NUMBER                           
         LA    R1,1(R1)                                                         
         LA    RF,4(RF)            NEXT ENTERABLE ROUTINE                       
         BCT   R0,*-16                                                          
         LA    R2,COMLEN(R2)       NEXT TABLE ENTRY                             
         B     RUNF20                                                           
*                                                                               
RUNF30   MVC   LITVALS(LITVALSL),GLOBALS     LITERALS TO GLOBAL STORAGE         
         GOTO1 MAIN                 ACQUIRE STORAGE                             
*                                                                               
         L     RF,SALAREA           CLEAR SALARY AREA                           
         USING SALARYD,RF                                                       
         XC    SALBLOCK(SALLNQ),SALBLOCK                                        
         DROP  RF                                                               
*                                                                               
         USING LDGRECD,R2                                                       
         LA    R2,DKEY                                                          
         MVC   LDGKEY,XSPACES                                                   
         MVC   LDGKCPY,RCCOMPFL                                                 
         MVC   LDGKUNT(L'CLILEG),CLILEG   GET 1C OFFICE LENGTH                  
         BAS   RE,READ                                                          
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO                                                           
         BAS   RE,GET                                                           
         USING ACLELD,R4                                                        
         LA    R4,LDGRFST                                                       
         SR    R0,R0                                                            
RUNF40   CLI   ACLEL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   ACLEL,ACLELQ                                                     
         BE    RUNF50                                                           
         IC    R0,ACLLN                                                         
         AR    R4,R0                                                            
         B     RUNF40                                                           
RUNF50   MVC   OFFLN,ACLVLEN       MOVE 1C OFFICE LENGTH INTO STORAGE           
*                                                                               
*        GOTO1 SUPBLD              BUILD TABLE OF SUPERLEDGER CODES             
*                                                                               
*        L     R2,ADMASTC                                                       
*        USING MASTD,R2                                                         
*        L     R4,MCBXAREA                                                      
*        USING BOXD,R4                                                          
*        MVC   BOXWIDTH,=F'198'    SET WIDTH FOR REPORT                         
*                                                                               
RUNFX    B     EXIT                                                             
         DROP  R2,R4                                                            
         EJECT                                                                  
**********************************************************************          
* REQUEST FIRST                                                      *          
**********************************************************************          
         SPACE 1                                                                
         USING BIND,RF                                                          
REQF     DS    0H                                                               
         L     RF,NMEBUFF          CLEAR TABLES                                 
         XC    BININ,BININ                                                      
         L     RF,DUPBUFF                                                       
         XC    BININ,BININ                                                      
         L     RF,TIBUFF                                                        
         XC    BININ,BININ                                                      
         L     RF,AGRPBUFF                                                      
         XC    BININ,BININ                                                      
         DROP  RF                                                               
*                                                                               
         CLI   QOPT7,C'Y'                                                       
         BNE   REQF10                                                           
         MVI   RCSUBPRG,9          SET PRINT RTE AS INVALID                     
         GOTO1 ADWNL,DMCB,(RC),DWNINIT      INITIALIZE DOWNLOAD RTE             
*                                                                               
REQF10   NI    FLAG,X'FF'-FLGYTD   TURN OFF YTD FLAG                            
         XC    FLAG2,FLAG2         INITIALIZE FLAG2                             
*                                                                               
         LA    R2,MAXDPTS          MAXIMUM NUMBER OF DEPTS                      
         L     RE,ADUPREC          CLEAR DEPT TOTAL RECORD FOR DUPREP           
         LA    RF,SRTLEN                                                        
         SR    R1,R1                                                            
         MVCL  RE,R0               CLEAR BLOCK                                  
         BCT   R2,*-8                                                           
*                                                                               
         MVC   MSGS,XSPACES        CLEAR MESSAGE-NOT USED UNTIL DUP REP         
         MVC   SVPRDNM,XSPACES     CLEAR SAVED AREA FOR PROD NAME               
         MVI   ACTIVITY,C'N'                                                    
         MVC   WORK(4),QSTART                                                   
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(1,WORK+6)                                  
         MVC   START,WORK+6                                                     
*                                                                               
         GOTO1 SUPBLD              BUILD TABLE OF SUPERLEDGER CODES             
*                                                                               
         USING ACQD,RF                                                          
         L     RF,ADQSTACK                                                      
         MVC   ACQCFLT1(L'ACMCFLTS),XSPACES CLEAR CONTRA FILTERS?               
         DROP  RF                                                               
*                                                                               
         USING ACMD,RF                                                          
         L     RF,AMONACC                                                       
         NI    ACMCFIND,ALL-ACMCCFLT   DO NOT FILTER ON CONTRA ACCOUNTS         
         DROP  RF                                                               
*                                                                               
         MVC   WORK(4),QEND                                                     
         GOTO1 DATCON,DMCB,(0,WORK),(1,WORK+6)                                  
         MVC   END,WORK+6                                                       
*                                                                               
         BAS   RE,GETFIS           GET FISCAL DATES FOR COMPARE                 
*                                                                               
         MVC   YTDQSTR,QSTART      SAVE QSTART FOR MONTHLY BUCKETS              
         MVC   SVSTART,START                                                    
         CLI   QOPT3,C'Y'          RUN YTD-SUPPRESS UNUSED MONTHS               
         BNE   REQF30                                                           
*                                                                               
         MVC   START,FISCSTR       ADJUST START DATE                            
         MVC   WORK(2),START                                                    
         MVI   WORK+2,X'01'                                                     
         GOTO1 DATCON,DMCB,(1,WORK),(0,WORK+6) WORK+6=C'YYMMDD'                 
         MVC   QSTART,WORK+6       QSTART=CL4'YYMM'                             
         MVC   YTDQSTR,QSTART                                                   
         OI    FLAG,FLGYTD         SET FLAG TO SHOW INTERNAL YTD IN USE         
*                                                                               
         USING MTHD,R3             BUILD LIST OF PACKED MONTHS                  
REQF30   LA    R3,MNTHS            FROM QSTART THRU QEND                        
         XC    MNTHS,MNTHS                                                      
         MVC   WORK(4),YTDQSTR                                                  
         LA    R0,MAXMTHS-1                                                     
         LA    R5,1                                                             
         MVC   MTHCODE,START       SET 1ST MONTH                                
         STC   R5,MTHNUM                                                        
REQF40   CLC   MTHCODE,END                                                      
         BE    REQF50                                                           
         LA    R5,1(R5)            ADD NUMBER OF REQUEST MONTHS                 
         LA    R3,MTHLEN(R3)                                                    
         GOTO1 ADDAY,DMCB,(C'M',WORK),WORK+6,1   GET NEXT MONTH                 
         GOTO1 DATCON,DMCB,(0,WORK+6),(1,WORK+12)                               
         MVC   MTHCODE,WORK+12     PACKED                                       
         ZAP   MTHHRS,ZEROS                                                     
         STC   R5,MTHNUM                                                        
         MVC   WORK(4),WORK+6                                                   
         BCT   R0,REQF40                                                        
*                                                                               
REQF50   STC   R5,RQMNTHS                                                       
         LR    R0,R5               NUMBER OF MONTHS IN REQUEST                  
         USING MTHD,R3             LIST OF PACKED MONTHS                        
         LA    R3,MNTHS            FROM QSTART THRU QEND                        
         USING QTRD,R4                                                          
REQF60   L     R4,QTRMTAB          UPDATE QTR TABLE                             
REQF70   CLI   QTRMTH,EOT          MUST FIND A MONTH MATCH                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   MTHMNTH,QTRMTH                                                   
         BE    REQF80                                                           
         LA    R4,QTRDLN(R4)                                                    
         B     REQF70                                                           
*                                                                               
REQF80   LA    R2,QTRMNAME                                                      
         CLI   0(R2),X'40'         FIND SPACE AND PLUG YR IN AFTER IT           
         BE    *+12                                                             
         LA    R2,1(R2)                                                         
         B     *-12                                                             
*                                                                               
         MVC   WORK(L'MTHCODE),MTHCODE    GET YEAR FROM DATCON                  
         MVI   WORK+L'MTHCODE,X'01'       DATCON NEEDS DAY                      
         GOTO1 DATCON,DMCB,(1,WORK),(20,WORK+6)                                 
         MVC   1(4,R2),WORK+6                                                   
*                                                                               
         MVI   QTRNUMB,1           FIGURE OUT QTR NUMBER                        
         CLI   MTHNUM,3                                                         
         BNH   REQF90                                                           
         MVI   QTRNUMB,2                                                        
         CLI   MTHNUM,6                                                         
         BNH   REQF90                                                           
         MVI   QTRNUMB,3                                                        
         CLI   MTHNUM,9                                                         
         BNH   REQF90                                                           
         MVI   QTRNUMB,4                                                        
REQF90   LA    R3,MTHLEN(R3)       FROM QSTART THRU QEND                        
         BCT   R0,REQF60                                                        
         DROP  R4                                                               
         MVC   SVMNTHS,MNTHS                                                    
         MVC   SRQMNTHS,RQMNTHS    RESTORE MNTHS TABLE                          
*                                                                               
         MVI   METHOD,C'1'         *** SET METHOD TO DEFAULT ****               
         CLI   QMTHD,SPACE                                                      
         BNH   *+10                                                             
         MVC   METHOD,QMTHD        METHOD FROM REQUEST                          
*                                                                               
         MVC   RQDETAIL,XSPACES    CLEAR REQUEST OFF/CLI/PRD/DPT/GRP            
         CLC   QAPPL+6(6),XSPACES  ANY GROUP SPECIFIED?                         
         BNH   *+10                                                             
         MVC   RQGRP,QAPPL+6       SAVE OFF REQUESTED GROUP IF ANY              
*                                                                               
         CLC   QSELECT,XSPACES    MUST SPECIFY TOYOTA DIVISION TMS/LEX          
         BH    *+14                                                             
         MVC   RQCLIENT,=C'TMS'   DEFAULT IS TOYOTA                             
         B     REQF120                                                          
*                                                                               
         LA    R1,CLITAB           MAKE SURE CLIENT IS VALID                    
REQF100  CLI   0(R1),EOF                                                        
         BNE   REQF110                                                          
         OI    FLAG,FLGICLI        INVALID CLIENT                               
*                                                                               
         MVC   XP+6(17),=CL17'CLIENT IS INVALID'                                
         MVC   XPSECOND+6(34),=CL34'TMS AND LEX ARE ONLY VALID CLIENTS'         
         GOTO1 ACREPORT                                                         
         MVI   FCRDACC,C'N'        DONT READ ANY ACCOUNTS                       
         B     EXIT                                                             
*                                                                               
REQF110  CLC   QSELECT(3),0(R1)                                                 
         BE    *+12                                                             
         LA    R1,L'CLITAB(R1)                                                  
         B     REQF100                                                          
*                                                                               
         LA    R1,L'RQCLIPRD-1     SET LENGTH FOR EX MVC-CLI AND PROD           
         LA    RE,RQCLIENT         ASSUME THAT QSELECT HAS CLI/PROD             
         LA    RF,QSELECT          SO POINT TO START OF BOTH FIELDS             
         CLC   QSELECT(3),XSPACES  DOES QSELECT HAVE A CLIENT?                  
         BH    *+16                YES - MOVE IN CLIENT AND/OR PRODUCT          
         LA    R1,L'RQCLIENT-1     SET LENGTH FOR EX MVC-CLI OR PROD            
         LA    RE,RQPROD           ELSE POINT RE AND RF TO PROD                 
         LA    RF,L'RQCLIENT(RF)                                                
         EX    R1,*+4                                                           
         MVC   0(0,RE),0(RF)       SAVE CLIENT AND/OR PRODUCT                   
*                                                                               
         CLC   RQPROD,XSPACES      IF PROD WAS REQUESTED GET NAME               
         BE    REQF120                                                          
*                                                                               
         USING ACTRECD,R2                                                       
         LA    R2,DKEY                                                          
         MVC   ACTKEY,XSPACES                                                   
         MVC   ACTKCPY,RCCOMPFL                                                 
         MVC   ACTKUNT(2),=C'SJ'               GET PRODUCT NAME                 
         MVC   ACTKACT(L'RQCLIPRD),RQCLIPRD    READ FOR REQ CLI/PRD             
         BAS   RE,RHIGH                                                         
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,AIO                                                           
         BAS   RE,GET                                                           
*                                                                               
         USING NAMELD,R4                                                        
         LA    R4,ACTRFST                                                       
         CLI   NAMEL,NAMELQ                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SH    R1,=Y(NAMLN1Q+1)                                                 
         EX    R1,*+4                                                           
         MVC   SVPRDNM(0),NAMEREC  MOVE PROD NAME INTO SAVED AREA               
         DROP  R4                                                               
*                                                                               
REQF120  CLC   QOFFICE,XSPACES     1C OFFICE SPECIFIED?                         
         BNH   *+10                                                             
         MVC   RQOFFICE,QOFFICE    SAVE 1C OFFICE                               
         CLC   QAPPL,XSPACES       APG DEPT SPECIFIED?                          
         BNH   REQF130                                                          
         MVC   RQDEPT,QAPPL        APG DEPT                                     
         MVI   MASK,EQUAL                                                       
         CLI   QAPPL,C'*'          ALL ACCEPT?                                  
         BNE   REQF130                                                          
         MVC   RQDEPT,QAPPL+1      APG DEPT                                     
         MVI   MASK,NOTEQUAL                                                    
*                                                                               
REQF130  L     RF,SALAREA                                                       
         USING SALARYD,RF                                                       
         XC    WORK,WORK                                                        
         MVC   WORK(8),SALBLOCK+8   ADDR AND LENGTH OF ACQUIRED BUFFER          
         XC    SALBLOCK(SALLNQ),SALBLOCK                                        
         MVC   SALBLOCK+8(8),WORK   RESTORE ADDR/LEN OF BUFFER                  
         MVC   SALCMPY,RCCOMPFL                                                 
         MVC   SALMETHD,METHOD      COST METHOD                                 
         MVC   SALACOVL,COVAIL                                                  
         OI    SALSTAT1,SALHRRAT   TURN ON INCLUDE HOURLY RATES                 
         LA    R0,SALACTM#         NUMBER OF MONTHLY HRLY ACCUMS                
         LA    R1,SALACTM1                                                      
         ZAP   0(L'SALACTM1,R1),ZEROS                                           
         LA    R1,L'SALACTM1(R1)                                                
         BCT   R0,*-10                                                          
*                                                                               
         LA    R1,SRKLNQ           KEY FOR SORT      INIT FOR SORT **           
         CVD   R1,DUB              CONVERT KEY LEN TO CHARS                     
         OI    DUB+7,X'0F'                                                      
         UNPK  SORTCARD+15(3),DUB+6(2)                                          
         LA    R1,SRTLEN           SORT RECORD LENGTH                           
         CVD   R1,DUB              CONVERT REC LEN TO CHARS                     
         OI    DUB+7,X'0F'                                                      
         UNPK  RECCARD+22(4),DUB+5(3)                                           
         GOTO1 ADSORTER,DMCB,SORTCARD,RECCARD,0                                 
*                                                                               
RQFX     B     EXIT                                                             
         DROP  R2,R3,RF                                                         
         EJECT                                                                  
**********************************************************************          
* FIRST FOR LEDGER                                                   *          
**********************************************************************          
         SPACE 1                                                                
LDGF     DS    0H                                                               
         L     RF,ADLDGHIR                                                      
         USING ACLELD,RF                                                        
         MVC   LLEVA(LLEVELLN),ACLVALS     LEVEL LENGTHS/NAMES                  
         LA    R3,LENLEVLS                 INDIVIDUAL LENGTHS OF LEVELS         
         SR    R4,R4                                                            
         LA    R1,LLEVA                    COMBINED LEVEL LENGTHS               
         LA    R0,LLEVLNUM                 MAXIMUM NUMBER OF LEVELS             
LDGF10   DS    0H                                                               
         SR    R5,R5                                                            
         IC    R5,0(R1)                    PREVIOUS COMBINED LENGTH             
         SR    R5,R4                       MINUS NEW COMBINED LENGTH            
         BP    *+6                         EQUALS INDIVIDUAL LEVEL LEN          
         DC    H'0'                                                             
         STC   R5,0(R3)                    SAVE INDIVD LENGTH OF LEVEL          
         CLI   0(R1),MAXLEN                LAST LEV HAS MAXLEN FOR ACCT         
         BE    LDGF20                                                           
         SR    R4,R4                                                            
         IC    R4,0(R1)                    COMBINED LENGTH IN R4                
         LA    R1,LLEVALN(R1)              BUMP TO NEXT COMBINED LENGTH         
         LA    R3,L'LENLEVA(R3)            NEXT INDIVDUAL LEN SAVE AREA         
         BCT   R0,LDGF10                                                        
         DC    H'0'                                                             
*                                                                               
         USING SALARYD,RF                                                       
LDGF20   L     RF,SALAREA                                                       
         MVC   SALLEVS,LENLEVLS            SET 1R LEVELS FOR SALHST             
         B     EXIT                                                             
         DROP  RF                                                               
         EJECT                                                                  
**********************************************************************          
* FIRST FOR LEVEL A                                                  *          
**********************************************************************          
         SPACE 1                                                                
LEVAF    DS    0H                                                               
         MVC   OFFIC,XSPACES                                                    
         MVC   OFFICN,XSPACES                                                   
         USING NAMELD,R4                                                        
         L     R4,ADLVANAM         ADDR OF NAME EL                              
         SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SH    R1,=Y(NAMLN1Q+1)                                                 
         EX    R1,*+4                                                           
         MVC   OFFICN(0),NAMEREC                                                
         L     R2,ADHEIRA                                                       
         SR    R1,R1                                                            
         IC    R1,LENLEVA          LENGTH OF OFFICE                             
         SH    R1,=H'1'                                                         
         EX    R1,*+4                                                           
         MVC   OFFIC(0),3(R2)      SAVE OFFICE CODE FOR SALHST                  
         MVC   WORK,XSPACES                                                     
         LA    RF,WORK                                                          
         USING NMED,RF             SAVE NAME FOR REPORTING                      
         MVC   NMEREC(L'OFFIC),OFFIC                                            
         MVC   NMENAME,OFFICN                                                   
         GOTO1 BINADD,DMCB,WORK,NMEBUFF                                         
         L     RF,SALAREA                                                       
         USING SALARYD,RF                                                       
         MVC   SALOFFC,XSPACES                                                  
         MVC   SALOFFC,OFFIC                                                    
         B     EXIT                                                             
         DROP  R4,RF                                                            
         EJECT                                                                  
**********************************************************************          
* FIRST FOR LEVEL B                                                  *          
**********************************************************************          
         SPACE 1                                                                
LEVBF    DS    0H                                                               
         MVC   DEPART,XSPACES                                                   
         MVC   DPTCODE,XSPACES                                                  
         MVC   DPTNAME,XSPACES                                                  
         USING NAMELD,R4                                                        
         L     R4,ADLVBNAM          ADDR OF NAME EL                             
         SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SH    R1,=Y(NAMLN1Q+1)                                                 
         EX    R1,*+4                                                           
         MVC   DPTNAME(0),NAMEREC                                               
*                                                                               
         L     R2,ADHEIRB                                                       
         LA    R2,3(R2)                                                         
         MVC   DPTCODE,0(R2)       OFFICE / DEPT                                
         MVC   WORK,XSPACES                                                     
         LA    RF,WORK                                                          
         USING NMED,RF             SAVE NAME FOR REPORTING                      
         MVC   NMEREC(L'DPTCODE),DPTCODE                                        
         MVC   NMENAME,DPTNAME                                                  
         GOTO1 BINADD,DMCB,WORK,NMEBUFF                                         
         SR    R0,R0                                                            
         IC    R0,LENLEVA                                                       
         AR    R2,R0               R2 NOW POINTS TO DEPT                        
         SR    R1,R1                                                            
         IC    R1,LENLEVB                                                       
         SH    R1,=H'1'                                                         
         EX    R1,*+4                                                           
         MVC   DEPART(0),0(R2)     SAVE DEPT CODE                               
         L     RF,SALAREA                                                       
         USING SALARYD,RF                                                       
         MVC   SALDEPT,XSPACES                                                  
         MVC   SALDEPT(L'DEPART),DEPART      SAVE FOR SALARY LOOKUP             
         B     EXIT                                                             
         DROP  R4,RF                                                            
         EJECT                                                                  
**********************************************************************          
* FIRST FOR LEVEL C                                                  *          
**********************************************************************          
         SPACE 1                                                                
LEVCF    DS    0H                                                               
         MVC   SUBCODE,XSPACES                                                  
         MVC   SUBNAME,XSPACES                                                  
         USING NAMELD,R4                                                        
         L     R4,ADLVCNAM          ADDR OF NAME EL                             
         SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SH    R1,=Y(NAMLN1Q+1)                                                 
         EX    R1,*+4                                                           
         MVC   SUBNAME(0),NAMEREC                                               
         L     R2,ADHEIRC                                                       
         LA    R2,3(R2)                                                         
         SR    R0,R0                                                            
         IC    R0,LLEVB                                                         
         AR    R2,R0               R2 NOW POINTS TO SUBDEPT                     
         SR    R1,R1                                                            
         IC    R1,LENLEVC                                                       
         SH    R1,=H'1'                                                         
         EX    R1,*+4                                                           
         MVC   SUBCODE(0),0(R2)    SAVE SUBDEPT CODE                            
         L     R2,ADHEIRC                                                       
         LA    R2,3(R2)                                                         
         LA    RF,WORK                                                          
         USING NMED,RF             SAVE NAME FOR REPORTING                      
         MVC   NMEREC,0(R2)                                                     
         MVC   NMENAME,SUBNAME                                                  
         GOTO1 BINADD,DMCB,WORK,NMEBUFF                                         
         L     RF,SALAREA                                                       
         USING SALARYD,RF                                                       
         MVC   SALSDPT,XSPACES                                                  
         MVC   SALSDPT(L'SUBCODE),SUBCODE     SAVE FOR SALARY LOOKUP            
         B     EXIT                                                             
         DROP  R4,RF                                                            
         EJECT                                                                  
**********************************************************************          
* FIRST FOR ACCOUNT                                                  *          
**********************************************************************          
         SPACE 1                                                                
PACC     DS    0H                                                               
         MVI   ACCSTUS,0                                                        
         MVI   FCRDHIST,C'N'                                                    
         MVC   MNTHS,SVMNTHS       RESTORE MNTHS TABLE                          
         MVC   RQMNTHS,SRQMNTHS    RESTORE MNTHS TABLE                          
*                                 CLEAR SORT RECORD AREA                        
*        GOTO1 CLRSORT,DMCB,ASRTREC                                             
*        USING SRTD,R5                                                          
*        L     R5,ASRTREC                                                       
*                                                                               
         USING ACTRECD,R6                                                       
         L     R6,ADACC                                                         
*        MVC   SR1R,ACTKCULA      1R KEY                                        
         L     R4,ADACCSTA                                                      
         USING RSTELD,R4                                                        
         CLI   QOPT5,C'S'         SUPPRESS LOCKED ACCOUNTS                      
         BNE   *+12                                                             
         TM    RSTSTAT,RSTSACIL                                                 
         BO    EXIT                                                             
         MVI   FCRDHIST,C'Y'                                                    
         MVC   PRSCODE,XSPACES                                                  
         MVC   PRSNAME,XSPACES                                                  
         USING NAMELD,R4                                                        
         L     R4,ADACCNAM          ADDR OF NAME EL                             
         SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SHI   R1,NAMLN1Q                                                       
         CHI   R1,L'NAMEREC-2     WE WANT ONLY 34 OR LESS CHARS OF NAME         
         BNH   *+8                                                              
         LHI   R1,34                                                            
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   PRSNAME+2(0),NAMEREC                                             
*                                                                               
         LA    RE,=C'3A1B'      MARK PERSONS IN 3A1B/4A1B AS FREELNCERS         
         CLC   =C'TMS',RQCLIENT    ARE WE DOING TOYOTA TMS                      
         BE    *+8                                                              
         LA    RE,=C'4A1B'         IF LEXUS MARK PEOPLE IN 4A1B                 
         CLC   DPTCODE,0(RE)                                                    
         BNE   *+10                                                             
         MVC   PRSNAME(2),=C'* '                                                
*                                                                               
         L     R2,ADACC                                                         
         LA    R2,3(R2)                                                         
         SR    R0,R0                                                            
         IC    R0,LLEVC                                                         
         AR    R2,R0                                                            
         SR    R1,R1                                                            
         IC    R1,LENLEVD                                                       
         SH    R1,=H'1'                                                         
         EX    R1,*+4                                                           
         MVC   PRSCODE(0),0(R2)     SAVE PERSON CODE                            
*                                                                               
         L     RF,SALAREA                                                       
         USING SALARYD,RF                                                       
         MVC   SALPRSN,XSPACES                                                  
         MVC   SALPRSN(L'PRSCODE),PRSCODE     SAVE FOR SALARY LOOKUP            
         LA    R0,SALACTM#         NUMBER OF MONTHLY HRLY ACCUMS                
         LA    R1,SALACTM1                                                      
         ZAP   0(L'SALACTM1,R1),ZEROS                                           
         LA    R1,L'SALACTM1(R1)                                                
         BCT   R0,*-10                                                          
         USING MTHD,RF                                                          
         LA    RF,MNTHS            CLEAR MONTHLY TABLE                          
         LA    R0,MAXMTHS                                                       
         ZAP   MTHHRS,ZEROS                                                     
         LA    RF,MTHLEN(RF)                                                    
         BCT   R0,*-10                                                          
         DROP  RF                                                               
*                                                                               
* INITIALIZE EFFECTIVE DATE TABLE AND TABLE ENTRY NUMBER                        
*                                                                               
         XC    EFFDTTAB(EFFDT1LN),EFFDTTAB   CLEAR EFF DATE TABLE               
*        XC    EFFSTR,EFFSTR                                                    
*        XC    EFFEND,EFFEND                                                    
         MVI   EFFDTNUM,0          SET TABLE ENTRY NUMBER TO 0                  
*                                                                               
         USING EFFDTD,RE                                                        
         LA    RE,EFFDTTAB         RE=A(EFFECTIVE DATE TABLE)                   
         SR    RF,RF                                                            
*                                                                               
         USING FFTELD,R1                                                        
         L     R1,ADACC            FIND FREE FORM ELEMENT                       
         LA    R1,ACCORFST(R1)     BUMP TO 1ST ELEMENT                          
PACC10   CLI   FFTEL,0                                                          
         BE    PACC30                                                           
         CLI   FFTEL,FFTELQ        X'DB' - FREEFORM TEXT ELEMENT                
         BNE   *+12                                                             
         CLI   FFTTYPE,FFTTFEER    X'DD' - FEE REPORT INCLUSION (221)           
         BE    PACC20                                                           
PACC15   SR    R0,R0                                                            
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     PACC10                                                           
*                                                                               
PACC20   AHI   RF,1                INCREMENT TABLE ENTRY NUMBER                 
         MVC   EFFDTSTR,FFTSTDT    SAVE OFF ACCOUNT START DATE                  
         MVC   EFFDTEND,FFTENDT    SAVE OFF CLIENT END DATE                     
         LA    RE,EFFDTLNQ(RE)     BUMP TO NEXT AVAILABLE TABLE ENTRY           
         CHI   RF,20               MAXIMUM NUMBER OF TABLE ENTRIES              
         BNE   PACC15                                                           
         DROP  R1,RE                                                            
*                                                                               
PACC30   STCM  RF,1,EFFDTNUM       STORE TABLE ENTRY NUMBER                     
*        BZ    *+8                 IF THERE ARE EFFECTIVE DATES CHANGE          
         LTR   RF,RF                                                            
         BZ    *+8                 IF THERE ARE EFFECTIVE DATES CHANGE          
         BAS   RE,FIXMTHTB         THE MTH TABLE ACCORDINGLY                    
         GOTO1 CLRSORT,DMCB,ASRTREC                                             
         USING SRTD,R5                                                          
         L     R5,ASRTREC                                                       
         MVC   SR1R,ACTKCULA      1R KEY                                        
         BAS   RE,LOOKUP           LOOKUP APG CODE                              
         MVC   SRDEPT,WORK         APG DEPT                                     
         MVC   SROFFICE,DPTCODE    OFFICE/DEPT OF PERSON                        
         MVC   SRTITLE,SUBCODE     TITLE                                        
         MVC   SRPERSON,PRSCODE    PERSON CODE                                  
         CLC   RQDEPT,XSPACES                                                   
         BNH   ACFX                                                             
*                                                                               
         MVC   ACFXBC+1(1),MASK    * MASK SETS CONDITION FOR BC                 
         CLC   RQDEPT,SRDEPT       REQUESTED APG DEPT?                          
ACFXBC   BC    0,ACFX              * EITHER "BE" "BNE" SET BY MASK              
         MVI   FCRDHIST,C'N'                                                    
*                                                                               
ACFX     DS    0H                                                               
         B     EXIT                                                             
         DROP  R4,R5,R6                                                         
         EJECT                                                                  
**********************************************************************          
* FIRST FOR COUNTRA-ACCOUNT                                          *          
**********************************************************************          
         SPACE 1                                                                
PSUB     DS    0H                                                               
         USING SRTD,R5                                                          
         L     R5,ASRTREC                                                       
         OI    ACCSTUS,WANTIT      SET TO "WE WANT THIS ACCOUNT"                
         XC    SVGRP,SVGRP         CLEAR SAVED AREA FOR GROUP                   
*                                                                               
         L     R6,ADSUBAC                                                       
         SH    R6,DATADISP         ADDR OF CONTRA RECORD                        
         USING CACRECD,R6                                                       
         CLC   NONCLI,CACKCUNT     TEST FOR C'1N' CONTRA                        
         BNE   *+12                                                             
         NI    ACCSTUS,ALL-WANTIT  SET TO "DON'T WANT THIS ACCT"                
         B     PSUBX                                                            
*                                                                               
         LA    RF,CACKCACT         RF = A(OFFICE)                               
         SR    R1,R1                                                            
         IC    R1,OFFLN                                                         
         AR    RF,R1               POINT RF PAST OFFICE                         
*                                                                               
         CLC   RQCLIENT,=C'LEX'    ONLY FOR LEXUS                               
         BNE   PSUB20                                                           
         CLC   =C'LDA',0(RF)       IS THE CLIENT CODE LDA-CREATIVE??            
         BNE   PSUB20              NO - DON'T BOTHER CHECKING THE REST          
*                                                                               
         USING BIND,RE                                                          
         L     RE,APRDBUFF         R1=PRODUCT TABLE                             
         ICM   R0,15,BININ                                                      
         BZ    PSUB20                                                           
         USING PRDTBD,R1                                                        
         LA    R1,BINTABLE                                                      
PSUB10   CLC   PRDCP,0(RF)         IS THIS ONE OF THE CLI/PRD                   
         BNE   *+14                                                             
         MVC   SVGRP,PRDGRP        SAVE GROUP FROM TABLE                        
         B     PSUB20                                                           
         LA    R1,PRDLNQ(R1)                                                    
         BCT   R0,PSUB10                                                        
         NI    ACCSTUS,ALL-WANTIT  SET TO "DON'T WANT THIS ACCT"                
         B     PSUBX                                                            
         DROP  R1,RE                                                            
*                                                                               
PSUB20   DS    0H                                                               
         NI    ACCSTUS,ALL-REQCLI     SET TO "NOT THE REQUESTED CLI"            
         NI    ACCSTUS,ALL-REQPRD     SET TO "NOT THE REQUESTED PRD"            
         NI    ACCSTUS,ALL-REQLDA     SET TO "NOT THE LDA CREATIVE "            
         CLI   RQOFFICE,SPACE         1C OFFICE REQUEST?                        
         BE    PSUB30                 NO --                                     
         SR    R1,R1                                                            
         IC    R1,OFFLN                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   CACKCACT(0),RQOFFICE                                             
         BNE   PSUBX               NOT THE REQUESTED 1C OFFICE/CLIENT           
*                                                                               
PSUB30   LA    RF,CACKCACT         RF = A(OFFICE)                               
         SR    R1,R1                                                            
         IC    R1,OFFLN                                                         
         AR    RF,R1               POINT RF PAST OFFICE                         
*                                                                               
         CLC   RQCLIENT,0(RF)      MATCH THE CLIENT AND PRODUCT                 
         BE    PSUB40                                                           
         CLC   RQCLIENT,=C'LEX'    ONLY FOR LEXUS                               
         BNE   PSUBX                                                            
         CLC   =C'LDA',0(RF)       IF CLIENT CODE IS LDA SET CLI BIT            
         BNE   PSUBX               BIT NOT COMPENSATED FOR LDA CREATIVE         
PSUB40   OI    ACCSTUS,REQCLI      MARK AS REQUESTED CLIENT                     
         CLC   =C'LDA',0(RF)       IF CLIENT CODE IS LDA DON'T SET PRD          
         BE    PSUB50                                                           
*                                                                               
         CLC   RQPROD,XSPACES      DO WE HAVE A PRODUCT???                      
         BNH   PSUB50                                                           
         CLC   RQPROD,3(RF)        MATCH THE PRODUCT                            
         BNE   PSUBX                                                            
         OI    ACCSTUS,REQPRD      MARK AS REQUESTED CLIENT/PROD                
         B     PSUBX                                                            
*                                                                               
         USING BIND,RE                                                          
PSUB50   L     RE,APRDBUFF         R1=PRODUCT TABLE                             
         ICM   R0,15,BININ                                                      
         BZ    PSUBX                                                            
         USING PRDTBD,R1                                                        
         LA    R1,BINTABLE                                                      
PSUB60   CLC   PRDCP,0(RF)         IS THIS ONE OF THE CLI/PRD                   
         BNE   PSUB80                                                           
         MVC   SVGRP,PRDGRP        SAVE GROUP FROM TABLE                        
         TM    FLAG2,FLGFH         ARE WE READING FH                            
         BNO   PSUB70                                                           
         CLC   PRDGRP,=C'50'       LDA NOT INCLUDED IN PROD HOURS               
         BNE   PSUB70                                                           
         OI    ACCSTUS,REQLDA      MARK AS LDA CREATIVE                         
         B     PSUBX                                                            
PSUB70   OI    ACCSTUS,REQPRD      MARK AS REQUESTED CLIENT/PROD                
         B     PSUBX                                                            
PSUB80   LA    R1,PRDLNQ(R1)                                                    
         BCT   R0,PSUB60                                                        
         DROP  R1,RE                                                            
*                                                                               
PSUBX    B     EXIT                                                             
         DROP  R5,R6                                                            
         EJECT                                                                  
**********************************************************************          
* PROCESS HISTORY BUCKETS                                            *          
**********************************************************************          
         SPACE 1                                                                
PHST     DS    0H                                                               
         TM    ACCSTUS,WANTIT                                                   
         BZ    PHSTX                                                            
         L     R6,ADSUBAC          CONTRA RECORD                                
         SH    R6,DATADISP                                                      
         USING CACRECD,R6                                                       
         CLI   CACKBTYP,FEES       GET RID FEE BUCKETS                          
         BE    PHSTX                                                            
*                                                                               
         USING BUKELD,R4                                                        
         L     R4,ADTRANS                                                       
         CLI   0(R4),BUKELQ        MAKE SURE IT'S A HISTORY BUCKET              
         BNE   PHSTX                                                            
         CLC   BUKMOS,START        MAKE SURE IT IS IN DATE RANGE                
         BL    PHSTX                                                            
         CLC   BUKMOS,END                                                       
         BH    PHSTX                                                            
*                                                                               
         USING EFFDTD,R1                                                        
         LA    R1,EFFDTTAB         R1=A(EFFECTIVE DATE TABLE)                   
         SR    R0,R0                                                            
         ICM   R0,1,EFFDTNUM       IF TABLE IS EMPTY-NO NEED TO FILTER          
         BZ    PHST05                                                           
*                                                                               
PHST01   CLC   BUKMOS,EFFDTSTR     IF EFFECTIVE DATES-MAKE SURE THE             
         BL    *+14                   BUCKETS ARE WITHIN AT LEAST               
         CLC   BUKMOS,EFFDTEND        ONE OF THE DATE RANGES                    
         BNH   PHST05                                                           
*                                                                               
         LA    R1,EFFDTLNQ(R1)     BUMP TO NEXT TABLE ENTRY                     
         BCT   R0,PHST01                                                        
         B     PHSTX               NOT WITHING ANY DATE RANGE - EXIT            
         DROP  R1                                                               
*                                                                               
*        MVC   MSG1,=CL10'HISTORY-1'                                            
*        GOTO1 ADUMP,DMCB,(RC),(R4),16                                          
*                                                                               
PHST05   CLI   CACKBTYP,BENEFIT    REVERSE BENEFIT BUCKETS                      
         BNE   *+10                                                             
         MP    BUKDR,=P'-1'                                                     
         CLI   CACKBTYP,HOURS                                                   
         BE    PHST10                                                           
         CLI   BUKMOS,X'95'                                                     
         BNE   PHST10                                                           
         CLC   CACKBTYP(L'METHOD),METHOD  DOLLARS FOR METHOD BUCKETS            
         BNE   PHSTX                                                            
         CLI   CACKSTYP,SALARY            SALARY $'S FOR METHOD ONLY            
         BNE   PHSTX                                                            
*                                                                               
PHST10   DS    0H                                                               
         OI    ACCSTUS,ACTIVE      SET THIS PERSON ACTIVE IN REQUEST            
*                                                                               
         USING GRPD,RE                                                          
         LA    RE,GRPWRK                                                        
         USING GRPDTD,R2                                                        
         LA    R2,GRPDTL                 GROUP DETAIL SECTION                   
*                                                                               
         MVC   GRPKEY(GRPKLEN),XSPACES   CLEAR DATA FIELDS                      
         XC    GRPSTAT,GRPSTAT           CLEAR STATUS BYTE                      
         LA    RF,GRPBKDSP(RE)           ZAP BUCKETS TO ZERO                    
         LA    R0,GRPNUMBK                                                      
         ZAP   0(SDBUKLN,RF),=P'0'                                              
         LA    RF,SDBUKLN(RF)                                                   
         BCT   R0,*-10                                                          
*                                                                               
         USING SRTD,R5                                                          
         L     R5,ASRTREC                                                       
         USING SDTD,R3                                                          
         LA    R3,SRDTL                                                         
*                                                                               
         MVC   GRPGRP,SVGRP        FILL IN GROUP                                
         MVC   GRPLOC,SRACC        FILL IN DEPT/OFF/TITLE                       
         CLC   GRPPERS,=C'CC1026'                                               
         BNE   *+10                                                             
         MVC   GRPPERS,=C'800025'                                               
         DROP  RE                                                               
*                                                                               
         LA    R0,MAXMTHS          NUMBER OF MONTHS                             
PHST20   CLC   BUKMOS,SDMNTH       MATCH ON MONTH                               
         BE    PHST30                                                           
         LA    R3,SDLEQ(R3)                                                     
         LA    R2,GRPDTLQ(R2)      BUMP GROUP TABLE FOR THE RIGHT MNTH          
         BCT   R0,PHST20                                                        
         DC    H'0'                HAVE TO FIND MONTH                           
*                                                                               
PHST30   MVI   BYTE,X'00'                                                       
         SR    R1,R1                                                            
         IC    R1,SDMNUM           CURRENT MONTH NUMBER                         
         SR    R0,R0                                                            
         IC    R0,RQMNTHS          NUMBER OF MONTHS IN REQUEST                  
         SR    R0,R1                                                            
         LTR   R0,R0               NUMBER OF YTD BUCKETS TO UPDATE              
         BNP   *+8                                                              
         STC   R0,BYTE             USE BYTE AS COUNTER                          
         CLI   BUCKTYPE,HOURS      HOURS                                        
         BNE   PHST90                                                           
*                                  ******HOUR BUCKETS*******                    
         AP    SDTHRS,BUKCR        ADD TO TOTAL HOURS (MONTH)                   
         AP    SDYTHRS,BUKCR       ADD TO TOTAL HOURS (YTD)                     
         ZAP   GRPTHRS,BUKCR       ADD TO TOTAL HOURS (MONTH)                   
         ZAP   GRPYTHRS,BUKCR      ADD TO TOTAL HOURS (YTD)                     
*                                                                               
         USING MTHD,RF                                                          
         LA    RF,MNTHS            MONTHLY TABLE                                
         LA    R0,MAXMTHS                                                       
*                                                                               
         CLC   BUKMOS,MTHCODE      MONTH EQUAL ADD IN HRS                       
         BE    *+14                                                             
         LA    RF,MTHLEN(RF)                                                    
         BCT   R0,*-14                                                          
         DC    H'0'                MUST FIND THE MONTH                          
         AP    MTHHRS,BUKCR                                                     
         DROP  RF                                                               
*                                                                               
         TM    ACCSTUS,REQCLI      REQUESTED CLIENT?                            
         BZ    PHST50                                                           
*        MVC   MSG1,=CL10'HISTORY-2'                                            
*        GOTO1 ADUMP,DMCB,(RC),(R4),16                                          
         CLC   BUKMOS,SVSTART      CHECK TO SEE IF WITHIN REQ RANGE             
         BL    *+8                                                              
         OI    ACCSTUS,REQCLI1     REQUESTED CLIENT AT LEAST ONCE               
         AP    SDCLIHR,BUKCR       ADD TO CLIENT HOURS MONTH AND YTD            
         ZAP   GRPCLIHR,BUKCR      ADD TO CLIENT HOURS MONTH AND YTD            
         AP    SDYCHRS,BUKCR                                                    
         ZAP   GRPYCHRS,BUKCR                                                   
         TM    ACCSTUS,REQPRD      REQUESTED PRODUCT?                           
         BZ    PHST40                                                           
         AP    SDPRDHR,BUKCR       ADD TO PROD HOURS MONTH AND YTD              
         ZAP   GRPPRDHR,BUKCR      ADD TO PROD HOURS MONTH AND YTD              
         AP    SDYPHRS,BUKCR                                                    
         ZAP   GRPYPHRS,BUKCR                                                   
PHST40   TM    ACCSTUS,REQLDA      IS THIS AN LDA CREATIVE?                     
         BZ    PHST50                                                           
         ZAP   GRPLDAHR,BUKCR      ADD TO LDA HOURS MONTH AND YTD               
         ZAP   GRPYLHRS,BUKCR                                                   
*                                                                               
PHST50   SR    R0,R0                                                            
         IC    R0,BYTE             MORE YTD MONTHS TO UPDATE?                   
         LTR   R0,R0                                                            
         BZ    PHST110                                                          
PHST60   LA    R3,SDLEQ(R3)        NEXT MONTH BUCKET LINE IN SORT REC           
         LA    R2,GRPDTLQ(R2)      NEXT MONTH BUCKET LINE IN SORT REC           
         AP    SDYTHRS,BUKCR       ADD TO TOTAL YTD HOURS NEXT MONTH            
         ZAP   GRPYTHRS,BUKCR      ADD TO TOTAL YTD HOURS NEXT MONTH            
         TM    ACCSTUS,REQCLI      REQUESTED CLIENT?                            
         BZ    PHST80                                                           
         CLC   BUKMOS,SVSTART      CHECK TO SEE IF WITHIN REQ RANGE             
         BL    *+8                                                              
         OI    ACCSTUS,REQCLI1     REQUESTED CLIENT AT LEAST ONCE               
         AP    SDYCHRS,BUKCR                                                    
         ZAP   GRPYCHRS,BUKCR                                                   
         TM    ACCSTUS,REQPRD      REQUESTED PRODUCT?                           
         BZ    PHST70                                                           
         AP    SDYPHRS,BUKCR                                                    
         ZAP   GRPYPHRS,BUKCR                                                   
PHST70   TM    ACCSTUS,REQLDA      LDA CREATIVE?                                
         BZ    PHST80                                                           
         ZAP   GRPYLHRS,BUKCR                                                   
PHST80   BCT   R0,PHST60                                                        
         B     PHST110                                                          
*                                                                               
* ******DOLLAR BUCKETS*******                                                   
*                                                                               
PHST90   TM    ACCSTUS,REQCLI      REQUESTED CLIENT?                            
         BZ    PHST110                                                          
         AP    SDPST,BUKDR         ADD TO MONTHLY POSTING                       
         ZAP   GRPPST,BUKDR        ADD TO MONTHLY POSTING                       
         AP    SDYPST,BUKDR        ADD TO YTD POSTING(FOR THE MONTH)            
         ZAP   GRPYPST,BUKDR       ADD TO YTD POSTING(FOR THE MONTH)            
         SR    R0,R0                                                            
         IC    R0,BYTE             MORE YTD MONTHS TO UPDATE?                   
         LTR   R0,R0                                                            
         BZ    PHST110                                                          
PHST100  LA    R3,SDLEQ(R3)        NEXT MONTH BUCKET LINE IN SORT REC           
         LA    R2,GRPDTLQ(R2)      NEXT MONTH BUCKET LINE IN SORT REC           
         AP    SDYPST,BUKDR        ADD TO TOTAL YTD POSTING NEXT MONTH          
         ZAP   GRPYPST,BUKDR       ADD TO TOTAL YTD POSTING NEXT MONTH          
         BCT   R0,PHST100                                                       
*                                                                               
PHST110  DS    0H                                                               
         TM    ACCSTUS,ACTIVE      NO ACTIVITY FOR THIS EMPLOYEE                
         BZ    PHSTX                                                            
*                                                                               
         BAS   RE,PSTGRP                                                        
*                                                                               
PHSTX    B     EXIT                                                             
         DROP  R3,R4,R5,R6                                                      
         EJECT                                                                  
**********************************************************************          
* END OF A PERSON PUT RECORD TO SORT                                 *          
**********************************************************************          
         SPACE 1                                                                
ACCL     DS    0H                                                               
         TM    ACCSTUS,ACTIVE      NO ACTIVITY FOR THIS EMPLOYEE                
         BZ    ACCLX                                                            
         TM    ACCSTUS,REQCLI1     REQUESTED CLIENT AT LEAST ONCE               
         BZ    ACCLX                                                            
*                                                                               
         USING SRTD,R5                                                          
         L     R5,ASRTREC                                                       
         LA    RF,WORK                                                          
         USING NMED,RF             SAVE NAME FOR REPORTING                      
         MVC   NMEREC,SR1R+3                                                    
         MVC   NMENAME,PRSNAME                                                  
         CLC   SRPERSON,=C'CC1026'                                              
         BNE   ACCL10                                                           
         LA    R2,NMEREC                                                        
         SR    R0,R0                                                            
         IC    R0,LLEVC                                                         
         AR    R2,R0                                                            
         MVC   0(6,R2),=C'800025'                                               
ACCL10   GOTO1 BINADD,DMCB,WORK,NMEBUFF                                         
         DROP  RF                                                               
*                                                                               
         USING SDTD,R3                                                          
         LA    R3,SRDTL                                                         
         SR    R0,R0                                                            
         IC    R0,RQMNTHS                                                       
ACCL20   CP    SDCLIHR,ZEROS       ANY TOYOTA STUFF AL ALL?                     
         BNE   ACCL30                                                           
         CP    SDYCHRS,ZEROS                                                    
         BNE   ACCL30                                                           
         CP    SDPST,ZEROS                                                      
         BNE   ACCL30                                                           
         CP    SDYPST,ZEROS                                                     
         BNE   ACCL30                                                           
         LA    R3,SDLEQ(R3)        NEXT MONTH BUCKET LINE IN SORT REC           
         BCT   R0,ACCL20                                                        
         B     EXIT                                                             
*                                                                               
ACCL30   DS    0H                                                               
         BAS   RE,SALUP            LOOKUP SALARY                                
         CLC   SRPERSON,=C'CC1026'                                              
         BNE   ACCL40                                                           
         MVC   SRPERSON,=C'800025'                                              
         LA    R2,SR1R                                                          
         LA    R2,3(R2)                                                         
         SR    R0,R0                                                            
         IC    R0,LLEVC                                                         
         AR    R2,R0                                                            
         SR    R1,R1                                                            
         IC    R1,LENLEVD                                                       
         SH    R1,=H'1'                                                         
         EX    R1,*+4                                                           
         MVC   0(0,R2),SRPERSON                                                 
ACCL40   BAS   RE,SALGRP           ADD ONLY SALARY TO GROUP                     
         GOTO1 ADSORTER,DMCB,(L'PUTC,PUTC),(R5)                                 
         MVI   ACTIVITY,C'Y'                                                    
         CLC   SRDEPT,=C'00'       UNKNOWN                                      
         BE    ACCL50              DON'T POST TO DUP TABLE                      
         MVC   WORK,XSPACES                                                     
         LA    RF,WORK                                                          
         USING DUPD,RF             CHECK DUPLICATE LOCATION                     
         MVC   DUPPER,SRPERSON                                                  
         GOTO1 BINADD,DMCB,WORK,DUPBUFF                                         
         OC    ADDRSAV,ADDRSAV                                                  
         BZ    ACCL50                                                           
         L     RF,ADDRSAV                                                       
         OI    DUPSTAT,DUPLOC      MARK AS DUP                                  
*                                                                               
ACCL50   MVC   WORK,XSPACES                                                     
         LA    RF,WORK                                                          
         USING TITD,RF             UDPATE TITLE TABLE                           
         MVC   TINAME,XSPACES                                                   
         MVC   TIPREC,PRSCODE      AND PERSON TITLE                             
         MVC   TINAME,PRSNAME                                                   
         GOTO1 BINADD,DMCB,WORK,TIBUFF                                          
*                                                                               
ACCLX    B     EXIT                                                             
         DROP  R3,R5,RF                                                         
         EJECT                                                                  
**********************************************************************          
* REQUEST LAST                                                       *          
**********************************************************************          
         SPACE 1                                                                
REQL     DS    0H                                                               
         TM    FLAG,FLGICLI               INVALID CLIENT                        
         BO    EXIT                                                             
         GOTO1 REPORTS                    DO REPORTING                          
*                                                                               
         CLI   QOPT7,C'Y'                 WAS DOWNLOAD SELECTED                 
         BNE   REQL10                                                           
         TM    REPBYTE1,REPDWN             WAS ANYTHING DOWNLOADED?             
         BNO   REQL10                                                           
         LA    RE,XP                                                            
         LA    RF,SVPRNLNQ                                                      
         SR    R1,R1                                                            
         ICM   R1,8,=X'40'                                                      
         MVCL  RE,R0                                                            
         MVC   DWNFLD,XSPACES             SPACE OUT DOWNLOAD FIELD              
         GOTO1 ADWNL,DMCB,(RC),DWNEOR     DOWNLOAD EOR MARKER                   
*                                                                               
REQL10   GOTO1 ADSORTER,DMCB,(L'ENDC,ENDC)                                      
*                                                                               
         GOTO1 BUFFALO,DMCB,(L'CLEARC,CLEARC),ADBUFF,1,(X'80',2)                
         LA    R0,BUFDUP                                                        
         GOTO1 BUFFALO,DMCB,(L'CLEARC,CLEARC),((R0),ADBUFF),1,(X'80',2)         
         LA    R0,TCAPEQU                                                       
         GOTO1 BUFFALO,DMCB,(L'CLEARC,CLEARC),((R0),ADBUFF),1,(X'80',2)         
         LA    R0,TCQEQU                                                        
         GOTO1 BUFFALO,DMCB,(L'CLEARC,CLEARC),((R0),ADBUFF),1,(X'80',2)         
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* RUN LAST                                                           *          
**********************************************************************          
         SPACE 1                                                                
RUNL     DS    0H                                                               
         GOTO1 WRAP            RETURN ACQUIRED STORAGE                          
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* ROUTINE TO GET FISCAL START DATE                                   *          
**********************************************************************          
         SPACE 1                                                                
         USING NFISTABD,R3                                                      
*                                                                               
GETFIS   NTR1                                                                   
         L     R3,ANFISTAB                                                      
GETF10   CLC   START,NFEND         DOES START FALL IN THIS TABLE ENTRY          
         BNH   GETF20                                                           
         LA    R3,NFISTABQ(R3)     BUMP TO NEXT RANGE IN TABLE                  
         B     GETF10                                                           
*                                                                               
GETF20   DS    0H                                                               
         CLC   START+1(1),NFSTRMM  CHECK WHICH FISC MONTH ARE WE DOING          
         BNL   GETF30              IF IT'S LOW THAN IT'S THE NEXT YR            
         MVC   FISCEND,END                                                      
         MVC   FISCEND+1(1),NFENDMM                                             
         MVC   WORK(4),QSTART                                                   
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 ADDAY,DMCB,(C'Y',WORK),WORK+6,-1                                 
         GOTO1 DATCON,DMCB,(0,WORK+6),(1,WORK)                                  
         MVC   FISCSTR,WORK                                                     
         MVC   FISCSTR+1(1),NFSTRMM                                             
*                                                                               
         CLC   START(1),END        MATCH ON YEARS                               
         BE    GETF40              IF NOT EQUAL 2 YRS DIFFERENCE                
*        CLC   END+1(1),NFENDMM                                                 
*        BNL   GETF40                                                           
*                                                                               
         MVC   WORK(2),FISCEND     ADJUST END DATE'S YEAR                       
         MVI   WORK+2,X'01'                                                     
         GOTO1 DATCON,DMCB,(1,WORK),(0,WORK+6) WORK+6=C'YYMMDD'                 
         MVC   QEND(4),WORK+6      QEND=CL4'YYMM'                               
         MVC   WORK(4),QEND                                                     
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 ADDAY,DMCB,(C'Y',WORK),WORK+6,-1                                 
         GOTO1 DATCON,DMCB,(0,WORK+6),(1,WORK)                                  
         MVC   FISCEND,WORK        FISCEND WITH YR ADJUSTED                     
         B     GETF40                                                           
*                                                                               
GETF30   MVC   FISCSTR,START       FORCE FISCAL START MONTH                     
         MVC   FISCSTR+1(1),NFSTRMM X'10'                                       
         MVC   WORK(4),QSTART                                                   
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 ADDAY,DMCB,(C'Y',WORK),WORK+6,1                                  
         GOTO1 DATCON,DMCB,(0,WORK+6),(1,WORK)                                  
         MVC   FISCEND,WORK                                                     
         MVC   FISCEND+1(1),NFENDMM                                             
*                                                                               
GETF40   DS    0H                                                               
         ZAP   ADJHOURS,NFHOURS    MOVE NEW HOURS INTO ADJUSTED HOURS           
         ZAP   MANHOURS,NFMANHR    MOVE NEW TOTAL MANYEAR  HOURS                
*                                                                               
*        MVC   START,FISCSTR       ADJUST START DATE                            
*        MVC   WORK(2),START                                                    
*        MVI   WORK+2,X'01'                                                     
*        GOTO1 DATCON,DMCB,(1,WORK),(0,WORK+6) WORK+6=C'YYMMDD'                 
*        MVC   QSTART(4),WORK+6    QSTART=CL4'YYMM'                             
*                                                                               
         CLC   END,FISCEND                                                      
         BNH   GETFX                                                            
         MVC   END,FISCEND         ADJUST END DATE                              
         MVC   WORK(2),END                                                      
         MVI   WORK+2,X'01'                                                     
         GOTO1 DATCON,DMCB,(1,WORK),(0,WORK+6) WORK+6=C'YYMMDD'                 
         MVC   QEND(4),WORK+6      QSTART=CL4'YYMM'                             
*                                                                               
GETFX    B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
**********************************************************************          
* FIX MONTH TAB ACCORDING TO EFFECTIVE DATES                         *          
**********************************************************************          
         SPACE 1                                                                
         USING EFFDTD,R4                                                        
FIXMTHTB NTR1                                                                   
         LA    R4,EFFDTTAB         R1=A(EFFECTIVE DATE TABLE)                   
         SR    R2,R2                                                            
         ICM   R2,1,EFFDTNUM       IF TABLE IS EMPTY-NO NEED TO FILTER          
         BZ    FIXMX                                                            
*                                                                               
*        MVC   EFFSTR,EFFDTSTR                                                  
*        MVC   EFFEND,EFFDTEND                                                  
*        LA    R4,EFFDTLNQ(R4)                                                  
*        BCT   R2,*-10                                                          
*                                                                               
         USING MTHD,R3             BUILD LIST OF PACKED MONTHS                  
         LA    R3,MNTHS            FROM QSTART THRU QEND                        
         XC    MNTHS,MNTHS                                                      
         XC    BYTE,BYTE                                                        
*                                                                               
         LA    R0,MAXMTHS                                                       
         ZAP   MTHHRS,ZEROS        PUT ZEROS IN MTHHRS                          
         LA    R3,MTHLEN(R3)                                                    
         BCT   R0,*-10                                                          
         LA    R3,MNTHS            FROM QSTART THRU QEND                        
*                                                                               
FIXM10   CLC   START,EFFDTEND      FIND EFFECTIVE DATES WITH IN REQ             
         BH    FIXM50                                                           
         CLC   END,EFFDTSTR                                                     
         BL    FIXM50                                                           
*                                                                               
         LA    R0,MAXMTHS-1                                                     
         SR    R5,R5                                                            
         IC    R5,BYTE                                                          
         LA    R5,1(R5)                                                         
         STC   R5,MTHNUM                                                        
         MVC   WORK(4),YTDQSTR                                                  
         MVC   WORK+4(2),=C'01'                                                 
         MVC   MTHCODE,START       SET 1ST MONTH AS QSTART DATE                 
         CLC   EFFDTSTR,START      COMPARE QSTART TO EFFECTIVE STR DT           
         BNH   FIXM20                                                           
         MVC   MTHCODE,EFFDTSTR    SET 1ST MONTH AS EFFECTIVE ST DT             
         MVC   WORK+6(2),EFFDTSTR                                               
         MVI   WORK+8,X'01'                                                     
         GOTO1 DATCON,DMCB,(1,WORK+6),(0,WORK)                                  
*                                                                               
FIXM20   CLC   MTHCODE,EFFDTEND                                                 
         BE    FIXM40                                                           
         CLC   MTHCODE,END                                                      
         BE    FIXM40                                                           
         LA    R5,1(R5)            ADD NUMBER OF REQUEST MONTHS                 
         LA    R3,MTHLEN(R3)                                                    
         GOTO1 ADDAY,DMCB,(C'M',WORK),WORK+6,1   GET NEXT MONTH                 
         GOTO1 DATCON,DMCB,(0,WORK+6),(1,WORK+12)                               
         MVC   MTHCODE,WORK+12     PACKED                                       
         STC   R5,MTHNUM                                                        
         MVC   WORK(4),WORK+6                                                   
         BCT   R0,FIXM20                                                        
         B     *+8                                                              
*                                                                               
FIXM40   LA    R3,MTHLEN(R3)                                                    
         STC   R5,BYTE                                                          
*                                                                               
FIXM50   LA    R4,EFFDTLNQ(R4)     BUMP TO NEXT TABLE ENTRY                     
         BCT   R2,FIXM10                                                        
         B     FIXMX                                                            
*                                                                               
         STC   R5,RQMNTHS                                                       
         LR    R0,R5               NUMBER OF MONTHS IN REQUEST                  
         USING MTHD,R3             LIST OF PACKED MONTHS                        
         LA    R3,MNTHS            FROM QSTART THRU QEND                        
         USING QTRD,R4                                                          
FIXM60   L     R4,QTRMTAB          UPDATE QTR TABLE                             
FIXM70   CLI   QTRMTH,EOT          MUST FIND A MONTH MATCH                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   MTHMNTH,QTRMTH                                                   
         BE    FIXM80                                                           
         LA    R4,QTRDLN(R4)                                                    
         B     FIXM70                                                           
*                                                                               
FIXM80   MVI   QTRNUMB,1           FIGURE OUT QTR NUMBER                        
         CLI   MTHNUM,3                                                         
         BNH   FIXM90                                                           
         MVI   QTRNUMB,2                                                        
         CLI   MTHNUM,6                                                         
         BNH   FIXM90                                                           
         MVI   QTRNUMB,3                                                        
         CLI   MTHNUM,9                                                         
         BNH   FIXM90                                                           
         MVI   QTRNUMB,4                                                        
FIXM90   LA    R3,MTHLEN(R3)       FROM QSTART THRU QEND                        
         BCT   R0,FIXM60                                                        
*                                                                               
FIXMX    B     EXIT                                                             
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
**********************************************************************          
* LOOKUP APG DEPT CODE                                               *          
**********************************************************************          
         SPACE 1                                                                
LOOKUP   NTR1                                                                   
         L     RF,ADACC                                                         
         MVC   WORK,XSPACES                                                     
         MVC   SVKEY,XSPACES                                                    
         MVC   SVKEY(L'APGGIV),1(RF)    1R ACCOUNT CODE                         
         L     RF,APGGBUF                                                       
         LA    R4,4                4 LEVELS                                     
         LA    R5,LOOKTAB                                                       
LOOK10   CLI   0(R5),EOT                                                        
         BE    LOOK30                                                           
         LA    R1,SVKEY+2          BUMP PAST U/L                                
         LA    RE,WORKD                                                         
         SR    R3,R3                                                            
         ICM   R3,3,0(R5)                                                       
         AR    RE,R3                                                            
         SR    R0,R0                                                            
         ICM   R0,1,0(RE)                                                       
         AR    R1,R0                                                            
         MVC   0(12,R1),XSPACES    CLEAR LOWEST LEVEL                           
         USING BIND,RF                                                          
         L     R0,BININ            R0 TO NUMBER IN TABLE                        
         LTR   R0,R0                                                            
         BP    *+6                                                              
         DC    H'0'                                                             
         LA    R2,BINTABLE         R2 TO APG TABLE                              
         USING APGGD,R2                                                         
LOOK20   CLC   APGGIV,SVKEY                                                     
         BE    LOOK40              FOUND 1R CODE IN TABLE                       
         LA    R2,APGGLEN(R2)                                                   
         BCT   R0,LOOK20                                                        
         LA    R5,2(R5)                                                         
         BCT   R4,LOOK10                                                        
LOOK30   MVC   WORK(2),=C'00'      NO RULE                                      
         B     LOOKXX                                                           
LOOK40   MVC   WORK(L'APGGREC),APGGREC                                          
LOOKXX   B     EXIT                                                             
*                                                                               
*                                                                               
LOOKTAB  DC    AL2(LLEVD-WORKD)                                                 
         DC    AL2(LLEVC-WORKD)                                                 
         DC    AL2(LLEVB-WORKD)                                                 
         DC    AL2(LLEVA-WORKD)                                                 
         DC    AL1(EOT)                                                         
         DROP  R2,RF                                                            
         EJECT                                                                  
**********************************************************************          
* SALARY LOOKUP                                                      *          
**********************************************************************          
         SPACE 1                                                                
SALUP    NTR1                                                                   
         USING SALARYD,RE                                                       
         USING MTHD,RF                                                          
         L     RE,SALAREA                                                       
         LA    RF,MNTHS                                                         
         LA    R0,MAXMTHS                                                       
         LA    R1,SALACTM1               ACTUAL MONTHLY HOURS                   
         ZAP   0(L'SALACTM1,R1),MTHHRS   INTO MONTHLY ACCUMS OF BLOCK           
         LA    RF,MTHLEN(RF)                                                    
         LA    R1,L'SALACTM1(R1)                                                
         BCT   R0,*-14                                                          
         DROP  RE,RF                                                            
*                                                                               
         USING SRTD,R5                                                          
         USING SDTD,R6                                                          
         L     R5,ASRTREC                                                       
         LA    R6,SRDTL                                                         
         LA    R4,MAXMTHS          MOVE MONTHS AND SALARY TO ASRTREC            
SALUP10  OC    SDMNTH,SDMNTH                                                    
         BZ    SALUPX              DONE                                         
         CLI   SDMNTH,X'95'                                                     
         BNL   SALUP20                                                          
         MVC   WORK(2),SDMNTH      GET SALARY FOR MONTH                         
         MVC   WORK+2(2),SDMNTH                                                 
         GOTO1 ACSLRY,DMCB,(X'80',ADACC),WORK,SALAREA2,ADCOMFAC                 
         L     R2,SALAREA2                                                      
         USING SLRD,R2                                                          
         ZAP   DUB,SLRTOT                                                       
         B     SALUP30                                                          
         USING SALARYD,R2                                                       
SALUP20  L     R2,SALAREA                                                       
         MVC   SALSTART(2),SDMNTH                                               
         MVI   SALSTART+2,X'01'                                                 
         MVC   SALEND(2),SALSTART                                               
         MVI   SALEND+2,X'31'                                                   
         GOTO1 ACSALHST,DMCB,ACWORKD,SALAREA,AIO2                               
         OC    SALSTAT2,SALSTAT2   ANY ERRORS?                                  
         BZ    *+14                NO                                           
         TM    SALSTAT2,ALL-SALINVPR ANY ERROR BUT PERSON MISSING               
         BZ    *+6                                                              
         DC    H'0'                DIE FOR NOW                                  
         ZAP   DUB,SALSALRY                                                     
*                                                                               
*                                  SAVE MONTHLY SALARY IN MINI ELEMENT          
SALUP30  AP    SDSAL,DUB                                                        
         AP    SDSALY,DUB          AND YTD AS WELL                              
         SR    R1,R1                                                            
         IC    R1,SDMNUM           CURRENT MONTH NUMBER                         
         SR    R0,R0                                                            
         IC    R0,RQMNTHS          NUMBER OF MONTHS IN REQUEST                  
         SR    R0,R1                                                            
         LTR   R0,R0               NUMBER OF YTD BUCKETS TO UPDATE              
         BNP   SALUP40                                                          
         LR    R1,R6               CURRENT MONTH                                
         LA    R1,SDLEQ(R1)        UPDATE YTD SALARY BUCKETS                    
         AP    SDSALY-SDTD(SDBUKLN,R1),DUB                                      
         BCT   R0,*-10                                                          
*                                                                               
SALUP40  LA    R6,SDLEQ(R6)                                                     
         BCT   R4,SALUP10                                                       
SALUPX   DS    0H                                                               
         B     EXIT                                                             
         DROP  R2,R5,R6                                                         
         EJECT                                                                  
**********************************************************************          
* POST SORT RECORD TO GROUP TABLE                                    *          
**********************************************************************          
         SPACE 1                                                                
         USING GRPD,R2                                                          
PSTGRP   NTR1                                                                   
         LA    R2,GRPWRK           GROUP TOTAL RECORD                           
*                                                                               
         CLC   RQGRP,XSPACES       DID THEY REQUEST ANY GROUP?                  
         BE    *+20                NO  - RUN IT ON ALL GROUPS                   
         CLC   GRPGRP,RQGRP        YES - MATCH ON GROUP                         
         BE    *+10                                                             
         XC    GRPGRP,GRPGRP       CLEAR GROUP                                  
*                                                                               
* DON'T ADD ANYTHING TO TABLE NOT FILLED IN                                     
*                                                                               
         CLC   GRPLOC,XSPACES               ANY KEY?                            
         BE    PSTGX                                                            
*                                                                               
         USING GRPDTD,R6                                                        
         LA    R6,GRPDTL                    SORT DETAIL                         
*                                                                               
PSTG10   LA    RE,GRPSAL                                                        
         LA    R0,GRPNUMDT                                                      
PSTG20   CP    0(SDBUKLN,RE),=P'0'          ANY DATA                            
         BNE   *+16                                                             
         LA    RE,SDBUKLN(RE)                                                   
         BCT   R0,PSTG20                                                        
         B     PSTGX                        NOTHING THERE - DON'T ADD           
*                                                                               
* DON'T ADD TO TABLE IF IT HAS A GROUP AND NO PROG HOURS                        
*       UNLESS IT IS GROUP 50 THEN CHECK LDA HOURS                              
*                                                                               
         LA    R1,GRPPRDHR-GRPDTD           DISPL TO PROD HOURS                 
         TM    FLAG2,FLGFH         ARE WE READING FH                            
         BNO   PSTG30                                                           
         CLC   GRPGRP,=C'50'                GROUP 50 POINT DISP-LDA             
         BNE   PSTG30                                                           
         LA    R1,GRPLDAHR-GRPDTD           DISPL TO LDA HOURS                  
*                                                                               
PSTG30   LA    R6,GRPDTL                    SORT DETAIL                         
*                                                                               
         LA    R0,MAXMTHS                                                       
PSTG40   LA    RE,GRPSAL                                                        
         AR    RE,R1                        BUMP TO PROD/LDA HOURS              
         CP    0(SDBUKLN,RE),=P'0'          ANY HOURS                           
         BNE   PSTG50                                                           
         LA    R6,GRPDTLQ(R6)                                                   
         BCT   R0,PSTG40                                                        
         B     PSTGX                        NOTHING THERE - DON'T ADD           
*                                                                               
PSTG50   DS    0H                  READ TO SEE IF RECORD IS DUP                 
         TM    FLAG2,FLGFH         ARE WE READING FH                            
         BNO   *+14                                                             
         CLC   GRPGRP,=C'50'       DONT BOTHER CHECKING IF ITS LDA              
         BE    PSTG80                                                           
         CLC   GRPDPT,=C'00'       OR IF DEPT 00                                
         BE    PSTG80                                                           
*                                                                               
         USING BIND,RE                                                          
         L     RE,AGRPBUFF         RE=GROUP TABLE                               
         ICM   R0,15,BININ                                                      
         BZ    PSTG80              NOTHING IN TABLE JUST ADD IT                 
         LA    R4,BINTABLE                                                      
         DROP  RE                                                               
*                                                                               
PSTG60   CLC   GRPLOC,GRPLOC-GRPD(R4)           DUP LOCATION?                   
         BNE   PSTG70                                                           
         CLC   GRPGRP,0(R4)                     IF SAME GROUP SKIP              
         BE    PSTG70                                                           
         TM    FLAG2,FLGFH         ARE WE READING FH                            
         BNO   *+14                                                             
         CLC   GRPGRP-GRPD(L'GRPGRP,R4),=C'50'  DONT SET IF ITS LDA             
         BE    PSTG70                                                           
         OI    GRPSTAT,GRPDUP               TURN ON BIT IN WORK FIELD           
         OI    GRPSTAT-GRPD(R4),GRPDUP      TURN ON BIT IN TABLE                
         B     PSTG80                                                           
*                                                                               
PSTG70   LA    R4,GRPLEN(R4)                                                    
         BCT   R0,PSTG60                                                        
*                                                                               
PSTG80   DS    0H                  READ TO SEE IF RECORD IS DUP                 
         GOTO1 BINADD,DMCB,GRPWRK,AGRPBUFF                                      
PSTGX    B     EXIT                                                             
         DROP  R2,R6                                                            
         EJECT                                                                  
**********************************************************************          
* ADD SALARY AND 1C HOURS FOR CURRENT ACCOUNT TO GROUP TOTAL RECORD  *          
**********************************************************************          
         SPACE 1                                                                
SALGRP   NTR1                                                                   
         USING SRTD,R5                                                          
         L     R5,ASRTREC          SORT RECORD                                  
*                                                                               
         USING BIND,RE                                                          
         L     RE,AGRPBUFF         RE=GROUP TABLE                               
         ICM   R0,15,BININ                                                      
         BZ    SALGX                                                            
         USING GRPD,R4                                                          
         LA    R4,BINTABLE                                                      
         DROP  RE                                                               
*                                                                               
SALG10   CLC   SRACC(SRKLNQ),GRPLOC   SAME PERSON?                              
         BNE   SALG30                                                           
*                                                                               
         USING SDTD,R6                                                          
         LA    R6,SRDTL               MONTHLY DETAIL                            
         USING GRPDTD,R3                                                        
         LA    R3,GRPDTL              POINT R3 TO DETAIL INFO                   
         LA    R2,MAXMTHS             ALL MONTHS                                
*                                                                               
SALG20   ZAP   GRPSAL,SDSAL           MONTHLY SALARY                            
         ZAP   GRPSALY,SDSALY         YTD     SALARY                            
         ZAP   GRPTHRS,SDTHRS         MONTHLY TOTAL 1C HOURS                    
         ZAP   GRPYTHRS,SDYTHRS       YTD     TOTAL 1C HOURS                    
         LA    R3,GRPDTLQ(R3)                                                   
         LA    R6,SDLEQ(R6)                                                     
         BCT   R2,SALG20                                                        
*                                                                               
SALG30   LA    R4,GRPLEN(R4)                                                    
         BCT   R0,SALG10                                                        
*                                                                               
SALGX    B     EXIT                                                             
         DROP  R3,R4,R5,R6                                                      
         EJECT                                                                  
**********************************************************************          
* DATA MANAGER ROUTINES                                              *          
**********************************************************************          
         SPACE 1                                                                
RHIGH    LR    R0,RE               SAVE RETURN POINT                            
         GOTO1 DATAMGR,DMCB,DMRDHI,DIR,DKEY,DIRAREA                             
         MVC   DA,DIRAREA+(ACCKDA-ACCRECD)                                      
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
READ     LR    R0,RE               SAVE RETURN POINT                            
         GOTO1 DATAMGR,DMCB,DMREAD,DIR,DKEY,DIRAREA                             
         MVC   DA,DIRAREA+(ACCKDA-ACCRECD)                                      
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
RSEQ     LR    R0,RE               SAVE RETURN POINT                            
         GOTO1 DATAMGR,DMCB,DMRSEQ,DIR,DKEY,DIRAREA                             
         MVC   DA,DIRAREA+(ACCKDA-ACCRECD)                                      
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
GET      LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,GETREC,MST,DA,(R2),DMWORK                           
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
**********************************************************************          
* DATA CONSTANTS                                                     *          
**********************************************************************          
         SPACE 1                                                                
* EXTERNAL ROUTINES                                                             
*                                                                               
ADCONS   DS    0A                                                               
         DC    V(ACSLRY)                                                        
         DC    V(ACSALHST)                                                      
         DC    V(COVAIL)                                                        
         DC    V(UNDERLIN)                                                      
         DC    V(HELLO)                                                         
         DC    V(PRNTBL)           PRINT DATA                                   
         DC    V(DLFLD)            DOWNLOAD MODULE                              
*                                                                               
         DC    A(CCOMTAB)                                                       
         DC    A(CSUMPST)                                                       
         DC    A(CDPTBUCK)                                                      
         DC    A(CDPTBKFR)                                                      
         DC    A(CQTRMTAB)                                                      
         DC    A(AMAINTAB)                                                      
         DC    A(AWORKTAB)                                                      
         DC    A(VCOMMON)          COMMON ROUTINES NMOD #1                      
         DC    A(VCOMMON2)         COMMON ROUTINES NMOD #2                      
         DC    A(CIO)                                                           
         DC    A(CIO2)                                                          
         DC    A(CDIO)                                                          
         DC    A(CSALAREA)                                                      
         DC    A(CSALARE2)                                                      
         DC    A(DUMP)             PRINTABLE ROUTINE                            
         DC    A(DWNL)             DOWNLOAD                                     
         DC    A(DWNRTE)           DOWNLOAD ROUTINE                             
         DC    A(DWNBUF)           DOWNLOAD BUFFER                              
         DC    A(NFISTAB)          FISCAL YEAR TABLE                            
         DC    A(DPTTOTS)          DEPARTMENT TOTALS                            
*                                                                               
GLOBALS  DS    0X                                                               
         DC    P'0'         ZEROS                                               
         DC    C'1N'        NONCLI                                              
         DC    C'1C'        CLILEG                                              
         DC    C'1R'        EMPLEDG                                             
         DC    CL6'ACCDIR'                                                      
         DC    CL6'ACCMST'                                                      
         DC    CL8'GETREC  '                                                    
         DC    CL8'ADDREC  '                                                    
         DC    CL8'PUTREC  '                                                    
         DC    12X'FF'      FOXES                                               
         DC    C'SET  '     SETC                                                
         DC    C'PUT  '     PUTC                                                
         DC    C'ADD  '     ADDC                                                
         DC    C'GET  '     GETC                                                
         DC    C'SEQ  '     SEQC                                                
         DC    C'END  '     ENDC                                                
         DC    C'HIGH '     HIGHC                                               
         DC    C'CLEAR'     CLEARC                                              
GLOBALSL EQU   *-GLOBALS                                                        
*                                                                               
CLITAB   DS    0CL3                                                             
         DC    C'TMS'                                                           
         DC    C'LEX'                                                           
         DC    AL1(EOF)                                                         
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,000,A),FORMAT=BI,WORK=1'                     
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=(0000,,,)'                             
         EJECT                                                                  
********************************************************************            
* NEW FISCAL YEAR ADJUST TABLE COVERED BY NFISTABD                 *            
********************************************************************            
NFISTAB  DS      0F                                                *            
         DC    XL2'0010',XL2'A009',PL6'180000',PL6'154000',CL1'H'  *            
         DC    XL2'A010',XL2'A103',PL6'090000',PL6'077000',CL1'H'  *            
         DC    XL2'A104',XL2'A203',PL6'180000',PL6'154000',CL1'H'  *            
         DC    XL2'A204',XL2'FF03',PL6'180000',PL6'161000',CL1'R'  *            
********************************************************************            
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
ABILITY  EQU   (4095*3)-(*-ACFR02)    REMAINING ADDRESSIBILITY                  
         EJECT                                                                  
***********************************************************************         
* DUMP RECORDS                                                        *         
***********************************************************************         
         SPACE 1                                                                
         DS    0D                                                               
DUMP     NMOD1 0,**DUMP**                                                       
         L     RC,0(R1)                                                         
         CLI   QOPT9,C'Y'                                                       
         BNE   DUMPX                                                            
         LA    R0,L'MSG1                                                        
         LA    R2,MSG1                                                          
         L     R3,4(R1)                                                         
         L     R4,8(R1)                                                         
*                                                                               
         LA    R5,=C'2D'                                                        
         GOTO1 PRNTBL,DMCB,((R0),(R2)),(R3),C'DUMP',(R4),(R5),         X        
               (C'P',PRINT)                                                     
*                                                                               
DUMPX    XIT1                                                                   
*        MVC   MSG1,=CL10'TRNS  REC'                                            
*        GOTO1 ADUMP,DMCB,(RC),(R2),SRTLEN                                      
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         DROP  R9,RA,RB,RC                                                      
         EJECT                                                                  
         DS    0D                  DOWNLOAD BUFFER                              
DWNBUF   DS    CL250                                                            
**********************************************************************          
* ROUTINES ENTERABLE FROM BASE OR WORK NMODS                         *          
* ************WORK NMOD # 1 ****************                         *          
**********************************************************************          
         SPACE 1                                                                
         DS    0D                                                               
VCOMMON  NMOD1 0,**COMM1**,R9                                                   
         USING ACWORKD,RA          RA=A(GENERAL W/S)                            
         LA    RC,SPACEND                                                       
         USING WORKD,RC            RC=A(LOCAL W/S)                              
         SRL   RF,32-8                                                          
         SLL   RF,2                                                             
         B     *+4(RF)                                                          
         B     VMAIN               1 -GETMAIN WORK AREAS AND STORAGE            
         B     VWRAP               2 -FREEMAIN WORK AREAS AND STORAGE           
         B     VSUPBLD             3 -BUILD TABLE OF SUPERLEDGER ACCTS          
         B     VGETNAME            4 -GETNAME REC IN AIO RET IN WORK            
         B     VBINADD             5 -BINARY TABLE ADD                          
         B     VBINSRCH            6 -SEARCH BINARY TABLE                       
         B     VCLRSORT            7 -CLEAR SORT AREA                           
         DC    (COMSPARE*L'COM1)X'00'                                           
*                                                                               
COM1XIT  XIT1  ,                                                                
         EJECT                                                                  
**********************************************************************          
* ACQUIRE CORE FOR BINARY TABLES / BUFFALO                           *          
**********************************************************************          
         SPACE 1                                                                
VMAIN    DS    0H                                                               
         L     R0,=A(LENBUFF)            ACQUIRE AN ADDITIONAL BUFFER           
         GETMAIN R,LV=(0)                                                       
         ST    R1,ABUFF                  SAVE A(BUFFER)                         
         LR    R2,R1                     R2=BUFFER POINTER                      
         L     RE,ADMASTC                                                       
         USING MASTD,RE                                                         
         STCM  R2,15,MCUSRDMP            PRINT THE BUFFER IN A DUMP             
         LR    RF,R2                                                            
         A     RF,=A(LENBUFF)                                                   
         STCM  RF,15,MCUSRDMP+4          END OF DUMP AREA                       
         DROP  RE                                                               
*                                        INIT BINARY TABLES                     
         USING MAIND,R3                                                         
         USING BIND,R2                                                          
         LA    R0,MAINNUM                NUMBER OF BUFFERS NEEDED               
         L     R3,MAINTAB                TABLE OF BINARY PARMS                  
MAIN00   MVC   0(L'MAINEYE,R2),MAINEYE   SEED TABLE EYE CATCHER                 
         LA    R2,L'MAINEYE(R2)                                                 
         SR    RE,RE                                                            
         ICM   RE,3,MAINAST              DISP TO TAB ADDR IN WORK STOR          
         AR    RE,RC                     ADD ADDR OF WORKING STORAGE            
         ST    R2,0(RE)                  SAVE ADDR OF TABLE                     
         XC    BIND(BINLENQ),BIND        BUILD BIN TABLE PARMS                  
         MVC   BINLEN+2(2),MAINLEN                                              
         MVC   BINDISPK+2(2),MAINDISK                                           
         MVC   BINMAX+2(2),MAINMAX                                              
         MVC   BINNUMB,MAINNUMB                                                 
         MVC   BINFRST,MAINFRST                                                 
         ICM   RE,15,MAINSIZE                                                   
         AR    R2,RE                     BUMP BY LENGTH OF TABLE                
         LA    R3,MAINLNQ(R3)                                                   
         BCT   R0,MAIN00                                                        
*                                        INIT BINARY TABLES                     
         USING WORKMD,R3                                                        
         LA    R0,MWRKNUM                NUMBER OF WORK AREAS NEEDED            
         L     R3,WORKTAB                TABLE OF WORK AREAS                    
MAIN04   SR    RE,RE                                                            
         ICM   RE,3,WORKAST              DISP TO TAB ADDR IN WORK STOR          
         AR    RE,RC                     ADD ADDR OF WORKING STORAGE            
         ST    R2,0(RE)                  SAVE ADDR OF TABLE                     
         SR    RE,RE                                                            
         ICM   RE,3,WORKSIZE                                                    
         AR    R2,RE                     BUMP BY LEN OF WORK AREA               
         LA    R3,WORKLNQ(R3)                                                   
         BCT   R0,MAIN04                                                        
*                                        SET BUFFALOC                           
         ST    R2,ADBUFF                                                        
         USING BUFFALOD,R4                                                      
         L     R4,ADBUFF                                                        
         XC    BUFFCNTL,BUFFCNTL         CLEAR BUFFALO CONTROL AREA             
         MVI   BUFFCNTL,BUFKLEN          1ST BYTE IS THE KEY LENGTH             
         MVI   BUFFCNTL+1,ASCEND         2ND BYTE IS ASCENDING                  
         MVI   BUFFCNTL+2,X'FF'          3RD BYTE IS X'FF'                      
         MVC   BUFFLKEY,=A(BUFKLEN)      LENGTH OF THE KEY                      
         MVC   BUFFLDTA,=A(BDATLN)       LENGTH OF THE DATA                     
         MVC   BUFFLALL,=A(BRECLN)       LENGTH OF THE RECORD                   
         MVC   BUFFROWS,=A(BUFROW)       # OF ACCUM ROWS                        
         MVC   BUFFCOLS,=A(BBKCNT)       # OF ACCUM COLS                        
         MVI   BUFFFLVR,PACKED           FLAVOR                                 
         MVC   BUFFCRMX,=A(BUFMAX)       MAX NUMBER OF LINES IN CORE            
         MVI   BUFFFLIP,FILINDC          FILE INDICATOR                         
*                                                                               
         GOTO1 BUFFALO,DMCB,(L'SETC,SETC),ADBUFF                                
         B     COM1XIT                                                          
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
**********************************************************************          
* SUB-ROUTINE TO WRAP UP REPORT                                      *          
* FREE UP ACQUIRED CORE                                              *          
**********************************************************************          
         SPACE 1                                                                
VWRAP    DS    0H                                                               
         L     R1,ABUFF                                                         
         L     R0,=A(LENBUFF)                                                   
         FREEMAIN R,A=(1),LV=(0)                                                
*                                                                               
         L     RE,VEXTRAS                                                       
         USING RUNXTRAD,RE                                                      
         L     RE,ADMASTD                                                       
         USING MASTD,RE                                                         
         XC    MCUSRDMP(8),MCUSRDMP CLEAR OUT EXTRA DUMP AREA                   
         B     COM1XIT                                                          
         DROP  RE                                                               
         EJECT                                                                  
**********************************************************************          
* BUILD SUPERLEDGER TABLE                                            *          
* START DATE IS SET BEFORE THE CALL TO SUPBLD                        *          
**********************************************************************          
         SPACE 1                                                                
VSUPBLD  DS    0H                           GET APG S/L RULES                   
         USING BIND,RF                                                          
         L     RF,APGBUFF                   CLEAR APG TABLE                     
         XC    BININ,BININ                                                      
         L     RF,APGGBUF                   CLEAR APG DPT TABLE                 
         XC    BININ,BININ                                                      
         L     RF,APRDBUFF                  CLEAR PROD TABLE                    
         XC    BININ,BININ                                                      
         DROP  RF                                                               
         USING ACTRECD,R6                                                       
         L     R6,AIO                                                           
         MVC   ACTKEY,XSPACES               CLEAR THE KEY                       
         MVC   ACTKCPY,RCCOMPFL             COMPANY                             
         MVC   ACTKUNT(2),=C'FR'            UNIT/LEDGER TO READ                 
*                                                                               
         USING NFISTABD,RF                                                      
         L     RF,ANFISTAB                                                      
         CLC   START,NFEND         DOES START FALL IN THIS TABLE ENTRY          
         BNH   SUPB05                                                           
         LA    RF,NFISTABQ(RF)     BUMP TO NEXT RANGE IN TABLE                  
         B     *-14                                                             
SUPB05   MVC   ACTKLDG,NFSUPLDG    CORRECT LEDGER TO READ                       
         CLI   ACTKLDG,C'H'        ARE WE READING FH                            
         BNE   *+8                                                              
         OI    FLAG2,FLGFH         READING FH SUPERLEDGER                       
         DROP  RF                                                               
*                                                                               
         MVC   CUL,ACTKEY                   SAVE COMP/UNIT/LEDGER               
         GOTO1 DATAMGR,DMCB,(DMINBTS,DMRDHI),DIR,AIO,AIO                        
SUPB10   GOTO1 DATAMGR,DMCB,(DMINBTS,DMRSEQ),DIR,AIO,AIO                        
         CLC   CUL,ACTKEY                                                       
         BNE   SUPBXIT                                                          
*                                                                               
         MVC   DA,ACTKDA                                                        
         GOTO1 DATAMGR,DMCB,(DMINBTS,GETREC),MST,DA,AIO,DMWORK                  
         TM    8(R1),ALL-PASSDEL                                                
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R1,=C'30'                                                        
         TM    FLAG2,FLGFH       IF READING FH THEN 30 AND ON ARE PRODS         
         BO    *+8                                                              
         LA    R1,=C'50'                                                        
         CLC   ACTKACT(2),0(R1)  FROM THIS ACCOUNT ON ARE PRODS                 
         BNL   SUPB50                                                           
*                                                                               
         L     R2,AAPGWK                                                        
         MVC   0(APGLEN,R2),XSPACES                                             
         USING APGD,R2                                                          
         MVC   APGREC,ACTKACT           RECEIVING ACCOUNT                       
         GOTO1 GETNAME                  GETNAME OF REC IN AIO                   
         MVC   APGNME,WORK                                                      
         SR    R4,R4                                                            
         GOTO1 HELLO,ELIST,(C'G',MST),('GLPELQ',ACTRECD),0                      
         CLI   ELERR,0                                                          
         BNE   SUPB30                                                           
         USING GLPELD,R4                                                        
         L     R4,ELADDR                                                        
SUPB20   LA    R3,WORK                                                          
         USING APGGD,R3                                                         
         MVC   APGGD(APGGLEN),XSPACES                                           
         MVC   APGGIV,GLPACC1           GIVING ACCOUNT                          
         MVC   APGGREC,APGREC                                                   
         GOTO1 BINADD,DMCB,WORK,APGGBUF                                         
SUPB30   DS    0H                                                               
         GOTO1 BINADD,DMCB,AAPGWK,APGBUFF                                       
         LTR   R4,R4                    GET NEXT RECORD                         
         BZ    SUPB10                                                           
SUPB40   SR    R0,R0                                                            
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0                                                          
         BE    SUPB10                                                           
         CLI   0(R4),GLPELQ             GET ELEMENT                             
         BE    SUPB20                                                           
         B     SUPB40                                                           
*                                                                               
* BUILD PRODUCT TABLE                                                           
*                                                                               
         USING PRDTBD,R2                                                        
SUPB50   LA    R2,PRDWRK                                                        
         XC    PRDWRK,PRDWRK                                                    
         MVC   PRDGRP,ACTKACT      RECEIVING ACCOUNT                            
         GOTO1 GETNAME             GETNAME OF REC IN AIO                        
         MVC   PRDGRPNM,WORK       GROUP NAME                                   
         SR    R4,R4                                                            
         GOTO1 HELLO,ELIST,(C'G',MST),('GLPELQ',ACTRECD),0                      
         CLI   ELERR,0                                                          
         BNE   SUPB80                                                           
*                                                                               
         USING GLPELD,R4                                                        
         L     R4,ELADDR                                                        
         USING ACQD,RF                                                          
SUPB60   DS    0H                                                               
         L     RF,ADQSTACK                                                      
         CLC   ACQCFLT1(L'ACMCFLTS),XSPACES ANY CONTRA FILTERS?                 
         BE    SUPB70                      NO                                   
         CLC   GLPFLTS,ACQCFLT1    ACQCFL1-5, GETS SET IN REQUEST PRGM          
         BNE   SUPB80                                                           
         DROP  RF                                                               
*                                                                               
SUPB70   MVC   PRDCP,GLPACC1A+2    MOVE IN CLIENT/PRODUCT                       
         GOTO1 BINADD,DMCB,PRDWRK,APRDBUFF                                      
SUPB80   SR    R0,R0                                                            
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0                                                          
         BE    SUPB10                                                           
         CLI   0(R4),GLPELQ             GET ELEMENT                             
         BE    SUPB60                                                           
         B     SUPB10                                                           
*                                                                               
SUPBXIT  B     COM1XIT                                                          
         DROP  R2,R3,R4,R6                                                      
         EJECT                                                                  
**********************************************************************          
* GET NAME FROM ACCOUNT OR CONTRA HEADER                             *          
**********************************************************************          
         SPACE 1                                                                
VGETNAME DS    0H                                                               
         USING ACTRECD,R6                                                       
         MVC   WORK,XSPACES                                                     
         L     R6,AIO                                                           
         GOTO1 HELLO,ELIST,(C'G',MST),('NAMELQ',ACTRECD),0                      
         CLI   ELERR,0                                                          
         BNE   GETNAME2                                                         
         L     R4,ELADDR                                                        
         USING NAMELD,R4               NAME EL                                  
         SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SH    R1,=Y(NAMLN1Q+1)                                                 
         BM    GETNXIT                                                          
         EX    R1,*+4                                                           
         MVC   WORK(0),NAMEREC                                                  
         B     GETNXIT                                                          
*                                                                               
GETNAME2 L     R6,AIO                                                           
         GOTO1 HELLO,ELIST,(C'G',MST),('CACELQ',ACTRECD),0                      
         CLI   ELERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CACELD,R4                                                        
         L     R4,ELADDR                                                        
         SR    R1,R1                                                            
         IC    R1,CACLN                                                         
         SH    R1,=Y(CACLN1Q+1)                                                 
         BM    GETNXIT                                                          
         EX    R1,*+4                                                           
         MVC   WORK(0),CACNAME     CONTRA NAME                                  
GETNXIT  B     COM1XIT                                                          
         DROP  R4,R6                                                            
         EJECT                                                                  
**********************************************************************          
* ROUTINE TO ADD TO A BINSRCH TABLE                                  *          
*         PARAM1              A(RECORD TO BE ADDED)                  *          
*         PARAM2              A(BINSRCH PARAMS)                      *          
**********************************************************************          
         SPACE 1                                                                
VBINADD  DS    0H                                                               
         USING BIND,R5                                                          
         XC    ADDRSAV,ADDRSAV     SAVE RECORD ADDRESS                          
         L     R5,4(R1)            BINSRCH PARAMETERS                           
         L     R3,0(R1)            A(RECORD)                                    
         MVC   DMCB+8(16),BININ                                                 
         LA    R2,BINTABLE                                                      
         GOTO1 BINSRCH,DMCB,(1,(R3)),(R2)                                       
         OC    DMCB(4),DMCB                                                     
         BNZ   *+6                                                              
         DC    H'0'                TABLE IS FULL                                
         MVC   BININ,DMCB+8        UPDATE COUNT                                 
         CLI   DMCB,1                                                           
         BE    BINXIT              NOT FOUND - ADDED                            
         L     R4,DMCB             A(RECORD FOUND)                              
         MVC   ADDRSAV,DMCB                                                     
         SR    R6,R6                                                            
         IC    R6,BINFRST          DISP. TO FIRST BUCKET                        
         LTR   R6,R6                                                            
         BZ    BINXIT              NO BUCKETS FOR THIS TABLE                    
         AR    R4,R6               RECORD FOUND                                 
         AR    R3,R6               NEW RECORD                                   
         SR    R0,R0                                                            
         IC    R0,BINNUMB          NUMBER OF BUCKETS                            
         LTR   R0,R0                                                            
         BZ    BINXIT              NO BUCKETS FOR THIS TABLE                    
         AP    0(SDBUKLN,R4),0(SDBUKLN,R3)     ADD NEW TO OLD                   
         LA    R4,SDBUKLN(R4)                                                   
         LA    R3,SDBUKLN(R3)                                                   
         BCT   R0,*-14                                                          
*                                                                               
BINXIT   B     COM1XIT                                                          
         DROP  R5                                                               
         EJECT                                                                  
**********************************************************************          
* SEARCH FOR RECORDS FROM BIN TABLES                                 *          
*        PARM 1   ADDR OF KEY FOR LOOKUP                             *          
*        PARM 2   ADDR OF BIND PARMS                                 *          
*        PARM 3   ADDR OF SAVE AREA FOR RECS                         *          
**********************************************************************          
         SPACE 1                                                                
VBINSRCH DS    0H                                                               
         USING BIND,R4                                                          
*                                                                               
         NI    FLAG,ALL-FLGNONME   TURN OFF NO NAME SWITCH                      
         LM    R3,R5,0(R1)          R3 - A(ITEM)                                
*                                   R4 - ADDR OF BIND PARMS                     
*                                   R5 - ADDR OF WHERE TO PUT THE REC           
*                                   NUMBER,LENGTH,KEY MAX (3RD PARM)            
         MVC   DMCB+8(BINKLEN),BININ                                            
         LA    R2,BINTABLE          A(TABLE)                                    
         GOTO1 BINSRCH,DMCB,(X'00',(R3)),(R2)                                   
         CLI   DMCB,0                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         LTR   R5,R5                                                            
         BZ    COM1XIT              JUST PASS BACK ADDR OF ENTRY                
         L     R2,DMCB              ADDR OF RECORD FOUND                        
         L     R1,BINLEN            LENGTH OF THE RECORD                        
         BCTR  R1,0                 REDUCED FOR THE EX                          
         EX    R1,*+4                                                           
         MVC   0(0,R5),0(R2)                                                    
         B     COM1XIT                                                          
         DROP  R4                                                               
         EJECT                                                                  
**********************************************************************          
* CLEAR SORT AREA                                                    *          
*       PARM 1   ADDR OF THE SORTREC AREA                            *          
**********************************************************************          
         SPACE 1                                                                
         USING SRTD,R5                                                          
VCLRSORT DS    0H                                                               
         L     R5,0(R1)           CLEAR SORT RECORD AREA                        
         MVC   SRTD(SRDLEN),XSPACES                                             
         ZAP   SRBUD,ZEROS                                                      
         USING SDTD,RF                                                          
         LA    RF,SRDTL                                                         
         LA    R0,MAXMTHS+1        ALL MONTHS PLUS TOTAL                        
CLR10    XC    SDMNTH(SDKEYLN),SDMNTH                                           
         LA    R1,SDBUK                                                         
         LA    RE,SDNUMBK          NUMBER ON MINI BUCKETS FOR MTH               
         ZAP   0(SDBUKLN,R1),ZEROS                                              
         LA    R1,SDBUKLN(R1)                                                   
         BCT   RE,*-10                                                          
         LA    RF,SDLEQ(RF)                                                     
         BCT   R0,CLR10                                                         
*                                                                               
         TM    FLAG,FLGPER         SHOULD WE CLEAR TABLE                        
         BO    CLR20                                                            
*                                                                               
         LA    RE,ADJTAB           CLEAR MONTHLY ADJUSTMENT TABLE               
         LA    RF,MAXMTHS*ADJLNQ                                                
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         USING ADJD,R1                                                          
CLR20    LA    R1,ADJTAB           MONTHLY ADJUSTMENT TABLE                     
         LA    RF,SRDTL                                                         
         LA    R0,MAXMTHS          MOVE MONTHS TO ASRTREC AND ADJTAB            
         USING MTHD,RE                                                          
         LA    RE,MNTHS                                                         
CLR30    MVC   SDMNTH,MTHCODE      MONTH CODE                                   
         MVC   ADJMNTH,MTHCODE                                                  
         MVC   SDMNUM,MTHNUM       NUMBER                                       
         LA    R1,ADJLNQ(R1)                                                    
         LA    RF,SDLEQ(RF)                                                     
         LA    RE,MTHLEN(RE)                                                    
         BCT   R0,CLR30                                                         
         DROP  R1                                                               
*                                                                               
         LA    RF,SRTOTAL                                                       
         MVC   SDMNTH,=X'FFFF'     MARK TOTAL ACCUMS                            
*                                                                               
         LA    R0,SDNUMBK          MAX NUMBER OF LOOPS                          
         LA    RF,SVSRTTOT         CLEAR SAVED TOTAL AREA                       
         ZAP   0(SDBUKLN,RF),=P'0'                                              
         LA    RF,SDBUKLN(RF)                                                   
         BCT   R0,*-10                                                          
*                                                                               
         B     COM1XIT                                                          
         DROP  R5,RE,RF                                                         
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
ABILITY1 EQU   (4095*3)-(*-VCOMMON)   REMAINING ADDRESSIBILITY                  
*                                     FREE UP ALL USINGS                        
         DROP  R9,RA,RB,RC                                                      
         EJECT                                                                  
**********************************************************************          
* ROUTINES ENTERABLE FROM BASE OR WORK NMODS                         *          
* ************WORK NMOD # 2 ****************                         *          
**********************************************************************          
         SPACE 1                                                                
         DS    0D                                                               
VCOMMON2 NMOD1 0,**COMM2**,R9                                                   
         USING ACWORKD,RA          RA=A(GENERAL W/S)                            
         LA    RC,SPACEND                                                       
         USING WORKD,RC            RC=A(LOCAL W/S)                              
         SRL   RF,32-8                                                          
         SLL   RF,2                                                             
         B     *+4(RF)                                                          
         B     VREPORTS            1 -DO THE REPORTING                          
         DC    (COMSPARE*L'COM2)X'00'                                           
*                                                                               
COM2XIT  XIT1  ,                                                                
         EJECT                                                                  
**********************************************************************          
* DO THE REPORTING                                                   *          
**********************************************************************          
         SPACE 1                                                                
VREPORTS DS    0H                                                               
         CLI   ACTIVITY,C'N'                                                    
         BE    COM2XIT                                                          
         XC    REPBYTE,REPBYTE                                                  
         XC    REPBYTE1,REPBYTE1                                                
         CLI   QOPT7,C'Y'          WAS DOWNLOAD OPTION SELECTED?                
         BNE   RPT10                                                            
         CLI   QOPT4,C' '          WAS ANY REPORT SELECTED                      
         BE    *+12                                                             
         CLI   QOPT4,C'A'          WAS PERSON REPORT SELECTED                   
         BNE   *+8                                                              
         MVI   REPBYTE,REPA                                                     
*                                                                               
RPT10    NI    FLAG2,FLGFH                                                      
         MVI   ACCSTUS,0                  CLEAR STATUS                          
         MVI   SVSTAT,0                   AND SAVE                              
         MVI   RCSUBPRG,0                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         MVC   SAVELEVS(SAVLVLN),XSPACES  CLEAR LEVEL NAMES                     
*                                         CLEAR DEPT/REQ TOTAL RECS             
         GOTO1 CLRSORT,DMCB,ADPTREC                                             
         GOTO1 CLRSORT,DMCB,AREQREC                                             
*                                                                               
         USING SRTD,R5                                                          
         GOTO1 ADSORTER,DMCB,(L'GETC,GETC)                                      
         L     RE,DMCB+4                                                        
         LTR   RE,RE                                                            
         BZ    COM2XIT                    NO SORT RECORDS                       
         LA    R1,SRTLEN                                                        
         L     RF,ASRTREC                                                       
         MOVE  ((RF),(R1)),(RE)                                                 
         B     RPT30                                                            
*                                                                               
RPT20    DS    0H                                                               
         GOTO1 ADSORTER,DMCB,(L'GETC,GETC)                                      
         L     RE,DMCB+4                                                        
         LTR   RE,RE                                                            
         BZ    RPT80               E - O - F REPORT TOTAL                       
         LA    R1,SRTLEN                                                        
         L     RF,ASRTREC          RECORD TO ASRTREC                            
         MOVE  ((RF),(R1)),(RE)                                                 
RPT30    L     R5,ASRTREC                                                       
         BRAS  RE,PCT              FIGURE OUT PCT OF TIME                       
         MVI   ACCSTUS,0           CLEAR STATUS                                 
         USING BIND,RE                                                          
         L     RE,DUPBUFF          ADDR OF DUP PERSON TABLE                     
         L     R0,BININ            NUMBER OF TABLE ENTRIES                      
         LTR   R0,R0                                                            
         BNP   RPT60                                                            
         LA    RF,BINTABLE         1ST TABLE ENTRY                              
         DROP  RE                                                               
         USING DUPD,RF                                                          
RPT40    CLC   SRPERSON,DUPPER     THIS PERSON HAS MULTI LOCATIONS              
         BE    RPT50                                                            
         LA    RF,DUPLEN(RF)                                                    
         BCT   R0,RPT40                                                         
         B     RPT60               SKIP SETTING STATUS                          
RPT50    TM    DUPSTAT,DUPLOC      DUP LOCATIONS?                               
         BZ    RPT60               NO- SKIP                                     
         OI    ACCSTUS,DUPLOCAT                                                 
         DROP  RF                                                               
*                                                                               
RPT60    CLC   APGDPT,XSPACES      1ST TIME?                                    
         BE    RPT70                                                            
         CLC   SRDEPT,APGDPT       CHANGE IN DEPT?                              
         BE    RPT70                                                            
         MVI   RCSUBPRG,1                                                       
         L     R5,ADPTREC          THE DEPT TOTAL RECORD                        
         BRAS  RE,DETAIL           PROCESS AS DETAIL RECORD                     
         MVC   SRDEPT,APGDPT       FILL IN DEPT FOR TOTALING                    
         CLC   SRDEPT,=C'00'       DON'T SAVE IF 00                             
         BE    *+8                                                              
         BRAS  RE,SAVDPT           ADD DEPT TOTALS TO DUP DEPT TAB              
         GOTO1 CLRSORT,DMCB,ADPTREC CLEAR DEPT RECORD                           
         L     R5,ASRTREC          SET R5 BACK TO PERSON SORT REC               
         MVI   RCSUBPRG,0                                                       
*                                                                               
RPT70    MVC   SVGRP,=C'**'        THIS REPORT IS CONSOLIDATED ONLY             
         MVC   SVGRPNM,=CL20'CONSOLIDATED'                                      
         BRAS  RE,ADJUST                                                        
         BRAS  RE,REFRESH          REFRESH LEVEL NAMES                          
         BRAS  RE,DETAIL           PROCESS THE DETAIL RECORD                    
         GOTO1 =A(ADDPHR),DMCB,(RC)  ADD PROD HOURS TO GROUP TABLE              
         GOTO1 =A(TOTIT),DMCB,(RC)   ADD DETAIL TO DEPT /REQUEST LEVEL          
         GOTO1 =A(POST),DMCB,(RC)    POST RECORD TO OTHER REPORTS               
         B     RPT20                 NEXT RECORD                                
*                                                                               
RPT80    MVI   RCSUBPRG,1                                                       
         L     R5,ADPTREC          LAST DEPT TOTAL RECORD                       
         BRAS  RE,DETAIL           PROCESS AS DETAIL RECORD                     
         MVC   SRDEPT,APGDPT       FILL IN DEPT FOR TOTALING                    
         CLC   SRDEPT,=C'00'       DON'T SAVE IF 00                             
         BE    *+8                                                              
         BRAS  RE,SAVDPT           ADD DEPT TOTALS TO DUP DEPT TAB              
         MVI   RCSUBPRG,3          REQUEST PAGE                                 
         L     R5,AREQREC          THE REQ TOTAL RECORD                         
         BRAS  RE,DETAIL           PROCESS AS DETAIL RECORD                     
RPTX     DS    0H                                                               
         NI    REPBYTE,X'FF'-REPA                                               
         DROP  R5                                                               
         EJECT                                                                  
**********************************************************************          
* PRINT DUP PERSON CODES                                             *          
**********************************************************************          
         SPACE 1                                                                
DUPREP   DS    0H                                                               
         CLI   QOPT7,C'Y'          WAS DOWNLOAD OPTION SELECTED?                
         BNE   DUP10                                                            
         CLI   QOPT4,C' '          WAS ANY REPORT SELECTED                      
         BE    *+12                                                             
         CLI   QOPT4,C'B'          WAS MULTI REPORT SELECTED                    
         BNE   *+8                                                              
         MVI   REPBYTE,REPB                                                     
*                                                                               
DUP10    MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         MVI   RCSUBPRG,4                                                       
         XC    FLAG,FLAG           CLEAR MONTHLY ADJUSTMENT TABLE               
         NI    FLAG2,FLGFH                                                      
         MVC   SVKEY,XSPACES                                                    
         MVC   SVPERCD,XSPACES                                                  
         MVC   MSGS,=CL35'CONSOLIDATED TOTALS + ADJUSTMENTS'                    
         GOTO1 CLRSORT,DMCB,ADPTREC                                             
         GOTO1 CLRSORT,DMCB,ASRTREC                                             
*                                                                               
         USING SRTD,RE                                                          
         L     RE,AREQREC          RE=A(REQTOTALS)                              
         USING SDTD,RF                                                          
         LA    RF,SRTOTAL          R6=A(SORT TOTAL DETAIL)                      
         LA    R0,SDNUMBK                                                       
         LA    R1,SDSAL                                                         
         ZAP   0(SDBUKLN,R1),=P'0'                                              
         LA    R1,SDBUKLN(R1)                                                   
         BCT   R0,*-10                                                          
         DROP  RE,RF                                                            
*                                                                               
         LA    R1,SVSRTDTL                                                      
         LA    R0,MAXMTHS*SDNUMBK  ALL MONTHS PLUS TOTAL                        
         ZAP   0(SDBUKLN,R1),ZEROS                                              
         LA    R1,SDBUKLN(R1)                                                   
         BCT   R0,*-10                                                          
*                                                                               
         USING SRTD,R5                                                          
         L     R5,ASRTREC          SORT RECORD                                  
         USING BUFD,R4                                                          
         L     R4,ABUFWRK                                                       
         MVC   BUFKEY(BUFKLEN),XSPACES  CLEAR BUFFALO AREA                      
         LA    R1,BUFBK                                                         
         LA    R0,BBKCNT                                                        
         ZAP   0(BUFBKLN,R1),ZEROS                                              
         LA    R1,BUFBKLN(R1)                                                   
         BCT   R0,*-10                                                          
*                                                                               
         MVI   BUFTYPE,BUFDUP      DUP LOCATION RECORDS                         
         LA    RF,HIGHC                                                         
         B     *+8                                                              
DUP20    DS    0H                                                               
         LA    RF,SEQC                                                          
         GOTO1 BUFFALO,DMCB,(L'SEQC,(RF)),ADBUFF,(R4),0                         
         TM    8(R1),EOF                                                        
         BO    DUPXX               NO DUP LOCATIONS TO REPORT                   
         MVC   SVBUFKEY,BUFKEY                                                  
         CLI   BUFTYPE,BUFDUP                                                   
         BE    DUP30                                                            
         MVC   MSG1,=CL10'BUFKEY'                                               
         GOTO1 ADUMP,DMCB,(RC),BUFKEY,L'BUFKEY                                  
         MVC   MSG1,=CL10'SRTREC'                                               
         GOTO1 ADUMP,DMCB,(RC),(R5),SRTLEN                                      
         MVC   MSG1,=CL10'SVSRT'                                                
         GOTO1 ADUMP,DMCB,(RC),SVSRTDTL,L'SVSRTDTL+L'SVSRTTOT                   
         GOTO1 =A(PSTADJ),DMCB,(RC)  POST ADJUSTED AMOUNT                       
         XC    FLAG,FLAG           CLEAR MONTHLY ADJUSTMENT TABLE               
         NI    FLAG2,FLGFH                                                      
         B     DUP130                                                           
*                                                                               
DUP30    CLC   SVKEY,XSPACES                                                    
         BE    DUP40                                                            
         CLC   BUFKEY(BUFSKLEN),SVKEY             SAME PERSON                   
         BE    DUP60                                                            
*                                                                               
DUP40    CLC   SVPERCD,XSPACES     IS THIS THE FIRST TIME?                      
         BE    DUP50                                                            
         CLC   SVPERCD,BUFPERS     ARE WE DOING THE SAME PERSON?                
         BE    DUP50                                                            
         CLI   SRDEPT,X'FF'        ARE WE DOING TOTALS?                         
         BNE   DUP50                                                            
         GOTO1 =A(PSTADJ),DMCB,(RC)  POST ADJUSTED AMOUNT                       
         XC    FLAG,FLAG           CLEAR MONTHLY ADJUSTMENT TABLE               
         NI    FLAG2,FLGFH                                                      
*                                                                               
DUP50    MVC   SVPERCD,BUFPERS     SAVE OFF BUFFALO PERSON FOR COMPARE          
         MVC   SVKEY,XSPACES       BUILD SORT KEY                               
         GOTO1 CLRSORT,DMCB,ASRTREC                                             
         OI    FLAG,FLGPER         GOT A PERSON-DONT CLEAR TABLE                
         MVC   SRPERSON,BUFPERS                                                 
         MVC   SRACC(BUFLOCLN),BUFLOCA                                          
         MVC   SVKEY(BUFSKLEN),BUFKEY                                           
         LA    RF,SRDTL                                                         
         ST    RF,ADDRSAV                                                       
         LA    RF,SVSRTDTL         SAVED AREA FOR SORT DETAIL                   
         ST    RF,SAVSRT                                                        
*                                                                               
DUP60    CLI   BUFLOCA,X'FF'       TOTAL LINE?                                  
         BE    DUP80                                                            
*                                                                               
         CP    BUFSAL,=P'0'                                                     
         BE    DUP80                                                            
*                                                                               
         USING ADJD,R1                                                          
         LA    R1,ADJTAB                                                        
         LA    R0,MAXMTHS                                                       
DUP70    CLC   ADJMNTH,BUFMNTH     FIND RIGHT MONTH                             
         BNE   *+14                                                             
         MVC   ADJLOC,BUFLOCA                                                   
         B     DUP80                                                            
         LA    R1,ADJLNQ(R1)                                                    
         BCT   R0,DUP70                                                         
         DROP  R1                                                               
*                                                                               
         USING SDTD,R6                                                          
DUP80    L     R6,ADDRSAV          MONTHLY DETAIL                               
         CLI   BUFMNTH,X'FF'       TOTAL LINE?                                  
         BNE   *+8                                                              
         LA    R6,SRTOTAL                                                       
         MVC   SDMNTH,BUFMNTH      MONTH                                        
*                                                                               
         CLI   SDMNTH,X'FF'        TOTAL LINE?                                  
         BE    DUP100                                                           
         L     RE,SUMPST                                                        
         USING SUMPD,RE                                                         
DUP90    LA    R1,BUFD             BUFFALO ACCUMS                               
         LA    R3,SDTD             SORT ACCUMS                                  
         CLI   SUMPDISP,EOT                                                     
         BE    DUP100                                                           
         AH    R1,SUMPBDIS         POINTS TO ACCUMS                             
         AH    R3,SUMPDISP                                                      
         CLC   SUMPBDIS,=H'0'                                                   
         BE    *+10                                                             
         ZAP   0(SDBUKLN,R3),0(BUFBKLN,R1)                                      
         LA    RE,SUMPLN(RE)                                                    
         B     DUP90                                                            
*                                                                               
DUP100   CLI   BUFLOCA,X'FF'       TOTAL LINE?                                  
         BNE   DUP110                                                           
         LA    R0,SDNUMBK          NUMBER OF BUCKETS                            
         L     RE,SAVSRT           START OF SAVED AREA                          
         LA    RF,SDSAL            BUCKETS TO ACCOUMULATE                       
         ZAP   0(SDBUKLN,RE),0(SDBUKLN,RF)                                      
         LA    RE,SDBUKLN(RE)                                                   
         LA    RF,SDBUKLN(RF)                                                   
         BCT   R0,*-14                                                          
*                                                                               
DUP110   LA    R6,SDLEQ(R6)        NEXT AVAILABLE MONTH SLOT                    
         ST    R6,ADDRSAV          SAVE IT                                      
         L     RE,SAVSRT                                                        
         LA    RE,SDDTLQ(RE)       NEXT SET OF BUCKTES                          
         ST    RE,SAVSRT           SAVE IT                                      
         CLI   BUFMNTH,X'FF'       TOTAL LINE?                                  
         BNE   DUP20                                                            
         CLI   SRDEPT,X'FF'        ARE WE DOING TOTALS?                         
         BNE   DUP120                                                           
         BRAS  RE,SAVTOT           SAVE TOTAL LINE BEFORE ADJUSTMENTS           
         BRAS  RE,ADJUST                                                        
         TM    SRSTAT,SRHRS        WAS THIS PERSON ADJUSTED                     
         BO    DUP120                                                           
         BRAS  RE,PCT              FIGURE OUT PCT OF TIME                       
*                                                                               
DUP120   BRAS  RE,REFRESH          REFRESH LEVEL NAMES                          
         BRAS  RE,DETAIL           PROCESS THE DETAIL RECORD                    
         B     DUP20                                                            
*                                                                               
DUP130   MVI   RCSUBPRG,1          DEPARTMENT PAGE                              
         LA    R2,MAXDPTS                                                       
         L     R5,ADUPREC          DUP DEPT TOTAL RECORD                        
DUP140   OC    SRDEPT,SRDEPT                                                    
         BZ    DUP150                                                           
         MVC   WORK,XSPACES                                                     
         LA    R3,WORK                                                          
         USING APGD,R3                                                          
         MVC   APGREC(L'SRDEPT),SRDEPT                                          
         GOTO1 BINSRC,DMCB,WORK,APGBUFF,WORK                                    
         MVC   APGNAME,APGNME      GET DEPARTMENT NAME AND CODE                 
         MVC   APGDPT,APGREC                                                    
         BRAS  RE,DETAIL           PROCESS AS DETAIL RECORD                     
         LA    R5,SRTLEN(R5)                                                    
         BCT   R2,DUP140                                                        
*                                                                               
DUP150   MVI   RCSUBPRG,3          REQUEST PAGE                                 
         L     R5,AREQREC          THE REQ TOTAL RECORD                         
         BRAS  RE,DETAIL           PROCESS AS DETAIL RECORD                     
*                                                                               
DUPXX    DS    0H                                                               
         NI    REPBYTE,X'FF'-REPB                                               
         DROP  R4,R6,RE                                                         
         EJECT                                                                  
**********************************************************************          
* PRINT ADDITIONAL REPORTS BY GROUP                                  *          
**********************************************************************          
         SPACE 1                                                                
GRPREP   DS    0H                                                               
         CLI   QOPT1,C'Y'          RUN GROUP REPORT?                            
         BNE   GRPRX                                                            
         CLI   QOPT7,C'Y'          WAS DOWNLOAD OPTION SELECTED?                
         BNE   GRPR10                                                           
         CLI   QOPT4,C' '          WAS ANY REPORT SELECTED                      
         BE    *+12                                                             
         CLI   QOPT4,C'C'          WAS GRP PERSON REPORT SELECTED               
         BNE   *+8                                                              
         MVI   REPBYTE,REPC                                                     
*                                                                               
GRPR10   MVI   FLAG,FLGGRP         SHOW WE ARE DOING GROUP REPORT               
         NI    FLAG2,FLGFH                                                      
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         MVI   RCSUBPRG,5          GROUP REPORTS                                
         GOTO1 CLRSORT,DMCB,ADPTREC                                             
         GOTO1 CLRSORT,DMCB,AGRPREC                                             
         GOTO1 CLRSORT,DMCB,AREQREC                                             
         MVC   SVGRP(L'SVGRP+L'SVGRPNM),XSPACES                                 
         MVC   SVDPT,XSPACES                                                    
*                                                                               
         USING SRTD,R5                                                          
         L     R5,ASRTREC          SORT RECORD                                  
         USING SDTD,R6                                                          
         LA    R6,SRDTL            MONTHLY DETAIL                               
*                                                                               
         USING BIND,RE                                                          
         L     RE,AGRPBUFF         RE=GROUP TABLE                               
         ICM   R0,15,BININ                                                      
         BZ    GRPRX                                                            
         USING GRPD,R4                                                          
         LA    R4,BINTABLE                                                      
         DROP  RE                                                               
*                                                                               
GRPR20   DS    0H                                                               
         MVI   RCSUBPRG,5          GROUP REPORTS                                
         USING GRPDTD,RE                                                        
         LA    RE,GRPDTL                                                        
         LA    R2,MAXMTHS                                                       
         CP    GRPSAL,=P'0'        IF THERE IS NO SALARY-SKIP                   
         BNE   *+16                                                             
         LA    RE,GRPDTLQ(RE)                                                   
         BCT   R2,*-14                                                          
         B     GRPR120                                                          
         DROP  RE                                                               
*                                                                               
         GOTO1 CLRSORT,DMCB,ASRTREC                                             
         TM    FLAG2,FLGFH         ARE WE READING FH                            
         BNO   GRPR30                                                           
         CLC   GRPGRP,=C'50'       GROUP 50 - LDA                               
         BNE   GRPR30                                                           
         OI    FLAG,FLGLDA         SET FLAG TO SHOW WE'RE DOING LDA             
GRPR30   CLC   SVGRP,XSPACES       ARE WE ON THE FIRST ONE?                     
         BE    GRPR40                                                           
         OC    GRPGRP,GRPGRP       ANY GROUP?                                   
         BZ    GRPR40                                                           
         CLC   SVGRP,GRPGRP        ARE WE DOING THE SAME GROUP?                 
         BE    GRPR40                                                           
*                                                                               
         MVI   RCSUBPRG,1          DEPT TOTAL PAGE                              
         L     R5,ADPTREC          THE DEPT TOTAL RECORD                        
         BRAS  RE,DETAIL           PROCESS AS DETAIL RECORD                     
         GOTO1 CLRSORT,DMCB,ADPTREC CLEAR DEPT RECORD                           
*                                                                               
         MVI   RCSUBPRG,2          GROUP TOTAL PAGE                             
         L     R5,AGRPREC          THE GROUP TOTAL RECORD                       
         BRAS  RE,DETAIL           PROCESS AS DETAIL RECORD                     
         GOTO1 CLRSORT,DMCB,AGRPREC CLEAR DEPT RECORD                           
         L     R5,ASRTREC          SET R5 BACK TO PERSON SORT REC               
         MVI   RCSUBPRG,5          RESET TO GROUP PAGE                          
         B     GRPR50                                                           
*                                                                               
GRPR40   CLC   SVDPT,XSPACES       ARE WE ON THE FIRST ONE?                     
         BE    GRPR50                                                           
         OC    GRPDPT,GRPDPT       ANY DEPT?                                    
         BZ    GRPR50                                                           
         CLC   SVDPT,GRPDPT        ARE WE DOING THE SAME DEPT?                  
         BE    GRPR50                                                           
*                                                                               
         MVI   RCSUBPRG,1          DEPT TOTAL PAGE                              
         L     R5,ADPTREC          THE DEPT TOTAL RECORD                        
         BRAS  RE,DETAIL           PROCESS AS DETAIL RECORD                     
         GOTO1 CLRSORT,DMCB,ADPTREC CLEAR DEPT RECORD                           
         L     R5,ASRTREC          SET R5 BACK TO PERSON SORT REC               
         MVI   RCSUBPRG,5          RESET TO GROUP PAGE                          
*                                                                               
GRPR50   MVC   SRACC(L'GRPLOC),GRPLOC   DEPT/OFF/TITLE/PERSON                   
         MVC   SRSTAT,GRPSTAT           STATUS BYTE                             
         MVC   SVDPT,GRPDPT             SAVE OFF DEPARTMENT                     
*                                                                               
         MVC   SVGRP,=C'00'             ASSUME IT IS UNKNOWN                    
         MVC   SVGRPNM,=CL20'UNKNOWN'                                           
         OC    GRPGRP,GRPGRP                                                    
         BZ    GRPR80                                                           
*                                                                               
         USING BIND,RE                                                          
         L     RE,APRDBUFF         R1=PRODUCT TABLE                             
         ICM   R2,15,BININ                                                      
         BZ    GRPR80                                                           
         USING PRDTBD,R1                                                        
         LA    R1,BINTABLE                                                      
         MVC   SVGRP(L'SVGRP+L'SVGRPNM),XSPACES                                 
GRPR60   CLC   GRPGRP,PRDGRP       IS THIS ONE OF THE CLI/PRD                   
         BNE   GRPR70                                                           
         MVC   SVGRP,PRDGRP        SAVE GROUP FROM TABLE                        
         MVC   SVGRPNM,PRDGRPNM    SAVE GROUP NAME FROM TABLE                   
         B     GRPR80                                                           
*                                                                               
GRPR70   LA    R1,PRDLNQ(R1)                                                    
         BCT   R2,GRPR60                                                        
         DROP  R1,RE                                                            
*                                                                               
         USING SDTD,R6                                                          
GRPR80   LA    R6,SRDTL                                                         
         USING GRPDTD,R3                                                        
         LA    R3,GRPDTL                                                        
         LA    R2,MAXMTHS                                                       
*                                                                               
         ZAP   SRYTPHR,GRPYTPHR    YTD PRODUCT HOURS                            
GRPR90   LA    RE,SDSAL            SORT RECORD  SALARY                          
         LA    RF,GRPSAL           GROUP RECORD SALARY                          
         LA    R1,GRPDTLQ1         BUCKETS NOT INCLUDING PST/YPST               
GRPR100  ZAP   0(SDBUKLN,RE),0(SDBUKLN,RF)                                      
         LA    RE,SDBUKLN(RE)                                                   
         LA    RF,SDBUKLN(RF)                                                   
         BCT   R1,GRPR100                                                       
*                                                                               
         ZAP   SDPST,GRPPST        MONTHLY POSTED                               
         ZAP   SDYPST,GRPYPST      YTD POSTED                                   
         ZAP   SDYTCHR,GRPYTCHR    YTD CLIENT  HOURS                            
         ZAP   SDTPST,GRPTPST      YTD TOTAL POSTED                             
         TM    FLAG2,FLGFH         ARE WE READING FH                            
         BNO   GRPR110                                                          
         CLC   GRPGRP,=C'50'       ARE WE DOING LDA CREATIVE?                   
         BNE   GRPR110                                                          
         ZAP   SDPRDHR,GRPLDAHR    USE LDA HOURS AS MONTHLY PROD HOURS          
         ZAP   SDYPHRS,GRPYLHRS    USE LDA HOURS AS YTD PROD HOURS              
GRPR110  LA    R6,SDLEQ(R6)                                                     
         LA    R3,GRPDTLQ(R3)                                                   
         BCT   R2,GRPR90                                                        
*                                                                               
         LA    RF,PCT              FIGURE OUT PCT OF TIME-DEF PCT               
         TM    SRSTAT,SRADJ+SRHRS+GRPDUP+GRPDUPLC                               
         BZ    *+8                                                              
         LA    RF,PCTADJ           YES-USE PCTADJ TO FIGURE PCT                 
         BASR  RE,RF               FIGURE OUT PCT OF TIME                       
*                                                                               
         MVC   MSGS,XSPACES                                                     
         BRAS  RE,REFRESH                                                       
         BRAS  RE,DETAIL                                                        
         GOTO1 =A(TOTIT),DMCB,(RC)   ADD TO DEPT/GROUP TOTALS                   
         TM    FLAG2,FLGFH         ARE WE READING FH                            
         BNO   *+14                                                             
         CLC   GRPGRP,=C'50'         ARE WE DOING LDA CREATIVE?                 
         BE    GRPR120                                                          
         BRAS  RE,PSTCAPG            POST TO RECAP AND QUARTER REPORT           
GRPR120  LA    R4,GRPLEN(R4)                                                    
         BCT   R0,GRPR20                                                        
*                                                                               
         MVI   RCSUBPRG,1           DEPT/(GRP) TOTAL PAGE                       
         L     R5,ADPTREC           THE DEPT TOTAL RECORD                       
         BRAS  RE,DETAIL            PROCESS AS DETAIL RECORD                    
         MVI   RCSUBPRG,2           GROUP TOTAL PAGE                            
         L     R5,AGRPREC           THE GROUP TOTAL RECORD                      
         BRAS  RE,DETAIL            PROCESS AS DETAIL RECORD                    
         MVI   RCSUBPRG,3           REQUEST TOTAL PAGE                          
         L     R5,AREQREC           THE REQUEST TOTAL RECORD                    
         BRAS  RE,DETAIL            PROCESS AS DETAIL RECORD                    
*                                                                               
GRPRX    DS    0H                                                               
         NI    REPBYTE,X'FF'-REPC                                               
         DROP  R4,R5,R6                                                         
         EJECT                                                                  
**********************************************************************          
* ANALYSIS OF PERSONNEL TIME AND COST RECAP-GROUP                    *          
**********************************************************************          
         SPACE 1                                                                
GRPRCAP  DS    0H                                                               
         LA    RE,XP                                                            
         LA    RF,SVPRNLNQ                                                      
         SR    R1,R1                                                            
         ICM   R1,8,XSPACES                                                     
         MVCL  RE,R0                                                            
*                                                                               
         CLI   QOPT7,C'Y'          WAS DOWNLOAD OPTION SELECTED?                
         BNE   GRPRC05                                                          
         CLI   QOPT4,C' '          WAS ANY REPORT SELECTED                      
         BE    *+12                                                             
         CLI   QOPT4,C'D'          WAS GROUP RECAP SELECTED                     
         BNE   *+8                                                              
         MVI   REPBYTE,REPD                                                     
GRPRC05  CLI   QOPT1,C'Y'          RUN GROUP REPORT?                            
         BNE   GRPRCX              GO TO DO ANALYSIS REPORT                     
         MVC   PAGE,=H'1'                                                       
         MVI   RCSUBPRG,6                                                       
         ZAP   COUNT,ZEROS                                                      
         MVC   XP,XSPACES          CLEAR ANYTHING LEFT IN XP                    
         XC    LSTGRP,LSTGRP       CLEAR GROUP CODE FIELD                       
*                                                                               
         LA    R0,DPTTOTN          NUMBER OF ACCUMS                             
         L     R1,ADPTTOTS                                                      
         ZAP   0(DPTQBLN,R1),ZEROS                                              
         LA    R1,DPTQBLN(R1)                                                   
         BCT   R0,*-10                                                          
*                                                                               
         USING TCAPD,R4                                                         
         L     R4,ARCAPTOT                                                      
         MVC   TCAPKEY(TCAKLEN),XSPACES  CLEAR RECAP TOTAL AREA                 
         BAS   RE,GRPRC10                                                       
         L     R4,ATCAPWK                                                       
         MVC   TCAPKEY(TCAKLEN),XSPACES  CLEAR RECAP AREA                       
         BAS   RE,GRPRC10                                                       
         B     GRPRC20                                                          
GRPRC10  LA    R1,TCABK                                                         
         LA    R0,TCACNT                                                        
         ZAP   0(TCABKLN,R1),ZEROS                                              
         LA    R1,TCABKLN(R1)                                                   
         BCT   R0,*-10                                                          
         BR    RE                                                               
*                                                                               
GRPRC20  MVI   TCAPTYP,TCAPGPQ                                                  
         GOTO1 BUFFALO,DMCB,(L'HIGHC,HIGHC),ADBUFF,(R4),0                       
         CLI   DMCB+8,0                                                         
         BNE   GRPRCX              GO TO DO ANALYSIS REPORT                     
         CLI   TCAPTYP,TCAPGPQ                                                  
         BNE   GRPRCX                                                           
         B     GRPRC40                                                          
*                                                                               
GRPRC30  LA    R0,TCAPGPQ                                                       
         L     R4,ATCAPWK                                                       
         GOTO1 BUFFALO,DMCB,(L'SEQC,SEQC),((R0),ADBUFF),(R4),0                  
         TM    8(R1),EOF                                                        
         BO    GRPRCX              DO MAIN RECAP REPORT                         
*                                                                               
         USING TCAPD,R4                                                         
GRPRC40  L     R4,ARCAPTOT                                                      
         MVC   TCAPKEY(TCAKLEN),XSPACES  CLEAR RECAP TOTAL AREA                 
         L     R4,ATCAPWK                                                       
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         USING BIND,RE                                                          
         L     RE,APRDBUFF         R1=PRODUCT TABLE                             
         ICM   R2,15,BININ                                                      
         BZ    GRPRC70                                                          
         USING PRDTBD,R1                                                        
         LA    R1,BINTABLE                                                      
         MVC   SVGRP(L'SVGRP+L'SVGRPNM),XSPACES                                 
GRPRC50  CLC   TCAPGRP,PRDGRP      IS THIS ONE OF THE CLI/PRD                   
         BNE   GRPRC60                                                          
         MVC   SVGRP,PRDGRP        SAVE GROUP FROM TABLE                        
         MVC   SVGRPNM,PRDGRPNM    SAVE GROUP NAME FROM TABLE                   
         B     GRPRC70                                                          
*                                                                               
GRPRC60  LA    R1,PRDLNQ(R1)                                                    
         BCT   R2,GRPRC50                                                       
         DROP  R1,RE                                                            
*                                                                               
         MVC   SVGRP,=C'**'                USE AS DEFAULT                       
         MVC   SVGRPNM,=CL20'CONSOLIDATED'                                      
         USING QTRD,RE                                                          
GRPRC70  L     RE,QTRMTAB          UPDATE QTR TABLE                             
GRPRC80  CLI   QTRMTH,EOT          MUST FIND A MONTH MATCH                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   TCAPMNTH,QTRMTH                                                  
         BE    *+12                                                             
         LA    RE,QTRDLN(RE)                                                    
         B     GRPRC80                                                          
         CLI   QOPT7,C'Y'                                                       
         BE    *+10                                                             
         MVC   XP+4(L'QTRMNAME),QTRMNAME                                        
         MVC   SVMNTH,QTRMNAME                                                  
         MVC   SVQTR,QTRNUMB       SAVE QUARTER NUMBER                          
         DROP  RE                                                               
         CP    COUNT,=P'8'                                                      
         BNE   *+8                                                              
         MVI   FORCEHED,C'Y'       9TH MONTH IN REQ TO NEW PAGE                 
         GOTO1 UNDERLIN,DMCB,(20,XP+4),(X'BF',XPSECOND+4)                       
         MVC   XPTHIRD+6(5),=C'HOURS'                                           
         MVC   XPFOURTH+6(4),=C'COST'                                           
         LA    R6,XPTHIRD+19       HOURS                                        
         LA    R7,XPFOURTH+19      COST                                         
         BRAS  RE,XFOOT            CROSSFOOT AND ADD TO TOTALS                  
         BRAS  RE,FRMRCAP          FORMAT MONTHLY TOTALS                        
         MVI   SPACING,2                                                        
         BRAS  RE,OUTREC           UPDATE HEADLINES AND PRINT                   
         LA    R0,TCAPGPQ                                                       
         GOTO1 BUFFALO,DMCB,(L'SEQC,SEQC),((R0),ADBUFF),(R4),0                  
         TM    8(R1),EOF                                                        
         BO    GRPRCX              DO QUARTERLY ANALYSIS REPORT                 
         CLI   TCAPYR,X'FF'                                                     
         BNE   GRPRC70             PROCESS NEXT MONTH                           
*                                                                               
         MVC   XP+4(14),=C'12 MONTH TOTAL'                                      
         GOTO1 UNDERLIN,DMCB,(20,XP+4),(X'BF',XPSECOND+4)                       
         BRAS  RE,OUTREC                                                        
         L     R4,ARCAPTOT         USE TOTAL WORK AREA                          
         MVC   XP+6(5),=C'HOURS'                                                
         MVC   XPTHIRD+6(4),=C'COST'                                            
         LA    R6,XP+19            HOURS                                        
         LA    R7,XPTHIRD+19       COST                                         
         BRAS  RE,FRMRCAP          FORMAT 12 TOTALS                             
         MVC   XPSECOND+4(19),=C'MANPOWER EQUIVALENT'                           
         MVC   XPFOURTH+4(15),=C'% OF TIME SPENT'                               
         BRAS  RE,EQUIV            GET EQUIVALENT HOURS                         
         BRAS  RE,OUTREC                                                        
         MVI   SPACING,2                                                        
         BRAS  RE,OUTREC                                                        
         MVI   SPACING,2                                                        
         BRAS  RE,OUTREC                                                        
         MVC   XP+70(42),=CL42'XXXXXX HOURS = (ONE) FULL TIME PERSON'           
         EDIT  MANHOURS,(6,XP+70),DROP=2 1540 OR 770 OR 1610                    
         MVC   XPSECOND+89(17),=C'FOR TWELVE MONTHS'                            
         CP    MANHOURS,=P'077000'                                              
         BNE   *+10                                                             
         MVC   XPSECOND+89(17),=C'FOR SIX MONTHS   '                            
         BRAS  RE,OUTREC                                                        
*                                                                               
* CLEAR RECAP TOTALS                                                            
*                                                                               
         USING TCAPD,R4                                                         
         L     R4,ARCAPTOT                                                      
         MVC   TCAPKEY(TCAKLEN),XSPACES  CLEAR RECAP TOTAL AREA                 
         BAS   RE,GRPRC90                                                       
         L     R4,ATCAPWK                                                       
         MVC   TCAPKEY(TCAKLEN),XSPACES  CLEAR RECAP AREA                       
         BAS   RE,GRPRC90                                                       
         B     GRPRC30                                                          
GRPRC90  LA    R1,TCABK                                                         
         LA    R0,TCACNT                                                        
         ZAP   0(TCABKLN,R1),ZEROS                                              
         LA    R1,TCABKLN(R1)                                                   
         BCT   R0,*-10                                                          
         BR    RE                                                               
*                                                                               
GRPRCX   DS    0H                                                               
         NI    REPBYTE,X'FF'-REPD                                               
         EJECT                                                                  
**********************************************************************          
* ANALYSIS (QUARTERLY) OF PERSONNEL TIME AND COST-GROUP              *          
**********************************************************************          
         SPACE 1                                                                
         USING PLINED,R7                                                        
GRPANL   DS    0H                                                               
         LA    RE,XP                                                            
         LA    RF,SVPRNLNQ                                                      
         SR    R1,R1                                                            
         ICM   R1,8,XSPACES                                                     
         MVCL  RE,R0                                                            
*                                                                               
         CLI   QOPT7,C'Y'          WAS DOWNLOAD OPTION SELECTED?                
         BNE   GRPAN05                                                          
         CLI   QOPT4,C' '          WAS ANY REPORT SELECTED                      
         BE    *+12                                                             
         CLI   QOPT4,C'E'          WAS GROUP ANALYSIS SELECTED                  
         BNE   *+8                                                              
         MVI   REPBYTE,REPE                                                     
GRPAN05  LA    R7,XP                                                            
         CLI   QOPT1,C'Y'          RUN GROUP REPORT?                            
         BNE   GRPANX                                                           
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         MVI   RCSUBPRG,7                                                       
         USING TCQD,R4                                                          
         L     R4,ATCQWK                CLEAR HOURS WORK AREA                   
         MVC   TCQKEY(TCQKLEN),XSPACES  CLEAR  AREA                             
         LA    R1,TCQBK                                                         
         LA    R0,TCQCNT                                                        
         ZAP   0(TCQBKLN,R1),ZEROS                                              
         LA    R1,TCQBKLN(R1)                                                   
         BCT   R0,*-10                                                          
         XC    SVAPGC,SVAPGC                                                    
         XC    SVGRP,SVGRP                                                      
         MVI   TCQTYP,TCQGPQ                                                    
         GOTO1 BUFFALO,DMCB,(L'HIGHC,HIGHC),ADBUFF,(R4),0                       
         CLI   DMCB+8,0                                                         
         BNE   GRPANX                                                           
         CLI   TCQTYP,TCQGPQ                                                    
         BNE   GRPANX                                                           
*                                                                               
GRPAN10  MVC   SVPERSON,XSPACES    NEED TO CLEAR PERSON FOR DWNLD               
         CLI   TCQGRP,X'FF'                                                     
         BE    GRPANX              DONE                                         
         CLI   TCQDEPT,X'FE'                                                    
         BE    GRPAN60             GROUP TOTAL HOURS                            
         CLI   TCQDEPT,X'FF'                                                    
         BE    GRPAN70             GROUP TOTAL COST                             
         CLI   TCQSUB,X'FE'                                                     
         BE    GRPAN80             DEPT TOTAL HOURS                             
         CLI   TCQSUB,X'FF'                                                     
         BE    GRPAN90             DEPT TOTAL COST                              
         CLI   TCQPRSN,X'FE'                                                    
         BE    GRPAN100            SUB/DEPT TOTAL HOURS                         
*                                                                               
* PRINT GROUP NAME                                                              
*                                                                               
         OC    SVGRP,SVGRP                                                      
         BNZ   GRPAN30                                                          
         USING BIND,RE                                                          
         L     RE,APRDBUFF         R1=PRODUCT TABLE                             
         ICM   R2,15,BININ                                                      
         BZ    GRPANX                                                           
         USING PRDTBD,R1                                                        
         LA    R1,BINTABLE                                                      
         MVC   SVGRP(L'SVGRP+L'SVGRPNM),XSPACES                                 
         CLC   TCQGRP,PRDGRP                                                    
         BE    GRPAN20                                                          
         LA    R1,PRDLNQ(R1)                                                    
         BCT   R2,*-14                                                          
         B     GRPAN30                                                          
*                                                                               
GRPAN20  MVC   SVGRP,PRDGRP        SAVE GROUP FROM TABLE                        
         MVC   SVGRPNM,PRDGRPNM                                                 
*        MVC   P+2(L'SVGRP),SVGRP                                               
*        MVC   P+5(L'PRDGRPNM),PRDGRPNM                                         
         MVC   PGANGRP,SVGRP                                                    
         MVC   PGANGRPN,PRDGRPNM                                                
         GOTO1 UNDERLIN,DMCB,(23,XP+2),(X'BF',XPSECOND+2)                       
         MVI   SPACING,2                                                        
         DROP  R1,RE                                                            
*                                                                               
* PRINT DEPARTMENT NAME                                                         
*                                                                               
GRPAN30  OC    SVAPGDPT,SVAPGDPT                                                
         BNZ   GRPAN40             ALREADY PRINTED DEPT                         
         MVC   WORK,XSPACES                                                     
         MVC   WORK(L'TCQDEPT),TCQDEPT                                          
         MVC   SVAPGDPT,WORK                                                    
         MVC   SVDPT,SVAPGDPT                                                   
         L     R5,APGBUFF          LOOK IN APG TABLE                            
         GOTO1 =A(GTANME),DMCB,(RC)     GET APG DEPT NAME IN WORK               
         MVC   SVDPTNME,WORK                                                    
         MVC   PANDPT,SVAPGDPT                                                  
         MVC   PANDPTNM,SVDPTNME                                                
         GOTO1 UNDERLIN,DMCB,(39,XPTHIRD+2),(X'BF',XPFOURTH+2)                  
         MVI   SPACING,2                                                        
         BRAS  RE,OUTREC                                                        
*                                                                               
* PRINT TITLE NAME                                                              
*                                                                               
GRPAN40  OC    SVAPGSUB,SVAPGSUB                                                
         BNZ   GRPAN50             ALREADY PRINTED TITLE                        
         MVC   SVAPGSUB,TCQSUB     TITLE                                        
         MVC   SVSUBNME,XSPACES                                                 
         MVC   SVSUBNME(L'SVAPGSUB),SVAPGSUB                                    
         MVC   XP+5(36),SVSUBNME                                                
         GOTO1 UNDERLIN,DMCB,(36,XP+5),(X'BF',XPSECOND+5)                       
         MVI   SPACING,2                                                        
         BRAS  RE,OUTREC                                                        
*                                  NOW DO THE PERSON                            
GRPAN50  MVC   WORK,XSPACES                                                     
         MVC   WORK(L'TCQPRSN),TCQPRSN  PERSON                                  
         L     R5,TIBUFF           LOOK IN TITLE/PERSON TABLE                   
         GOTO1 =A(GTANME),DMCB,(RC)  GET APG NAME                               
         MVC   XP+7(36),WORK                                                    
         MVC   SVPERSON,WORK       SAVE OFF PERSON FOR DOWNLOAD                 
         B     GRPAN110                                                         
*                                                                               
* *****  GROUP TOTAL HOURS *****                                                
*                                                                               
GRPAN60  BRAS  RE,OUTREC           SKIP A LINE                                  
         MVC   XP+5(10),=C'TOTALS FOR'                                          
         MVC   WORK,XSPACES                                                     
         MVC   WORK(L'SVGRPNM),SVGRPNM                                          
         MVC   WORK+21(5),=C'HOURS'                                             
         GOTO1 ADSQUASH,DMCB,WORK,27                                            
         SR    R1,R1                                                            
         IC    R1,DMCB+7                                                        
         SH    R1,=H'10'                                                        
         GOTO1 CHOPPER,DMCB,(27,WORK),((R1),XP+16),(L'XP,2)                     
         MVI   SPACING,2                                                        
         B     GRPAN110                                                         
*                                                                               
* *****  GROUP TOTAL COST *****                                                 
*                                                                               
GRPAN70  BRAS  RE,OUTREC                SKIP A LINE                             
         MVC   XP+5(10),=C'TOTALS FOR'                                          
         MVC   WORK,XSPACES                                                     
         MVC   WORK(L'SVGRPNM),SVGRPNM                                          
         MVC   WORK+21(5),=C'COSTS'                                             
         GOTO1 ADSQUASH,DMCB,WORK,27                                            
         SR    R1,R1                                                            
         IC    R1,DMCB+7                                                        
         SH    R1,=H'10'                                                        
         GOTO1 CHOPPER,DMCB,(27,WORK),((R1),XP+16),(L'XP,2)                     
         BRAS  RE,FRMC                                                          
         BRAS  RE,OUTREC                                                        
         XC    SVGRP,SVGRP                                                      
         MVI   FORCEHED,C'Y'                                                    
         B     GRPAN120                 SEQ                                     
*                                                                               
* *****  DEPT TOTAL HOURS *****                                                 
*                                                                               
GRPAN80  BRAS  RE,OUTREC           SKIP A LINE                                  
         MVC   XP+5(10),=C'TOTALS FOR'                                          
         MVC   WORK,XSPACES                                                     
         MVC   WORK(36),SVDPTNME                                                
         MVC   WORK+37(5),=C'HOURS'                                             
         GOTO1 ADSQUASH,DMCB,WORK,43                                            
         SR    R1,R1                                                            
         IC    R1,DMCB+7                                                        
         SH    R1,=H'10'                                                        
         GOTO1 CHOPPER,DMCB,(43,WORK),((R1),XP+16),(L'XP,2)                     
         MVI   SPACING,2                                                        
         B     GRPAN110                                                         
*                                                                               
* *****  DEPT TOTAL COST *****                                                  
*                                                                               
GRPAN90  BRAS  RE,OUTREC                SKIP A LINE                             
         MVC   XP+5(10),=C'TOTALS FOR'                                          
         MVC   WORK,XSPACES                                                     
         MVC   WORK(36),SVDPTNME                                                
         MVC   WORK+37(5),=C'COSTS'                                             
         GOTO1 ADSQUASH,DMCB,WORK,43                                            
         SR    R1,R1                                                            
         IC    R1,DMCB+7                                                        
         SH    R1,=H'10'                                                        
         GOTO1 CHOPPER,DMCB,(43,WORK),((R1),XP+16),(L'XP,2)                     
         XC    SVAPGDPT,SVAPGDPT                                                
         BRAS  RE,FRMC                                                          
         BRAS  RE,OUTREC                                                        
         MVI   FORCEHED,C'Y'                                                    
         B     GRPAN120                 SEQ                                     
*                                                                               
* *****  SUB TOTAL *****                                                        
*                                                                               
GRPAN100 BRAS  RE,OUTREC                SKIP A LINE                             
         MVC   XP+5(10),=C'TOTALS FOR'                                          
         MVC   WORK,XSPACES                                                     
         MVC   WORK(36),SVSUBNME                                                
         GOTO1 CHOPPER,DMCB,(36,WORK),(23,XP+16),(L'XP,2)                       
         XC    SVAPGSUB,SVAPGSUB                                                
         MVI   SPACING,2                                                        
*                                                                               
GRPAN110 BRAS  RE,FRMH             FORMAT REPORT 4 HOURS                        
         BRAS  RE,OUTREC                                                        
GRPAN120 LA    R0,TCQGPQ                                                        
         GOTO1 BUFFALO,DMCB,(L'SEQC,SEQC),((R0),ADBUFF),(R4),0                  
         CLI   DMCB+8,0                                                         
         BE    GRPAN10                                                          
         DC    H'0'                NO TOTAL RECORD                              
*                                                                               
GRPANX   DS    0H                                                               
         NI    REPBYTE,X'FF'-REPE                                               
         DROP  R4                                                               
         EJECT                                                                  
**********************************************************************          
* ANALYSIS OF PERSONNEL TIME AND COST RECAP                          *          
**********************************************************************          
         SPACE 1                                                                
RECAP    DS    0H                                                               
         LA    RE,XP                                                            
         LA    RF,SVPRNLNQ                                                      
         SR    R1,R1                                                            
         ICM   R1,8,XSPACES                                                     
         MVCL  RE,R0                                                            
*                                                                               
         CLI   QOPT7,C'Y'          WAS DOWNLOAD OPTION SELECTED?                
         BNE   RECP05                                                           
         CLI   QOPT4,C' '          WAS ANY REPORT SELECTED                      
         BE    *+12                                                             
         CLI   QOPT4,C'F'          WAS RECAP REPORT SELECTED                    
         BNE   *+8                                                              
         MVI   REPBYTE,REPF                                                     
*                                                                               
RECP05   NI    FLAG,X'FF'-FLGGRP   TURN OFF GROUP FLAG                          
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         MVI   RCSUBPRG,6                                                       
         MVC   MSGS,=CL35'CONSOLIDATED GRAND TOTAL'                             
         ZAP   COUNT,ZEROS                                                      
         MVC   XP,XSPACES          CLEAR ANYTHING LEFT IN XP                    
*                                                                               
         LA    R0,DPTTOTN          NUMBER OF ACCUMS                             
         L     R1,ADPTTOTS                                                      
         ZAP   0(DPTQBLN,R1),ZEROS                                              
         LA    R1,DPTQBLN(R1)                                                   
         BCT   R0,*-10                                                          
*                                                                               
         USING TCAPD,R4                                                         
         L     R4,ARCAPTOT                                                      
         MVC   TCAPKEY(TCAKLEN),XSPACES  CLEAR RECAP TOTAL AREA                 
         BAS   RE,RECP10                                                        
         L     R4,ATCAPWK                                                       
         MVC   TCAPKEY(TCAKLEN),XSPACES  CLEAR RECAP AREA                       
         BAS   RE,RECP10                                                        
         B     RECP20                                                           
RECP10   LA    R1,TCABK                                                         
         LA    R0,TCACNT                                                        
         ZAP   0(TCABKLN,R1),ZEROS                                              
         LA    R1,TCABKLN(R1)                                                   
         BCT   R0,*-10                                                          
         BR    RE                                                               
*                                                                               
RECP20   MVI   TCAPTYP,TCAPEQU                                                  
         GOTO1 BUFFALO,DMCB,(L'HIGHC,HIGHC),ADBUFF,(R4),0                       
         CLI   DMCB+8,0                                                         
         BNE   RECAXX              GO TO DO ANALYSIS REPORT                     
         CLI   TCAPTYP,TCAPEQU                                                  
         BNE   RECAXX                                                           
*                                                                               
         USING QTRD,RE                                                          
RECP30   L     RE,QTRMTAB          UPDATE QTR TABLE                             
RECP40   CLI   QTRMTH,EOT          MUST FIND A MONTH MATCH                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   TCAPMNTH,QTRMTH                                                  
         BE    *+12                                                             
         LA    RE,QTRDLN(RE)                                                    
         B     RECP40                                                           
         CLI   QOPT7,C'Y'          IF DOWNLOADING DO NOT MOVE IN P LINE         
         BE    *+10                                                             
         MVC   XP+4(L'QTRMNAME),QTRMNAME                                        
         MVC   SVMNTH,QTRMNAME                                                  
         MVC   SVQTR,QTRNUMB       SAVE QUARTER NUMBER                          
         DROP  RE                                                               
         CP    COUNT,=P'8'                                                      
         BNE   *+8                                                              
         MVI   FORCEHED,C'Y'       9TH MONTH IN REQ TO NEW PAGE                 
         GOTO1 UNDERLIN,DMCB,(20,XP+4),(X'BF',XPSECOND+4)                       
         MVC   XPTHIRD+6(5),=C'HOURS'                                           
         MVC   XPFOURTH+6(4),=C'COST'                                           
         LA    R6,XPTHIRD+19       HOURS                                        
         LA    R7,XPFOURTH+19      COST                                         
         BRAS  RE,XFOOT            CROSSFOOT AND ADD TO TOTALS                  
         BRAS  RE,FRMRCAP          FORMAT MONTHLY TOTALS                        
         MVI   SPACING,2                                                        
         BRAS  RE,OUTREC           UPDATE HEADLINES AND PRINT                   
         LA    R0,TCAPEQU                                                       
         GOTO1 BUFFALO,DMCB,(L'SEQC,SEQC),((R0),ADBUFF),(R4),0                  
         TM    8(R1),EOF                                                        
         BO    RECAXX              DO QUARTERLY ANALYSIS REPORT                 
         CLI   TCAPYR,X'FF'                                                     
         BNE   RECP30              PROCESS NEXT MONTH                           
         MVC   XP+4(14),=C'12 MONTH TOTAL'                                      
         GOTO1 UNDERLIN,DMCB,(20,XP+4),(X'BF',XPSECOND+4)                       
         BRAS  RE,OUTREC                                                        
         L     R4,ARCAPTOT         USE TOTAL WORK AREA                          
         MVC   XP+6(5),=C'HOURS'                                                
         MVC   XPTHIRD+6(4),=C'COST'                                            
         LA    R6,XP+19            HOURS                                        
         LA    R7,XPTHIRD+19       COST                                         
         BRAS  RE,FRMRCAP          FORMAT 12 TOTALS                             
         MVC   XPSECOND+4(19),=C'MANPOWER EQUIVALENT'                           
         MVC   XPFOURTH+4(15),=C'% OF TIME SPENT'                               
         BRAS  RE,EQUIV            GET EQUIVALENT HOURS                         
         BRAS  RE,OUTREC                                                        
         MVI   SPACING,2                                                        
         BRAS  RE,OUTREC                                                        
         MVI   SPACING,2                                                        
         BRAS  RE,OUTREC                                                        
         MVC   XP+70(42),=CL42'XXXXXX HOURS = (ONE) FULL TIME PERSON'           
         EDIT  MANHOURS,(6,XP+70),DROP=2 1540 OR 770 HOURS                      
         MVC   XPSECOND+89(17),=C'FOR TWELVE MONTHS'                            
         CP    MANHOURS,=P'077000' IS MANHOURS=770                              
         BNE   *+10                NO, DON'T CHANGE NARRATIVE                   
         MVC   XPSECOND+89(17),=C'FOR SIX MONTHS   '                            
         BRAS  RE,OUTREC                                                        
RECAXX   DS    0H                                                               
         NI    REPBYTE,X'FF'-REPF                                               
         EJECT                                                                  
**********************************************************************          
* ANALYSIS (QUARTERLY) OF PERSONNEL TIME AND COST                    *          
**********************************************************************          
         SPACE 1                                                                
         USING PLINED,R7                                                        
ANAL     DS    0H                                                               
         LA    RE,XP                                                            
         LA    RF,SVPRNLNQ                                                      
         SR    R1,R1                                                            
         ICM   R1,8,XSPACES                                                     
         MVCL  RE,R0                                                            
*                                                                               
         CLI   QOPT7,C'Y'          WAS DOWNLOAD OPTION SELECTED?                
         BNE   ANAL05                                                           
         CLI   QOPT4,C' '          WAS ANY REPORT SELECTED                      
         BE    *+12                                                             
         CLI   QOPT4,C'G'          WAS ANALYSIS REPORT SELECTED                 
         BNE   *+8                                                              
         MVI   REPBYTE,REPG                                                     
         MVC   SVGRP,XSPACES                                                    
         MVC   SVGRPNM,XSPACES                                                  
         MVC   SVSUBNME,XSPACES                                                 
ANAL05   LA    R7,XP                                                            
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         MVI   RCSUBPRG,7                                                       
         MVC   MSGS,=CL35'CONSOLIDATED GRAND TOTAL'                             
         USING TCQD,R4                                                          
         L     R4,ATCQWK                CLEAR HOURS WORK AREA                   
         MVC   TCQKEY(TCQKLEN),XSPACES  CLEAR  AREA                             
         LA    R1,TCQBK                                                         
         LA    R0,TCQCNT                                                        
         ZAP   0(TCQBKLN,R1),ZEROS                                              
         LA    R1,TCQBKLN(R1)                                                   
         BCT   R0,*-10                                                          
         XC    SVAPGC,SVAPGC                                                    
         MVI   TCQTYP,TCQEQU                                                    
         GOTO1 BUFFALO,DMCB,(L'HIGHC,HIGHC),ADBUFF,(R4),0                       
         CLI   DMCB+8,0                                                         
         BNE   ANALXX                                                           
         CLI   TCQTYP,TCQEQU                                                    
         BNE   ANALXX                                                           
*                                                                               
ANAL10   DS    0H                                                               
         MVC   SVPERSON,XSPACES                                                 
         CLI   TCQDEPT,X'FF'                                                    
         BE    ANALXX              DONE                                         
*                                                                               
         CLC   RQPROD,XSPACES      IF PROD WAS REQUESTED CHECK TOTAL            
         BE    *+14                                                             
         CP    TCTOT,=P'0'         DON'T REPORT ANY ZERO TOTALS                 
         BE    ANAL80                                                           
*                                                                               
         CLI   TCQSUB,X'FE'                                                     
         BE    ANAL40              DEPT TOTAL HOURS                             
         CLI   TCQSUB,X'FF'                                                     
         BE    ANAL50              DEPT TOTAL COST                              
         CLI   TCQPRSN,X'FE'                                                    
         BE    ANAL60              SUB/DEPT TOTAL HOURS                         
         OC    SVAPGDPT,SVAPGDPT                                                
         BNZ   ANAL20              ALREADY PRINTED DEPT                         
*                                                                               
* PRINT DEPARTMENT NAME                                                         
*                                                                               
         MVC   WORK,XSPACES                                                     
         MVC   WORK(L'TCQDEPT),TCQDEPT                                          
         MVC   SVAPGDPT,WORK                                                    
         MVC   SVDPT,SVAPGDPT                                                   
         L     R5,APGBUFF          LOOK IN APG TABLE                            
         GOTO1 =A(GTANME),DMCB,(RC)     GET APG DEPT NAME IN WORK               
         MVC   SVDPTNME,WORK                                                    
*        MVC   XP+2(L'SVAPGDPT),SVAPGDPT                                        
         MVC   PANDPT,SVAPGDPT                                                  
*        MVC   XP+5(36),SVDPTNME                                                
         MVC   PANDPTNM,SVDPTNME                                                
         GOTO1 UNDERLIN,DMCB,(39,PANDPT),(X'BF',XPSECOND+2)                     
         MVI   SPACING,2                                                        
         BRAS  RE,OUTREC                                                        
*                                                                               
ANAL20   DS    0H                                                               
         OC    SVAPGSUB,SVAPGSUB                                                
         BNZ   ANAL30              ALREADY PRINTED TITLE                        
*                                                                               
* PRINT TITLE NAME                                                              
*                                                                               
         MVC   SVAPGSUB,TCQSUB     TITLE                                        
         MVC   SVSUBNME,XSPACES                                                 
         MVC   SVSUBNME(L'SVAPGSUB),SVAPGSUB                                    
         MVC   PANTITLE,SVSUBNME                                                
         GOTO1 UNDERLIN,DMCB,(36,PANTITLE),(X'BF',XPSECOND+5)                   
         MVI   SPACING,2                                                        
         BRAS  RE,OUTREC                                                        
*                                  NOW DO THE PERSON                            
ANAL30   MVC   WORK,XSPACES                                                     
         MVC   WORK(L'TCQPRSN),TCQPRSN  PERSON                                  
         L     R5,TIBUFF           LOOK IN TITLE/PERSON TABLE                   
         GOTO1 =A(GTANME),DMCB,(RC)      GET APG NAME                           
*        MVC   XP+7(36),WORK                                                    
         MVC   PANPRSN,WORK                                                     
         MVC   SVPERSON,WORK                                                    
         B     ANAL70                                                           
*                                                                               
* *****  DEPT TOTAL HOURS *****                                                 
*                                                                               
ANAL40   BRAS  RE,OUTREC           SKIP A LINE                                  
*        MVC   XP+5(10),=C'TOTALS FOR'                                          
         MVC   PANGTOT,=C'TOTALS FOR'                                           
         MVC   WORK(36),SVDPTNME                                                
         MVC   WORK+37(5),=C'HOURS'                                             
         GOTO1 ADSQUASH,DMCB,WORK,43                                            
         ZIC   R1,DMCB+7                                                        
         SH    R1,=H'10'                                                        
         GOTO1 CHOPPER,DMCB,(43,WORK),((R1),PANGTOTD),(L'XP,2)                  
         MVI   SPACING,2                                                        
         B     ANAL70                                                           
*                                                                               
* *****  DEPT TOTAL COST *****                                                  
*                                                                               
ANAL50   BRAS  RE,OUTREC                SKIP A LINE                             
         MVC   PANGTOT,=C'TOTALS FOR'                                           
         MVC   WORK(36),SVDPTNME                                                
         MVC   WORK+37(5),=C'COSTS'                                             
         GOTO1 ADSQUASH,DMCB,WORK,43                                            
         SR    R1,R1                                                            
         IC    R1,DMCB+7                                                        
         SH    R1,=H'10'                                                        
         GOTO1 CHOPPER,DMCB,(43,WORK),((R1),PANGTOTD),(L'XP,2)                  
         XC    SVAPGDPT,SVAPGDPT                                                
         BRAS  RE,FRMC                                                          
         BRAS  RE,OUTREC                                                        
         MVI   FORCEHED,C'Y'                                                    
         B     ANAL80                   SEQ                                     
*                                                                               
* *****  SUB TOTAL *****                                                        
*                                                                               
ANAL60   BRAS  RE,OUTREC                SKIP A LINE                             
         MVC   PANGTOT,=C'TOTALS FOR'                                           
         MVC   WORK(36),SVSUBNME                                                
         GOTO1 CHOPPER,DMCB,(36,WORK),(23,PANGTOTD),(L'XP,2)                    
         XC    SVAPGSUB,SVAPGSUB                                                
         MVI   SPACING,2                                                        
*                                                                               
ANAL70   BRAS  RE,FRMH             FORMAT REPORT 4 HOURS                        
         BRAS  RE,OUTREC                                                        
ANAL80   LA    R0,TCQEQU                                                        
         GOTO1 BUFFALO,DMCB,(L'SEQC,SEQC),((R0),ADBUFF),(R4),0                  
         CLI   DMCB+8,0                                                         
         BE    ANAL10                                                           
         DC    H'0'                NO TOTAL RECORD                              
*                                                                               
ANALXX   DS    0H                                                               
         NI    REPBYTE,X'FF'-REPG                                               
         B     COM2XIT                                                          
*--------------------------------------------------------------------           
         LTORG                                                                  
*--------------------------------------------------------------------           
         DROP  R4,RB                                                            
         EJECT                                                                  
**********************************************************************          
* ADD DEPT TOTALS TO DUPLICATE DEPT TABLE FOR DUPREP                 *          
*     R5 = A(DEPT TOT RECORD)                                        *          
**********************************************************************          
         SPACE 1                                                                
         USING SRTD,RE                                                          
SAVDPT   NTR1  BASE=*,LABEL=*                                                   
         L     RE,ADUPREC                                                       
         LA    R0,MAXDPTS          MAXIMUM NUMBER OF DUP DEPT TOTS              
         OC    SRDEPT,SRDEPT                                                    
         JZ    *+14                                                             
         LA    RE,SRTLEN(RE)                                                    
         BCT   R0,*-14                                                          
         DC    H'0'                IF NO ROOM BUMP MAX                          
*                                                                               
         ST    RE,SAVERE           SAVE RE FOR AFTER MOVE                       
         LA    RF,SRTLEN                                                        
         LR    R1,RF                                                            
         LR    R0,R5               A(DEPT RECORD)                               
         MVCL  RE,R0               SAVE OFF DEPT REC TO DUP DEPT TOT            
*                                                                               
         L     RE,SAVERE           RESET ADDRESS                                
         USING SDTD,RF                                                          
         LA    RF,SRTOTAL          R6=A(SORT TOTAL DETAIL)                      
         LA    R0,SDNUMBK          CLEAR TOTALS                                 
         LA    R1,SDSAL                                                         
         ZAP   0(SDBUKLN,R1),=P'0'                                              
         LA    R1,SDBUKLN(R1)                                                   
         BCT   R0,*-10                                                          
*                                                                               
         J     COM2XIT                                                          
*--------------------------------------------------------------------           
         LTORG                                                                  
*--------------------------------------------------------------------           
         DROP  RB,RE,RF                                                         
         EJECT                                                                  
**********************************************************************          
* SAVE TOTAL LINE BEFORE ADJUSTMENTS                                 *          
*      R5 = A(SORT RECORD)                                           *          
**********************************************************************          
         SPACE 1                                                                
         USING SRTD,R5                                                          
SAVTOT   NTR1  BASE=*,LABEL=*                                                   
*        CLI   SDMNTH,X'FF'        TOTAL LINE?                                  
*        BE    SAVTX                                                            
*                                                                               
         USING SDTD,R6                                                          
         LA    R6,SRDTL                                                         
         LA    R2,MAXMTHS          NUMBER OF TIMES TO LOOP                      
SAVT10   CP    SDCLIHR,ZEROS       WORK ON TOYOTA THIS MONTH?                   
         BNE   SAVT20              YES-CONTINUE                                 
         CP    SDSAL,ZEROS         SALARY DOLLARS?                              
         BNE   SAVT20              YES-                                         
         CP    SDPST,ZEROS         DOLLARS ON TOYOTA THIS MONTH?                
         BE    SAVT40              NO- SKIP THIS MONTH                          
*                                                                               
SAVT20   DS    0H                                                               
         LA    RE,SVSRTTOT         SAVED AREA FOR TOTAL LINE                    
         LA    RF,SDTD                                                          
         L     R4,SUMPST                                                        
         USING SUMPD,R4                                                         
SAVT30   LR    R3,RF                                                            
         CLI   SUMPDISP,EOT                                                     
         BE    SAVT40                                                           
         AH    R3,SUMPDISP                                                      
         TM    SUMPSTAT,SUMPADD                                                 
         BZ    *+14                                                             
         AP    0(SDBUKLN,RE),0(SDBUKLN,R3) ADD OR ZAP PER TABLE STAT            
         B     *+10                                                             
         ZAP   0(SDBUKLN,RE),0(SDBUKLN,R3)                                      
         LA    RE,SDBUKLN(RE)                                                   
         LA    R4,SUMPLN(R4)                                                    
         B     SAVT30                                                           
*                                                                               
SAVT40   LA    R6,SDLEQ(R6)                 NEXT MONTH                          
         BCT   R2,SAVT10                                                        
*                                                                               
SAVTX    J     COM2XIT                                                          
*--------------------------------------------------------------------           
         LTORG                                                                  
*--------------------------------------------------------------------           
         DROP  R4,R5,R6,RB                                                      
         EJECT                                                                  
**********************************************************************          
* ADJUST POSTINGS IF NECESSARY                                       *          
**********************************************************************          
         SPACE 1                                                                
         USING SRTD,R5                                                          
         USING SDTD,R6                                                          
ADJUST   NTR1  BASE=*,LABEL=*                                                   
         L     R5,ASRTREC                                                       
         LA    R6,SRDTL                                                         
         LA    R4,MAXMTHS          MAX MONTHS                                   
         ZAP   PACK8,SDYPST        YTD-1 POSTINGS TO TOYOTA                     
         CP    SDYPHRS,ADJHOURS    THIS CAN'T HAPPEN 1ST TIME                   
         BL    ADJ10                                                            
         DC    H'0'                                                             
*                                                                               
ADJ02    CP    SDYPHRS,ADJHOURS                                                 
         BL    ADJ10                                                            
         ZAP   DUB,SDSALY          CURRENT YTD SALARY                           
         SP    DUB,PACK8           MINUS (YTD-1 POSTED SALARY)                  
         ZAP   SDPST,DUB           =NEW CURRENT MTH POSTING                     
         ZAP   SDYPST,DUB                                                       
         AP    SDYPST,PACK8        YTD-1 POSTING  + MTH = YTD POSTING           
         OI    SDSTAT,SDADJUST     MARK ADJUSTED                                
         OI    SRSTAT,SRHRS        MARK PERSON AS REACHING 1800 HRS             
         SR    R1,R1                                                            
         IC    R1,SDMNUM           CURRENT MONTH NUMBER                         
         SR    R0,R0                                                            
         IC    R0,RQMNTHS          NUMBER OF MONTHS IN REQUEST                  
         SR    R0,R1                                                            
         LTR   R0,R0               NUMBER OF YTD BUCKETS TO UPDATE              
         BNP   ADJXX                                                            
ADJ08    LA    R6,SDLEQ(R6)        ADJUST FOR 100% SALARY                       
         ZAP   SDPST,SDSAL                                                      
         ZAP   SDYPST,SDSALY                                                    
         OI    SDSTAT,SDADJUST     MARK ADJUSTED                                
         OI    SRSTAT,SRHRS        MARK PERSON AS REACHING 1800 HRS             
         BCT   R0,ADJ08                                                         
         B     ADJXX                                                            
*                                                                               
ADJ10    ZAP   PACK8,SDYPST        YTD-1 POSTINGS TO TOYOTA                     
         LA    R6,SDLEQ(R6)                                                     
         BCT   R4,ADJ02                                                         
ADJXX    DS    0H                                                               
         J     COM2XIT                                                          
*--------------------------------------------------------------------           
         LTORG                                                                  
*--------------------------------------------------------------------           
         DROP  R5,R6,RB                                                         
         EJECT                                                                  
**********************************************************************          
* PROCESS COST AS A PCT OF SALARY AND TIME                           *          
* R5 SET TO ADDRESS OF SORT RECORD COMING IN                         *          
**********************************************************************          
         SPACE 1                                                                
PCT      NTR1  BASE=*,LABEL=*                                                   
         USING SRTD,R5                                                          
         USING SDTD,R6                                                          
         LA    R6,SRDTL            MONTHLY DETAIL                               
         ZAP   MINUS1,ZEROS        SET YTD-1                                    
*                                                                               
         LA    R2,MAXMTHS          ALL MONTHS                                   
PCT10    OC    SDMNTH,SDMNTH                                                    
         BZ    PCT40               GET NEXT MONTH                               
*                                                                               
         CLC   RQCLIENT,=C'TMS'    ARE WE RUNNING TOYOTA?                       
         BNE   PCT20               MUST BE LEXUS                                
         LA    R1,SDYTHRS          YTD TOTAL 1C HOURS                           
         CP    SDYCHRS,ADJHOURS    IF CLI HRS ARE OVER 1800 HRS                 
         BL    *+8                                                              
         LA    R1,SDYCHRS          USE YTD CLIENT HOURS FROM ABOVE              
         ZAP   DUB,0(SDBUKLN,R1)                                                
         B     PCT30                                                            
*                                                                               
PCT20    LA    RE,SDYCHRS          ASSUME IT'S YTD CLIENT HRS                   
         TM    FLAG,FLGGRP         ARE WE RUNNING GROUP REPORT?                 
         BNO   *+8                                                              
         LA    RE,SDYTCHR          USE ACTUAL CLIENT HOURS INSTEAD              
*                                                                               
         LA    R1,SDYTHRS              YTD TOTAL 1C HOURS                       
         CP    0(SDBUKLN,RE),ADJHOURS  IF CLI HRS ARE OVER 1800 HRS             
         BL    *+6                                                              
         LR    R1,RE                   USE YTD CLIENT HOURS FROM ABOVE          
         ZAP   DUB,0(SDBUKLN,R1)                                                
*                                                                               
PCT30    ZAP   SDYPST,ZEROS                                                     
         ZAP   SDPST,ZEROS                                                      
         CP    SDYPHRS,ZEROS       YTD WORK ON PRODUCT THIS MONTH?              
         BE    PCT40               NO- SKIP THIS MONTH                          
         ZAP   PACK16,SDYPHRS      ACTUAL TOYOTA HOURS YTD                      
         SRP   PACK16,6,0          SHIFT 6 POSITIONS TO LEFT                    
         ZAP   SDYPCT,ZEROS                                                     
         CP    DUB,ZEROS           FIND ANY HOURS?                              
         BZ    *+16                                                             
         DP    PACK16,DUB          CLIENT BY TOTAL YTD                          
         ZAP   SDYPCT,PACK16(L'PACK16-L'DUB)                                    
*        TM    FLAG,FLGGRP         ARE WE RUNNING GROUP REPORT?                 
*        BO    *+10                IF WE ARE DONT ROUND YET                     
         SRP   SDYPCT,64-2,5                                                    
*                                                                               
* BECAUSE TOYOTA YEAR IS DIFFERENT THAN SAATCHI YEAR WE COULD NOT USE           
* REAL POSTED DOLLARS AS INTENDED SO WE NEED THIS CALC                          
*                                                                               
         ZAP   SDYPST,ZEROS                                                     
         ZAP   SDPST,ZEROS                                                      
         ZAP   PACK16,SDSALY       YTD SALARY                                   
         MP    PACK16,SDYPCT       X  YTD PERCENT OF TIME ON TOYOTA             
         SRP   PACK16,64-4,5       SHIFT AND ROUND FOR NON GROUP                
*                                                                               
         ZAP   SDYPST,PACK16+(L'PACK16-8)(8) YTD SALARY COST                    
         ZAP   SDPST,SDYPST        MONTHLY = YTD  - (YTD-1)                     
         SP    SDPST,MINUS1                                                     
         BNM   *+8                                                              
         OI    FLAG2,FLGNEG        SALARY ADJUSTED BACKWARDS                    
         ZAP   MINUS1,SDYPST       UPDATE YTD-1                                 
*                                                                               
PCT40    DS    0H                                                               
*        MVC   MSG1,=CL10'SRT DTL'                                              
*        GOTO1 ADUMP,DMCB,(RC),(R6),SDLEQ                                       
         LA    R6,SDLEQ(R6)                 NEXT MONTH                          
         BCT   R2,PCT10                                                         
*                                                                               
PCTX     J     COM2XIT                                                          
*--------------------------------------------------------------------           
         LTORG                                                                  
*--------------------------------------------------------------------           
         DROP  R5,R6,RB                                                         
         EJECT                                                                  
**********************************************************************          
* PROCESS COST AS A PCT OF POSTED AMOUNT PREVIOUSLY SAVED OFF        *          
* R5 SET TO ADDRESS OF SORT RECORD COMING IN                         *          
**********************************************************************          
         SPACE 1                                                                
PCTADJ   NTR1  BASE=*,LABEL=*                                                   
         USING SRTD,R5                                                          
         USING SDTD,R6                                                          
         LA    R6,SRDTL            MONTHLY DETAIL                               
         ZAP   MINUS1,ZEROS        SET YTD-1                                    
*                                                                               
         LA    R2,MAXMTHS          ALL MONTHS                                   
PCTA10   ZAP   SDYPST,ZEROS                                                     
         ZAP   SDPST,ZEROS                                                      
         CP    SDYPHRS,ZEROS       YTD WORK ON PRODUCT THIS MONTH?              
         BE    PCTA20              NO- SKIP THIS MONTH                          
         ZAP   PACK16,SDYPHRS      ACTUAL TOYOTA HOURS YTD                      
         SRP   PACK16,6,0          SHIFT 6 POSITIONS TO LEFT                    
         ZAP   SDYPCT,ZEROS                                                     
         ZAP   DUB,SRYTPHR                                                      
         CP    DUB,ZEROS           FIND ANY HOURS?                              
         BZ    *+16                                                             
         DP    PACK16,DUB          CLIENT BY TOTAL YTD                          
         ZAP   SDYPCT,PACK16(L'PACK16-L'DUB)                                    
         SRP   SDYPCT,64-2,5                                                    
*                                                                               
* BECAUSE TOYOTA YEAR IS DIFFERENT THAN SAATCHI YEAR WE COULD NOT USE           
* REAL POSTED DOLLARS AS INTENDED SO WE NEED THIS CALC                          
*                                                                               
         ZAP   SDYPST,ZEROS                                                     
         ZAP   SDPST,ZEROS                                                      
         ZAP   PACK16,SDTPST       YTD ACTUAL TOTAL POSTED                      
         MP    PACK16,SDYPCT       X  YTD PERCENT OF TIME ON TOYOTA             
         SRP   PACK16,64-4,5       SHIFT AND ROUND                              
         ZAP   SDYPST,PACK16+(L'PACK16-8)(8) YTD SALARY COST                    
         ZAP   SDPST,SDYPST        MONTHLY = YTD  - (YTD-1)                     
         SP    SDPST,MINUS1                                                     
         ZAP   MINUS1,SDYPST       UPDATE YTD-1                                 
*                                                                               
PCTA20   LA    R6,SDLEQ(R6)                 NEXT MONTH                          
         BCT   R2,PCTA10                                                        
*                                                                               
PCTAX    J     COM2XIT                                                          
*--------------------------------------------------------------------           
         LTORG                                                                  
*--------------------------------------------------------------------           
         DROP  R5,R6,RB                                                         
         EJECT                                                                  
**********************************************************************          
* PROCESS A DETAIL RECORD                                            *          
*         R5 SET TO ADDRESS OF SORT RECORD COMING IN                 *          
**********************************************************************          
         SPACE 1                                                                
DETAIL   NTR1  BASE=*,LABEL=*                                                   
         MVI   FORCEHED,C'Y'                                                    
         USING SRTD,R5                                                          
         USING SDTD,R6                                                          
         LA    R6,SRDTL            MONTHLY DETAIL                               
*                                                                               
         USING PLINED,R7                                                        
         LA    R7,XPTHIRD                                                       
         MVC   PCOMMENT,XSPACES    CLEAR COMMENT TO START                       
         LA    R7,XPFOURTH                                                      
         MVC   PCOMMENT,XSPACES                                                 
         LA    R7,XP               RESTORE XP LINE                              
*                                                                               
         LA    R2,MAXMTHS+1        ALL MONTHS PLUS TOTAL                        
DET10    OC    SDMNTH,SDMNTH                                                    
         BZ    DET140              GET NEXT MONTH                               
         CLI   SDMNTH,X'FF'        TOTAL LINE?                                  
         BNE   DET40                                                            
         CLI   QOPT2,C'S'          SUPPRESS ALL REPORTS EXCEPT RECAPS           
         BE    *+8                                                              
         BRAS  RE,OUTREC           SKIP A LINE                                  
         MVC   PMNTH(5),=C'TOTAL'                                               
*                                                                               
         TM    FLAG,FLGGRP         ARE WE DOING GROUP REPORT?                   
         BNO   DET30                                                            
         TM    SRSTAT,SRADJ+GRPDUP+GRPDUPLC  ADJUSTED OR IN A DUP LOC?          
         BZ    DET30                                                            
         CLI   RCSUBPRG,C'6'                                                    
         BE    DET20                                                            
         CLI   RCSUBPRG,C'7'                                                    
         BE    DET20                                                            
*        CLI   QOPT2,C'S'          SUPPRESS ALL REPORTS EXCEPT RECAPS           
*        BE    DET60                                                            
*                                                                               
DET20    LA    R7,XPTHIRD                                                       
         MVC   PYPCT(41),=C'NOTE:SAL COST=SAL COST FROM 1ST RPT * PCT'          
         LA    R7,XPFOURTH                                                      
         MVC   PYPCT(33),=C'PCT=(GRP PRD HRS/TOT PRD HRS FROM'                  
         MVC   PMCOST+1(22),=C'MULTI-LOCATION REPORT)'                          
         LA    R7,XP               RESTORE XP LINE                              
         B     DET100                                                           
*                                                                               
DET30    DS    0H                                                               
         CLI   RCSUBPRG,0          REGULAR REPORT?                              
         BNE   DET100                                                           
*        CLI   QOPT2,C'S'          SUPPRESS ALL REPORTS EXCEPT RECAPS           
*        BE    DET60                                                            
         TM    ACCSTUS,DUPLOCAT    IF DUP                                       
         BZ    DET100                                                           
         LA    R7,XPTHIRD                                                       
         MVC   PCOMMENT(37),=C'NOTE: PERSON IS IN MULTIPLE LOCATIONS'           
         LA    R7,XPFOURTH                                                      
         MVC   PCOMMENT(37),=C'SEE MULTI LOCS SECTION AFTER THIS RPT'           
         LA    R7,XP               RESTORE XP LINE                              
         B     DET100                                                           
*                                                                               
DET40    CP    SDCLIHR,ZEROS       WORK ON TOYOTA THIS MONTH?                   
         BNE   DET50               YES-CONTINUE                                 
         CP    SDSAL,ZEROS         SALARY DOLLARS?                              
         BNE   DET50               YES-                                         
         CP    SDPST,ZEROS         DOLLARS ON TOYOTA THIS MONTH?                
         BE    DET140              NO- SKIP THIS MONTH                          
*                                                                               
DET50    TM    FLAG,FLGGRP         ARE WE DOING GROUP REPORT?                   
         BNO   DET60                                                            
         TM    SRSTAT,SRHRS        DID PERSON EXCEED 1800 HRS                   
         BNO   *+10                                                             
         MVC   MSGS,=CL35'*PERSON HAS EXCEEDED 1800 CLI HRS*'                   
*                                                                               
         TM    SRSTAT,SRADJ+GRPDUP+GRPDUPLC+SRHRS                               
         BNZ   DET80               SKIP ADITIONAL FIGURING                      
*                                                                               
DET60    LA    RE,SDYTHRS          YTD TOTAL 1C HOURS                           
         CP    SDYPHRS,ADJHOURS    IF PROD   HOURS ARE OVER 1800 HOURS          
         BL    *+14                                                             
         ZAP   SDYPCT,=P'10000'      FORCE PERCENT TO BE 100%                   
         B     DET70                                                            
         ZAP   DUB,0(SDBUKLN,RE)                                                
*                                                                               
         ZAP   PACK16,SDYPHRS      ACTUAL PRODUCT HOURS YTD                     
         SRP   PACK16,6,0          SHIFT 6 POSITIONS TO LEFT                    
         ZAP   SDYPCT,ZEROS                                                     
         CP    DUB,ZEROS           FIND ANY HOURS?                              
         BZ    *+16                                                             
         DP    PACK16,DUB          CLIENT BY TOTAL YTD                          
         ZAP   SDYPCT,PACK16(L'PACK16-L'DUB)                                    
         SRP   SDYPCT,64-2,5                                                    
*                                                                               
DET70    ZAP   PACK16,SDSALY       YTD SALARY                                   
         SRP   PACK16,6,0          SHIFT 6 POSITIONS TO LEFT                    
         ZAP   SDHRRAT,ZEROS                                                    
         ZAP   DUB,SDYTHRS                                                      
         CP    SDYCHRS,ADJHOURS    IF CLIENT HOURS ARE OVER 1800 HOURS          
         BL    *+10                                                             
         ZAP   DUB,SDYCHRS         USE YTD PRODUCT HOURS                        
*                                                                               
         CP    DUB,ZEROS           FIND ANY HOURS?                              
         BZ    *+16                                                             
         DP    PACK16,DUB          HOURLY RATE                                  
         ZAP   SDHRRAT,PACK16(L'PACK16-L'DUB)                                   
         SRP   SDHRRAT,64-4,5                                                   
*                                                                               
DET80    C     R5,ASRTREC          DON'T DO FACTOR FOR TOTAL RECS               
         BNE   DET90                                                            
         ZAP   PACK16,SDPST        MONTHLY X FACTOR A                           
         MP    PACK16,=PL8'22500'                                               
         SRP   PACK16,64-4,5       SHIFT AND ROUND                              
         ZAP   SDFACA,PACK16+(L'PACK16-8)(8)                                    
         ZAP   SDTOTAL,SDFACA      FACTOR A IS THE TOTAL                        
*        AP    SDTOTAL,SDPST       TOTAL                                        
*                                                                               
DET90    CLC   SDMNTH,SVSTART      DONT PRINT UNLESS IN REQUEST RANGE           
         BL    DET140                                                           
         MVC   WORK(2),SDMNTH                                                   
         MVI   WORK+2,1                                                         
         GOTO1 DATCON,DMCB,(1,WORK),(6,PMNTH)        MMM/YY                     
         TM    SDSTAT,SDADJUST     IS THIS MONTH ADJUSTED?                      
         BZ    *+8                                                              
         MVI   PMSTAT,C'*'                                                      
*                                                                               
DET100   DS    0H                                                               
         EDIT  SDSAL,PSAL,2,MINUS=YES,COMMAS=YES,ZERO=BLANK                     
         EDIT  SDPRDHR,PPHRS,2,MINUS=YES,COMMAS=YES,ZERO=BLANK                  
         EDIT  SDCLIHR,PCHRS,2,MINUS=YES,COMMAS=YES,ZERO=BLANK                  
         EDIT  SDTHRS,PTHRS,2,MINUS=YES,COMMAS=YES,ZERO=BLANK                   
         EDIT  SDYPHRS,PYPHRS,2,MINUS=YES,COMMAS=YES,ZERO=BLANK                 
         EDIT  SDYPCT,PYPCT,2,MINUS=YES,TRAIL=C'%'                              
         EDIT  SDHRRAT,PYRATE,2,MINUS=YES,COMMAS=YES,ZERO=BLANK                 
         EDIT  SDYPST,PYCOST,2,MINUS=YES,COMMAS=YES,ZERO=BLANK                  
         EDIT  SDPST,PMCOST,2,MINUS=YES,COMMAS=YES,ZERO=BLANK                   
         EDIT  SDTOTAL,PFIN,2,MINUS=YES,COMMAS=YES,ZERO=BLANK                   
*                                                                               
         TM    FLAG,FLGGRP         ARE WE DOING GROUP REPORT?                   
         BNO   DET110                                                           
         TM    SRSTAT,SRADJ+GRPDUP+GRPDUPLC   ADJUSTED OR IN DUP LOC?           
         BZ    DET110                                                           
         MVI   RCSUBPRG,8                                                       
         EDIT  SRYTPHR,PYTPHR,2,MINUS=YES,COMMAS=YES,ZERO=BLANK                 
         EDIT  SDYPCT,PYGRPPCT,2,MINUS=YES,TRAIL=C'%'                           
*                                                                               
DET110   CLI   SDMNTH,X'FF'        TOTAL LINE?                                  
         BE    DET130                                                           
         LA    RE,SRTOTAL          UPDATE TOTAL LINE                            
         LA    RF,SDTD                                                          
         L     R4,SUMPST                                                        
         USING SUMPD,R4                                                         
DET120   LR    R1,RE                                                            
         LR    R3,RF                                                            
         CLI   SUMPDISP,EOT                                                     
         BE    DET130                                                           
         AH    R1,SUMPDISP                  POINTS TO SORT ACCUMS               
         AH    R3,SUMPDISP                                                      
         TM    SUMPSTAT,SUMPADD                                                 
         BZ    *+14                                                             
         AP    0(SDBUKLN,R1),0(SDBUKLN,R3) ADD OR ZAP PER TABLE STAT            
         B     *+10                                                             
         ZAP   0(SDBUKLN,R1),0(SDBUKLN,R3)                                      
         LA    R4,SUMPLN(R4)                                                    
         B     DET120                                                           
*                                                                               
DET130   DS    0H                                                               
         CLI   QOPT2,C'S'          SUPPRESS ALL REPORTS EXCEPT RECAPS           
         BE    DET140                                                           
         BRAS  RE,OUTREC           UPDATE HEADLINES AND PRINT                   
DET140   LA    R6,SDLEQ(R6)        NEXT MONTH                                   
         BCT   R2,DET10                                                         
         J     COM2XIT                                                          
*--------------------------------------------------------------------           
         LTORG                                                                  
*--------------------------------------------------------------------           
         DROP  R4,R5,R6,R7,RB                                                   
         EJECT                                                                  
**********************************************************************          
* PRINT/DOWNLOAD INTERFACE                                           *          
**********************************************************************          
         SPACE 1                                                                
OUTREC   NTR1  BASE=*,LABEL=*                                                   
         CLI   QOPT7,C'Y'          WAS DOWNLOADING SELECTED?                    
         BNE   OUTR10                                                           
         CLI   REPBYTE,0           WAS A REPORT SELECTED                        
         BE    OUTRX                                                            
         OI    REPBYTE1,REPDWN      SOMETHING IS DOWNLOADING                    
*                                                                               
         LA    RE,SVPRNT                                                        
         LA    RF,SVPRNLNQ                                                      
         LR    R1,RF                                                            
         LA    R0,XP                                                            
         MVCL  RE,R0                                                            
*                                                                               
         GOTO1 ADWNRTE,DMCB,(RC)   IF SO POINT TO DOWNLOAD                      
         MVC   RCSUBPRG,SVSUBPRG   RESTORE RCSUBPRG                             
*                                                                               
         LA    RE,SVPRNT                                                        
         LA    RF,SVPRNLNQ                                                      
         LR    R1,RF                                                            
         LA    R0,XSPACES                                                       
         MVCL  RE,R0                                                            
*                                                                               
         B     OUTRX               SKIP HEAD UP ROUTINE                         
*                                                                               
OUTR10   BRAS  RE,HEADUP           PRINT AS NORMAL                              
*                                                                               
OUTRX    J     COM2XIT                                                          
*--------------------------------------------------------------------           
         LTORG                                                                  
*--------------------------------------------------------------------           
         DROP  RB                                                               
         EJECT                                                                  
**********************************************************************          
* POST TO RECAP REPORT - GROUP                                       *          
**********************************************************************          
         SPACE 1                                                                
         USING SRTD,R5                                                          
         USING SDTD,R6                                                          
PSTCAPG  NTR1  BASE=*,LABEL=*                                                   
         TM    FLAG,FLGGRP         ARE WE RUNNING GROUP REPORT?                 
         BNO   PSTCGX              NO-DON'T CONTINUE                            
         CLI   QOPT6,C'S'                                                       
         BE    PSTCGX              NO RECAP REPORT                              
         L     R5,ASRTREC          SORT RECORD                                  
         CLC   SRDEPT,=C'00'       UNKNOWN                                      
         BE    PSTCGX                                                           
         CLI   SRACCT,X'FF'                                                     
         BE    PSTCGX              DONT PROCESS TOTAL RECORDS                   
*                                                                               
         LA    R6,SRDTL            MONTHLY DETAIL                               
         USING TCAPD,R4                                                         
         L     R4,ATCAPWK                                                       
         MVC   TCAPKEY(TCAKLEN),XSPACES  CLEAR RECAP AREA                       
         LA    R1,TCABK                                                         
         LA    R0,TCACNT                                                        
         ZAP   0(TCABKLN,R1),ZEROS                                              
         LA    R1,TCABKLN(R1)                                                   
         BCT   R0,*-10                                                          
*                                                                               
         LA    R2,MAXMTHS          ALL MONTHS                                   
         MVC   SVKEY,XSPACES                                                    
         MVI   TCAPTYP,TCAPGPQ     RECORD TYPE-GROUP                            
         MVC   TCAPGRP,SVGRP       GROUP CODE                                   
         MVC   SVKEY(TCAKLEN),TCAPKEY                                           
PSTCG10  MVC   TCAPKEY,SVKEY       RESTORE KEY                                  
         OC    SDMNTH,SDMNTH                                                    
         BZ    PSTCG30             GET NEXT MONTH                               
         CLC   SDMNTH,SVSTART                                                   
         BL    PSTCG30             GET NEXT MONTH                               
         MVC   TCAPYYMM,SDMNTH     YEAR/MONTH                                   
         LA    R1,TCABK                                                         
         LA    R0,TCACNT                                                        
         ZAP   0(TCABKLN,R1),ZEROS                                              
         LA    R1,TCABKLN(R1)                                                   
         BCT   R0,*-10                                                          
         AP    TCAPHT,SDPRDHR      ADD MNTHLY HRS AND COST TO TOTAL             
         AP    TCAPCT,SDTOTAL                                                   
         L     RF,DPTBUCK          READ FH BUCKETS TABLE                        
         TM    FLAG2,FLGFH         ARE WE READING FH SUPERLEDGER                
         BO    *+8                                                              
         L     RF,DPTBKFR          READ FR BUCKETS TABLE                        
         USING DPTD,RF                                                          
PSTCG20  CLI   DPTNUMB,EOT                                                      
         BNE   *+6                                                              
         DC    H'0'                CAN'T MATCH DEPARTMENT                       
         CLC   DPTNUMB,SRDEPT                                                   
         BE    *+12                                                             
         LA    RF,DPTDLN(RF)                                                    
         B     PSTCG20                                                          
         LR    R3,R4               RECORD ADDRESS                               
         LR    R1,R4                                                            
         AH    R1,DPTHDISP         POINTS TO DEPT HOURS ACCUM                   
         AH    R3,DPTCDISP         POINTS TO DEPT COST ACCUM                    
         AP    0(TCABKLN,R1),SDPRDHR                                            
         AP    0(TCABKLN,R3),SDTOTAL                                            
         DROP  RF                                                               
*                                                                               
         GOTO1 BUFFALO,DMCB,(L'PUTC,PUTC),ADBUFF,(R4)                           
         MVC   TCAPGRP,=X'FFFF'    GROUP TOTAL PAGE                             
         BASR  RE,RF                                                            
         MVC   TCAPGRP,SVGRP                                                    
         MVC   TCAPYYMM,=X'FFFF'   GROUP TOTAL LINE                             
         BASR  RE,RF                                                            
         MVC   TCAPGRP,=X'FFFF'                                                 
         BASR  RE,RF                                                            
PSTCG30  LA    R6,SDLEQ(R6)        NEXT MONTH                                   
         BCT   R2,PSTCG10                                                       
PSTCGX   DS    0H                                                               
         DROP  R4,R5,R6                                                         
         EJECT                                                                  
**********************************************************************          
* POST TO QTRLY REPORT - GROUP                                       *          
**********************************************************************          
         SPACE 1                                                                
         USING SRTD,R5                                                          
         USING SDTD,R6                                                          
PSTQTRG  DS    0H                                                               
         TM    FLAG,FLGGRP         ARE WE RUNNING GROUP REPORT?                 
         BNO   PSTQGX              NO-DON'T CONTINUE                            
*                                                                               
         CLI   QOPT8,C'S'                                                       
         BE    PSTQGX              NO TIME COST REPORT                          
         L     R5,ASRTREC          SORT RECORD                                  
         CLC   SRDEPT,=C'00'       UNKNOWN                                      
         BE    PSTQGX                                                           
         CLI   SRACCT,X'FF'                                                     
         BE    PSTQGX              DONT PROCESS TOTAL RECORDS                   
*                                                                               
         LA    R6,SRDTL            MONTHLY DETAIL                               
GRPHRS   USING TCQD,R4                                                          
         L     R4,ATCQWK           CLEAR HOURS WORK AREA                        
         MVC   GRPHRS.TCQKEY(TCQKLEN),XSPACES CLEAR AREA                        
         LA    R1,GRPHRS.TCQBK                                                  
         BAS   RE,PSTQG10                                                       
GRPCST   USING TCQD,R3                                                          
         L     R3,ATCQWK2          CLEAR COST WORK AREA                         
         MVC   GRPCST.TCQKEY(TCQKLEN),XSPACES                                   
         LA    R1,GRPCST.TCQBK                                                  
         BAS   RE,PSTQG10                                                       
         B     PSTQG20                                                          
*                                                                               
PSTQG10  LA    R0,TCQCNT                                                        
         ZAP   0(TCQBKLN,R1),ZEROS                                              
         LA    R1,TCQBKLN(R1)                                                   
         BCT   R0,*-10                                                          
         BR    RE                                                               
*                                                                               
PSTQG20  LA    R2,MAXMTHS+1                     ALL MONTHS PLUS TOTAL           
         MVC   SVKEY,XSPACES                                                    
         MVI   GRPHRS.TCQTYP,TCQGPQ             RECORD TYPE-GROUP               
         MVC   GRPHRS.TCQGRP,SVGRP              GROUP CODE                      
         MVC   GRPHRS.TCQDEPT,SRDEPT            APG DEPT                        
         MVC   GRPHRS.TCQTITL,SRTITLE           APG SUBDEPT                     
         MVC   GRPHRS.TCQSUB,SUBNAME            TITLE                           
         MVC   GRPHRS.TCQPRSN,SRPERSON          PERSON                          
         MVC   SVKEY(TCQKLEN),GRPHRS.TCQKEY                                     
         MVC   GRPCST.TCQKEY,GRPHRS.TCQKEY                                      
PSTQG30  MVC   GRPHRS.TCQKEY,SVKEY              RESTORE KEY                     
         MVC   GRPCST.TCQKEY,SVKEY                                              
         OC    SDMNTH,SDMNTH                                                    
         BZ    PSTQG60             GET NEXT MONTH                               
         CLC   SDMNTH,SVSTART                                                   
         BL    PSTQG60             GET NEXT MONTH                               
         CLI   SDMNTH,X'FF'                                                     
         BE    PSTQG50                                                          
         L     RE,QTRMTAB          QTRS TABLE                                   
         USING QTRD,RE                                                          
PSTQG40  CLI   QTRMTH,EOT                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   SDMNTH+1(1),QTRMTH  WHAT QTR DOES MONTH BELONG TO?               
         BE    *+12                                                             
         LA    RE,QTRDLN(RE)                                                    
         B     PSTQG40                                                          
         SR    RF,RF                                                            
         IC    RF,QTRNUMB                                                       
         BCTR  RF,0                                                             
         MH    RF,=Y(TCQBKLN)                                                   
         STH   RF,HALF                                                          
         LA    R1,GRPHRS.TCQBK                                                  
         AH    R1,HALF             POINTS TO QTR ACCUM                          
         DROP  RE                                                               
*                                                                               
*                                  * * * H 0 U R S * * *                        
         AP    0(TCQBKLN,R1),SDPRDHR        ADD TO QTR                          
         AP    GRPHRS.TCTOT,SDPRDHR         ADD TO TOTAL                        
         B     *+14                                                             
PSTQG50  ZAP   GRPHRS.TCTIME,SDYPCT    % OF TIME ON TOYOTA(TOT REC)             
         B     PSTQG60                                                          
*                                                                               
*                                  * * * C O S T * * *                          
         LA    R1,GRPCST.TCQBK                                                  
         AH    R1,HALF             POINTS TO QTR ACCUM                          
         AP    0(TCQBKLN,R1),SDTOTAL                                            
         AP    GRPCST.TCTOT,SDTOTAL   ADD TO TOTAL                              
*                                                                               
PSTQG60  LA    R6,SDLEQ(R6)        NEXT MONTH                                   
         BCT   R2,PSTQG30                                                       
                                                                                
*                                  HOURS RECORD                                 
         DS    0H                                                               
         GOTO1 BUFFALO,DMCB,(L'PUTC,PUTC),ADBUFF,ATCQWK                         
         XC    GRPHRS.TCQPRSN,GRPHRS.TCQPRSN                                    
         MVI   GRPHRS.TCQPRSN,X'FE'                                             
         BASR  RE,RF               SUB-DEPT TOTAL                               
         XC    GRPHRS.TCQSUB,GRPHRS.TCQSUB                                      
         MVC   GRPHRS.TCQSUB(2),=X'FE00'                                        
         MVC   GRPHRS.TCQTITL,=X'FFFF'                                          
         BASR  RE,RF               DEPT TOTALS                                  
         MVC   GRPHRS.TCQDEPT(3),=X'FE0000'                                     
         BASR  RE,RF               GROUP TOTALS                                 
         MVC   GRPHRS.TCQGRP(5),=X'FFFFFF0000'                                  
         BASR  RE,RF               REPORT TOTALS                                
*                                                                               
*                                  COST RECORD                                  
         XC    GRPCST.TCQPRSN,GRPCST.TCQPRSN                                    
         XC    GRPCST.TCQSUB,GRPCST.TCQSUB                                      
         MVC   GRPCST.TCQSUB(2),=X'FF00' GROUP  TOTALS FOR COST                 
         MVC   GRPCST.TCQTITL,=X'FFFF'                                          
         GOTO1 BUFFALO,DMCB,(L'PUTC,PUTC),ADBUFF,ATCQWK2                        
         MVC   GRPCST.TCQDEPT(3),=X'FF0000'                                     
         BASR  RE,RF                     REPORT TOTALS FOR COST                 
PSTQGX   DS    0H                                                               
         J     COM2XIT                                                          
*--------------------------------------------------------------------           
         LTORG                                                                  
*--------------------------------------------------------------------           
         DROP  R5,R6,RB                                                         
         DROP  GRPHRS,GRPCST                                                    
         EJECT                                                                  
**********************************************************************          
* REFRESH NAMES                                                      *          
**********************************************************************          
         SPACE 1                                                                
REFRESH  NTR1  BASE=*,LABEL=*                                                   
         USING SRTD,R5                                                          
         L     R5,ASRTREC                                                       
         CLC   SRDEPT,APGDPT       CHANGE IN DEPT?                              
         BE    REFR10                                                           
         MVC   APGNAME,XSPACES                                                  
         MVC   APGDPT,XSPACES                                                   
         CLI   SRDEPT,X'FF'        HIGHER LEVEL SORT REC- SKIP IT               
         BE    REFR10                                                           
         MVC   WORK,XSPACES                                                     
         LA    R3,WORK                                                          
         USING APGD,R3                                                          
         MVC   APGREC(L'SRDEPT),SRDEPT                                          
*                                                                               
         GOTO1 BINSRC,DMCB,WORK,APGBUFF,WORK                                    
         MVC   APGNAME,APGNME                                                   
         MVC   APGDPT,APGREC                                                    
*                                                                               
REFR10   CLC   SROFFICE,DPTCODE                                                 
         BE    REFR20                                                           
         MVC   DPTCODE,XSPACES                                                  
         MVC   DPTNAME,XSPACES                                                  
         CLI   SROFFICE,X'FF'                                                   
         BE    REFR20                                                           
         MVC   WORK,XSPACES                                                     
         LA    R3,WORK                                                          
         USING NMED,R3                                                          
         MVC   NMEREC(L'SROFFICE),SROFFICE                                      
         GOTO1 BINSRC,DMCB,WORK,NMEBUFF,WORK                                    
         MVC   DPTCODE,NMEREC                                                   
         MVC   DPTNAME,NMENAME                                                  
*                                                                               
REFR20   CLC   SUBCODEC,SRACCT     SUB-DEPT                                     
         BE    REFR30                                                           
         MVC   SUBCODE,XSPACES                                                  
         MVC   SUBCODEC,XSPACES                                                 
         MVC   SUBNAME,XSPACES                                                  
         CLI   SRTITLE,X'FF'                                                    
         BE    REFR30                                                           
         MVC   WORK,XSPACES                                                     
         LA    R3,WORK                                                          
         USING NMED,R3                                                          
         SR    R1,R1                                                            
         IC    R1,LLEVC                                                         
         SH    R1,=H'1'                                                         
         EX    R1,*+4                                                           
         MVC   NMEREC(0),SRACCT                                                 
         GOTO1 BINSRC,DMCB,WORK,NMEBUFF,WORK                                    
         MVC   SUBCODEC,NMEREC                                                  
         MVC   SUBCODE,SRTITLE                                                  
         MVC   SUBNAME,NMENAME                                                  
*                                                                               
REFR30   CLC   PRSCODEC,SRACCT     OFF/DPT/SUB/PRS                              
         BE    REFRXX                                                           
         CLI   SRTITLE,X'FF'                                                    
         BNE   *+14                                                             
         CLC   PRSCODE,SRPERSON                                                 
         BE    REFRXX                                                           
         MVC   PRSCODE,XSPACES                                                  
         MVC   PRSCODEC,XSPACES                                                 
         MVC   PRSNAME,XSPACES                                                  
         MVC   WORK,XSPACES                                                     
         LA    R3,WORK                                                          
         USING NMED,R3                                                          
         MVC   NMEREC(SRAKLNQ),SRACCT                                           
         GOTO1 BINSRC,DMCB,WORK,NMEBUFF,WORK                                    
         MVC   PRSCODEC,NMEREC                                                  
         MVC   PRSCODE,SRPERSON                                                 
         MVC   PRSNAME,NMENAME                                                  
*                                                                               
REFRXX   J     COM2XIT                                                          
*--------------------------------------------------------------------           
         LTORG                                                                  
*--------------------------------------------------------------------           
         DROP  R3,R5,RB                                                         
         EJECT                                                                  
**********************************************************************          
* HEADUP A NEW PAGE                                                  *          
**********************************************************************          
         SPACE 1                                                                
HEADUP   NTR1  BASE=*,LABEL=*                                                   
         CLC   RQPROD,XSPACES                                                   
         BNH   HDUP10                                                           
         LA    R1,XHEAD3           PUT PROD NAME AT BEGINNING OF 3RD            
         CLI   RCSUBPRG,8          HEADLINE FOR RCSUB #6 AND #7 THE             
         BE    *+12                RECAP AND ANALYSIS REPORTS                   
         CLI   RCSUBPRG,5          PUT IN THE MIDDLE OF 3RD LINE FOR            
         BH    *+8                 ALL THE OTHER REPORTS                        
         LA    R1,49(R1)                                                        
         MVC   0(L'RQPROD,R1),RQPROD                                            
         MVC   5(L'SVPRDNM,R1),SVPRDNM                                          
*                                                                               
HDUP10   TM    FLAG,FLGGRP         ARE WE DOING GROUP REPORT?                   
         BNO   *+18                                                             
         CLI   RCSUBPRG,2                                                       
         BH    *+10                                                             
         MVC   MSGS,=CL35'**********GROUP  TOTALS**********'                    
*                                                                               
         CLI   RCSUBPRG,8          GROUP REPORT FOR ADJ PERSON                  
         BE    HDUP20                                                           
         CLI   RCSUBPRG,5          0,1,2,3,4,5                                  
         BH    HDUP80                                                           
         CLI   RCSUBPRG,3          REQ TOTAL PAGE?                              
         BE    HDUP60                                                           
         CLI   RCSUBPRG,4          DUP LOCATION PAGE?                           
         BE    HDUP30                                                           
*                                                                               
HDUP20   CLC   SVGRP,XSPACES       ANY GROUP?                                   
         BNH   *+22                                                             
         MVC   XHEAD4(5),=C'GROUP'                                              
         MVC   XHEAD4+12(L'SVGRP),SVGRP                                         
         MVC   XHEAD4+22(L'SVGRPNM),SVGRPNM                                     
*                                                                               
         CLI   RCSUBPRG,2                  GROUP TOTAL PAGE?                    
         BE    HDUP60                                                           
*                                                                               
         MVC   XHEAD5(10),LLEVBNAM                                              
         MVC   XHEAD5+12(L'APGDPT),APGDPT                                       
         MVC   XHEAD5+22(L'APGNAME),APGNAME                                     
*                                                                               
         CLI   RCSUBPRG,1                  DEPT TOTAL PAGE?                     
         BE    HDUP60                                                           
         MVC   XHEAD6(10),LLEVANAM                                              
         MVC   XHEAD6+12(L'DPTCODE),DPTCODE                                     
         MVC   XHEAD6+22(L'DPTNAME),DPTNAME                                     
         MVC   XHEAD7(10),LLEVCNAM                                              
         MVC   XHEAD7+12(L'SUBCODE),SUBCODE                                     
         MVC   XHEAD7+22(L'SUBNAME),SUBNAME                                     
         MVC   XHEAD8(10),LLEVDNAM                                              
         MVC   XHEAD8+12(L'PRSCODE),PRSCODE                                     
         MVC   XHEAD8+22(L'PRSNAME),PRSNAME                                     
         B     HDUP50                                                           
*                                                                               
HDUP30   MVC   XHEAD5(10),LLEVDNAM         DUP LOCATION PAGE                    
         MVC   XHEAD5+12(L'PRSCODE),PRSCODE                                     
         MVC   XHEAD5+22(L'PRSNAME),PRSNAME                                     
         CLI   APGDPT,SPACE                                                     
         BE    HDUP40                                                           
         MVC   XHEAD6(10),LLEVBNAM                                              
         MVC   XHEAD6+12(L'APGDPT),APGDPT                                       
         MVC   XHEAD6+22(L'APGNAME),APGNAME                                     
         MVC   XHEAD7(10),LLEVANAM                                              
         MVC   XHEAD7+12(L'DPTCODE),DPTCODE                                     
         MVC   XHEAD7+22(L'DPTNAME),DPTNAME                                     
         MVC   XHEAD8(10),LLEVCNAM                                              
         MVC   XHEAD8+12(L'SUBCODE),SUBCODE                                     
         MVC   XHEAD8+22(L'SUBNAME),SUBNAME                                     
*                                                                               
HDUP40   MVC   XHEAD4+101(21),=C'*********************'                         
         MVC   XHEAD5+101(21),=C'** MULTI LOCATIONS **'                         
         MVC   XHEAD6+101(21),=C'*********************'                         
         B     HDUP60                                                           
*                                                                               
HDUP50   TM    FLAG,FLGGRP         ADDITIONAL GROUP REPORT-ADJ PERSON           
         BNO   HDUP60                                                           
         MVC   XHEAD4+101(21),=C'*********************'                         
         MVC   XHEAD5+101(21),=C'** REPORT BY GROUP **'                         
         MVC   XHEAD6+101(21),=C'*********************'                         
         MVC   XHEAD8+95(L'MSGS),MSGS                                           
*                                                                               
HDUP60   MVC   XHEAD1+46(35),=C'  T O Y O T A  F E E  R E P O R T  '            
         CLC   =C'LEX',RQCLIENT                                                 
         BNE   *+10                                                             
         MVC   XHEAD1+46(35),=C'   L E X U S  F E E  R E P O R T   '            
         MVC   XHEAD2+46(35),=C'  -------------------------------  '            
*                                                                               
         CLI   RCSUBPRG,1                                                       
         BE    HDUP70                                                           
         CLI   RCSUBPRG,2                                                       
         BE    HDUP70                                                           
         CLI   RCSUBPRG,3                                                       
         BNE   HDUPX                                                            
HDUP70   MVC   XHEAD5+90(L'MSGS),MSGS                                           
         MVC   XPTHIRD,XSPACES                                                  
         MVC   XPFOURTH,XSPACES                                                 
         B     HDUPX                                                            
*                                                                               
HDUP80   CLI   RCSUBPRG,6          RECAP REPORT                                 
         BL    HDUPX                                                            
         CLI   RCSUBPRG,7          TIME/COST REPORT                             
         BH    HDUPX                                                            
         TM    FLAG,FLGGRP         ADDITIONAL GROUP REPORT-ADJ PERSON           
         BNO   HDUP90                                                           
         MVC   XHEAD4+59(12),=C'  BY GROUP  '                                   
         MVC   XHEAD5+59(12),=C'************'                                   
         MVC   XHEAD4(5),=C'GROUP'                                              
         MVC   XHEAD4+12(L'SVGRP),SVGRP                                         
         MVC   XHEAD4+22(L'SVGRPNM),SVGRPNM                                     
*                                                                               
HDUP90   CLI   RCSUBPRG,6          RECAP REPORT                                 
         BE    HDUP100                                                          
         MVC   XHEAD5(10),LLEVBNAM                                              
         MVC   XHEAD5+12(L'SVDPT),SVDPT                                         
         MVC   XHEAD5+22(L'SVDPTNME),SVDPTNME                                   
*                                                                               
*                                                                               
HDUP100  MVC   XHEAD1+53(24),=C'TOYOTA MOTOR SALES, INC.'                       
         CLC   =C'LEX',RQCLIENT                                                 
         BNE   *+10                                                             
         MVC   XHEAD1+53(07),=C' LEXUS '                                        
         TM    FLAG,FLGGRP         ADDITIONAL GROUP REPORT-ADJ PERSON           
         BO    *+10                                                             
         MVC   XHEAD2+95(L'MSGS),MSGS                                           
         CLI   RCSUBPRG,6          RECAP REPORT                                 
         BNE   HDUPX                                                            
         MVC   XHEAD6+96(28),=CL28' ACCOUNT         MEDIA'                      
         TM    FLAG2,FLGFH          ARE WE DOING FH/OLD REPORT                  
         BO    HDUPX                                                            
         MVC   XHEAD6+96(28),=CL28' MEDIA           ACCOUNT'                    
*                                                                               
HDUPX    DS    0H                                                               
         GOTO1 ACREPORT                                                         
         J     COM2XIT                                                          
*--------------------------------------------------------------------           
         LTORG                                                                  
*--------------------------------------------------------------------           
         DROP  RB                                                               
         EJECT                                                                  
**********************************************************************          
* CROSSFOOT TO TOTALS                                                *          
**********************************************************************          
         SPACE 1                                                                
         USING TCAPD,R4                                                         
XFOOT    NTR1  BASE=*,LABEL=*                                                   
         L     R4,ATCAPWK                                                       
         LA    RE,TCABK                                                         
         L     R4,ARCAPTOT                                                      
         LA    RF,TCABK                                                         
         LA    R1,TCACNT           NUMBER OF BUCKETS                            
XFT10    AP    0(TCABKLN,RF),0(TCABKLN,RE)                                      
         LA    RF,TCABKLN(RF)                                                   
         LA    RE,TCABKLN(RE)                                                   
         BCT   R1,XFT10                                                         
         L     R4,ATCAPWK          RESET                                        
*                                                                               
* POST DEPARTMENT TOTALS                                                        
*                                                                               
         USING DPTTD,R1                                                         
         L     R1,ADPTTOTS                                                      
         LA    R2,DPTTOTL          TOTAL FOR DEPT                               
         SR    R3,R3                                                            
         IC    R3,SVQTR                                                         
         BCTR  R3,0                                                             
         MH    R3,=Y(DPTQBLN)                                                   
         LA    R1,0(R3,R1)                                                      
         LA    R0,TCADPCNT         # OF DEPARTMENTS                             
         LA    RE,TCAPC10                   1ST COST BUCKET                     
XFT20    AP    0(DPTQBLN,R1),0(TCABKLN,RE)  HRS ADD MNTHLY TO QTRLY             
         AP    0(DPTQBLN,R2),0(TCABKLN,RE)  AND TO TOTAL                        
         LA    RE,TCAPDACM(RE)              NEXT DEPT IN TCAPD                  
         LA    R1,DPTQLN(R1)                NEXT DEPT IN SUMMARY RECS           
         LA    R2,DPTQLN(R2)                NEXT DEPT OVERALL TOTAL             
         BCT   R0,XFT20                                                         
XFTX     J     COM2XIT                                                          
*--------------------------------------------------------------------           
         LTORG                                                                  
*--------------------------------------------------------------------           
         DROP  R1,R4,RB                                                         
         EJECT                                                                  
**********************************************************************          
* FORMAT LINE FOR RECAP REPORT                                       *          
**********************************************************************          
         SPACE 1                                                                
         USING TCAPD,R4                                                         
FRMRCAP  NTR1  BASE=*,LABEL=*                                                   
         L     R4,ATCAPWK                                                       
         LA    R3,TCAPH10          EDIT HOURS AND COST                          
         LA    R5,TCAPC10                                                       
         USING TCAPH10,R3                                                       
         USING TCAPC10,R5                                                       
         LA    R2,TCADPCNT         # OF DEPARTMENTS                             
FRMRCAP3 DS    0H                                                               
         EDIT  TCAPH10,(14,1(R6)),2,COMMAS=YES,MINUS=YES                        
         MVI   0(R7),C'$'                                                       
         EDIT  TCAPC10,(14,1(R7)),2,COMMAS=YES,MINUS=YES                        
         LA    R3,TCAPDACM(R3)                                                  
         LA    R5,TCAPDACM(R5)                                                  
         LA    R6,15(R6)                                                        
         LA    R7,15(R7)                                                        
         BCT   R2,FRMRCAP3                                                      
*                            TOTAL COL MUST BE LENGTH 13                        
         EDIT  TCAPH10,(14,1(R6)),2,COMMAS=YES,MINUS=YES                        
         MVI   0(R7),C'$'                                                       
         EDIT  TCAPC10,(14,1(R7)),2,COMMAS=YES,MINUS=YES                        
FRMRCX   J     COM2XIT                                                          
*--------------------------------------------------------------------           
         LTORG                                                                  
*--------------------------------------------------------------------           
         DROP  R3,R4,R5,RB                                                      
         EJECT                                                                  
**********************************************************************          
* COMPUTE EQUIVALENT HOURS                                           *          
**********************************************************************          
         SPACE 1                                                                
EQUIV    NTR1  BASE=*,LABEL=*                                                   
         USING TCAPD,R4                                                         
         L     R4,ARCAPTOT          USE TOTAL WORK AREA                         
         LA    R2,TCADPCNT+1        # OF DEPARTMENTS + 1 FOR TOTAL              
         LA    R3,XPSECOND+25                                                   
         LA    R5,TCAPH10           HOURS                                       
EQUIV10  ZAP   PACK8,=P'0'          ALWAYS SET PACK8 TO ZERO                    
         ZAP   PACK16,0(TCABKLN,R5) TOTAL DEPT TOYOTA HOURS                     
         BZ    EQUIV20              DON'T COMPUTE IF BUCKET IS EMPTY            
         SRP   PACK16,6,0           SHIFT 6 POSITIONS TO LEFT                   
         ZAP   PACK8,MANHOURS       DIVIDED BY 1540,770,1610 MANYR HOUR         
         DP    PACK16,PACK8  TOT BY 1540 OR 770(IF BTWN 0CT/00-MAR/01)          
         ZAP   PACK8,PACK16(L'PACK16-L'PACK8)                                   
         SRP   PACK8,64-6,5                                                     
EQUIV20  EDIT  PACK8,(5,0(R3))                                                  
         LA    R3,15(R3)                                                        
         LA    R5,TCAPDACM(R5)                                                  
         BCT   R2,EQUIV10                                                       
*                                                                               
* COMPUTE % OF TOTAL TIME                                                       
*                                                                               
         L     R4,ARCAPTOT         USE TOTAL WORK AREA                          
         LA    R2,TCADPCNT+1       # OF DEPARTMENTS + 1 FOR TOTAL               
         LA    R3,XPFOURTH+27                                                   
         LA    R5,TCAPH10          HOURS                                        
EQUIV30  ZAP   PACK8,=P'0'          ALWAYS SET PACK8 TO ZERO                    
         ZAP   PACK16,0(TCABKLN,R5) TOTAL DEPT TOYOTA HOURS                     
         BZ    EQUIV40              DON'T COMPUTE IF BUCKET IS EMPTY            
         SRP   PACK16,6,0           SHIFT 6 POSITIONS TO LEFT                   
         ZAP   PACK8,TCAPHT         DIVIDED BY TOTAL                            
         DP    PACK16,PACK8                                                     
         ZAP   PACK8,PACK16(L'PACK16-L'PACK8)                                   
         SRP   PACK8,64-3,5                                                     
EQUIV40  EDIT  PACK8,(6,0(R3)),1,TRAIL=C'%'                                     
         LA    R3,15(R3)                                                        
         LA    R5,TCAPDACM(R5)                                                  
         BCT   R2,EQUIV30                                                       
EQUIVX   J     COM2XIT                                                          
*--------------------------------------------------------------------           
         LTORG                                                                  
*--------------------------------------------------------------------           
         DROP  R4,RB                                                            
         EJECT                                                                  
**********************************************************************          
* FORMAT LINE FOR DETAIL REPORT (HOURS)                              *          
**********************************************************************          
         SPACE 1                                                                
FRMH     NTR1  BASE=*,LABEL=*                                                   
         USING TCQD,R4                                                          
         L     R4,ATCQWK                                                        
         LA    R6,XP+40                                                         
         LA    R3,TCQTR1           QUARTERLY HOURS                              
         USING TCQTR1,R3                                                        
         LA    R2,4                4 QUARTERS                                   
FRMH10   DS    0H                                                               
         EDIT  TCQTR1,(11,0(R6)),2,MINUS=YES                                    
         LA    R3,TCQBKLN(R3)                                                   
         LA    R6,15(R6)                                                        
         BCT   R2,FRMH10                                                        
         DROP  R3                                                               
         LA    R6,XP+115                                                        
         USING TCQD,R4                                                          
         L     R4,ATCQWK                                                        
*                                  TOTAL HOURS                                  
         EDIT  TCTOT,(11,0(R6)),2,MINUS=YES                                     
*                                                                               
         CLI   TCQPRSN,X'FE'       NO PERCENTAGES FOR TOTAL LINES               
         BNL   FRMHX                                                            
         CLI   TCQSUB,X'FE'                                                     
         BNL   FRMHX                                                            
         CP    TCTIME,ZEROS                                                     
         BZ    FRMHX               NO PERCENTAGE OF TIME                        
         LA    R6,XP+107                                                        
         ZAP   PACK8,TCTIME                                                     
         SRP   PACK8,64-2,5                                                     
         CP    PACK8,=P'100'                                                    
         BNH   *+10                                                             
         ZAP   PACK8,=P'100'       ONLY GO UP TO 100%                           
         EDIT  PACK8,(4,0(R6)),TRAIL=C'%'                                       
FRMHX    J     COM2XIT                                                          
*--------------------------------------------------------------------           
         LTORG                                                                  
*--------------------------------------------------------------------           
         DROP  R4,RB                                                            
         EJECT                                                                  
**********************************************************************          
* FORMAT LINE FOR DETAIL REPORT (COST)                               *          
**********************************************************************          
         SPACE 1                                                                
FRMC     NTR1  BASE=*,LABEL=*                                                   
         USING TCQD,R4                                                          
         L     R4,ATCQWK                                                        
*                                                                               
         TM    FLAG,FLGGRP         ARE WE RUNNING GROUP REPORT?                 
         BO    FRMC30                                                           
*                                                                               
         USING DPTAD,R3                                                         
         L     R3,ADPTTOTS         DEPT TOTALS                                  
         USING DPTD,RF                                                          
         L     RF,DPTBUCK          READ FH BUCKETS TABLE                        
         TM    FLAG2,FLGFH         ARE WE READING FH SUPERLEDGER                
         BO    *+8                                                              
         L     RF,DPTBKFR          READ FR BUCKETS TABLE                        
FRMC10   CLI   DPTNUMB,EOT                                                      
         BNE   *+6                                                              
         DC    H'0'                CAN'T MATCH DEPARTMENT                       
         CLC   DPTNUMB,TCQDEPT                                                  
         BE    *+12                                                             
         LA    RF,DPTDLN(RF)                                                    
         B     FRMC10                                                           
         AH    R3,DPTTDISP         POINTS TO DEPT COST ACCUM                    
         DROP  RF                                                               
*                                                                               
         LA    R7,XP+38                                                         
         LA    R2,4                4 QUARTERS                                   
FRMC20   DS    0H                                                               
         EDIT  DPTACCUM,(13,0(R7)),2,FLOAT=$,MINUS=YES                          
         LA    R3,DPTQBLN(R3)                                                   
         LA    R7,15(R7)                                                        
         BCT   R2,FRMC20                                                        
         LA    R7,XP+113           TOTAL                                        
         EDIT  DPTACCUM,(13,0(R7)),2,FLOAT=$,MINUS=YES                          
         B     FRMCX                                                            
*                                                                               
FRMC30   LA    R6,XP+40                                                         
         LA    R3,TCQTR1           QUARTERLY COSTS                              
         USING TCQTR1,R3                                                        
         LA    R2,4                4 QUARTERS                                   
FRMC40   DS    0H                                                               
         EDIT  TCQTR1,(11,0(R6)),2,FLOAT=$,MINUS=YES                            
         LA    R3,TCQBKLN(R3)                                                   
         LA    R6,15(R6)                                                        
         BCT   R2,FRMC40                                                        
         DROP  R3                                                               
*                                                                               
         LA    R6,XP+115                                                        
         USING TCQD,R4                                                          
         L     R4,ATCQWK                                                        
*                                                                               
         EDIT  TCTOT,(11,0(R6)),2,MINUS=YES  TOTAL COST                         
*                                                                               
FRMCX    J     COM2XIT                                                          
*--------------------------------------------------------------------           
         LTORG                                                                  
*--------------------------------------------------------------------           
         DROP  R4,RB                                                            
         EJECT                                                                  
**********************************************************************          
* REMAINING SPACE                                                    *          
**********************************************************************          
         SPACE 1                                                                
ABILITY2 EQU   (4095*3)-(*-VCOMMON2)  REMAINING ADDRESSIBILITY                  
         DROP  R9                     FREE UP ALL USINGS                        
*        DROP  RB,R9                  FREE UP ALL USINGS                        
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* CHECK TO SEE IF AMOUNTS WERE ADJUSTED                              *          
*       R5 = A(SORT RECORD)                                          *          
**********************************************************************          
         SPACE 1                                                                
         USING BUFD,R4                                                          
         USING SRTD,R5                                                          
         USING SDTD,R6                                                          
PSTADJ   NMOD1 0,**PSTA**                                                       
         L     RC,0(R1)                                                         
         LA    R3,SVSRTDTL         PRE ADJUSTED TOTALS                          
         L     R4,ABUFWRK                                                       
         LA    R6,SRDTL                                                         
         LA    R2,MAXMTHS+1        ALL MONTHS PLUS TOTAL                        
         MVC   SVDPT,XSPACES                                                    
*                                                                               
         TM    FLAG2,FLGNEG        DID SALARY GET ADJUSTED BACKWARDS?           
         BO    *+12                                                             
         TM    SRSTAT,SRHRS        DID PERSON REACH 1800 HRS (CLI TIME)         
         BNO   PSTAX               NO - EXIT                                    
*                                                                               
         LA    RE,SVSRTTOT         TOTAL DETAIL ACCUMS BEFORE ADJUST            
         LA    RF,SRTOTAL          SORTED ACCUMS AFTER ADJUST                   
PSTA10   CP    SDSAL-SDBUK(SDBUKLN,RE),SDSAL-SDTD(SDBUKLN,RF)                   
         BNE   PSTA20                                                           
         CP    SDPST-SDBUK(SDBUKLN,RE),SDPST-SDTD(SDBUKLN,RF)                   
         BNE   PSTA20                                                           
         CP    SDTOTAL-SDBUK(SDBUKLN,RE),SDTOTAL-SDTD(SDBUKLN,RF)               
         BE    PSTAX               NO CHANGE - EXIT                             
*                                                                               
PSTA20   NI    FLAG,X'FF'-FLGADJ   TURN OFF ADJUSTED FLAG                       
         ZAP   SDYPCT,SDYPCT-SDBUK(SDBUKLN,R3)   PCT MAY CHG SO LOSE IT         
         ZAP   SDHRRAT,SDHRRAT-SDBUK(SDBUKLN,R3) SAME WITH HOURLY RATE          
         LA    R0,SDNUMBK          NUMBER OF ACCUMS TO ADD                      
         LA    RE,SDSAL            1ST SORT BUCKET                              
         LR    RF,R3                                                            
PSTA30   SP    0(SDBUKLN,RE),0(SDBUKLN,RF)                                      
         BZ    *+8                                                              
         OI    FLAG,FLGADJ         MARK AS ADJUSTED                             
         LA    RE,SDBUKLN(RE)                                                   
         LA    RF,SDBUKLN(RF)                                                   
         BCT   R0,PSTA30                                                        
*                                                                               
         TM    FLAG,FLGADJ         WAS ANYTHING ADJUSTED?                       
         BNO   PSTA100             NO CHANGE - EXIT                             
         OI    SRSTAT,SRADJ        MARK PERSON AS ADJUSTED                      
*                                                                               
         USING TCAPD,R4                                                         
         L     R4,ATCAPWK                                                       
         MVC   TCAPKEY(TCAKLEN),XSPACES  CLEAR RECAP AREA                       
         LA    R1,TCABK                                                         
         LA    R0,TCACNT                                                        
         ZAP   0(TCABKLN,R1),ZEROS                                              
         LA    R1,TCABKLN(R1)                                                   
         BCT   R0,*-10                                                          
         DROP  R4                                                               
*                                                                               
         USING TCQD,R4                                                          
         L     R4,ATCQWK           CLEAR COST WORK AREA                         
         MVC   TCQKEY(TCQKLEN),XSPACES                                          
         LA    R1,TCQBK                                                         
         LA    R0,TCQCNT                                                        
         ZAP   0(TCQBKLN,R1),ZEROS                                              
         LA    R1,TCQBKLN(R1)                                                   
         BCT   R0,*-10                                                          
         DROP  R4                                                               
*                                                                               
         USING ADJD,R1                                                          
         LA    R1,ADJTAB           MONTHLY DETAIL                               
         LA    R0,MAXMTHS                                                       
PSTA40   CLI   SDMNTH,X'FF'                                                     
         BE    PSTA80                                                           
         OC    ADJLOC,ADJLOC       ANYMORE DATA?                                
         BZ    PSTA50                                                           
         CLC   SDMNTH,ADJMNTH      MATCH ON MONTH                               
         BNE   *+14                                                             
         MVC   SRACC(L'ADJLOC),ADJLOC  PUT IN MONTHS LOCATION                   
         B     *+16                                                             
PSTA50   LA    R1,ADJLNQ(R1)                                                    
         BCT   R0,PSTA40                                                        
         B     PSTA100             GET NEXT MONTH                               
         DROP  R1                                                               
*                                                                               
         TM    FLAG2,FLGNEG        DID SALARY GET ADJUSTED BACKWARDS?           
         BO    PSTA60              DONT BOTHER CHECKING DEPARTMENTS             
         CLC   SVDPT,XSPACES                                                    
         BE    *+16                                                             
         CLC   SRDEPT,SVDPT                                                     
         BE    *+6                                                              
         DC    H'0'                DIE IF MULTIPLE DEPT ADJUSTMENT              
PSTA60   MVC   SVDPT,SRDEPT        SAVE OFF ADJUSTED DEPARTMENT                 
*                                                                               
         CLI   QOPT6,C'S'                                                       
         BE    PSTA80              NO RECAP REPORT                              
*                                                                               
         USING TCAPD,R4                                                         
         L     R4,ATCAPWK                                                       
         MVC   SVKEY,XSPACES                                                    
         MVI   TCAPTYP,TCAPEQU     RECORD TYPE                                  
         MVC   SVKEY(TCAKLEN),TCAPKEY                                           
         MVC   TCAPKEY,SVKEY       RESTORE KEY                                  
         CLI   SDMNTH,X'FF'                                                     
         BE    PSTA80              GET NEXT MONTH                               
         MVC   TCAPYYMM,SDMNTH     YEAR/MONTH                                   
         LA    R1,TCABK                                                         
         LA    R0,TCACNT                                                        
         ZAP   0(TCABKLN,R1),ZEROS                                              
         LA    R1,TCABKLN(R1)                                                   
         BCT   R0,*-10                                                          
*                                                                               
         AP    TCAPHT,SDPRDHR      ADD MNTHLY HRS AND COST TO TOTAL             
         AP    TCAPCT,SDTOTAL                                                   
*                                                                               
         L     RF,DPTBUCK          READ FH BUCKETS TABLE                        
         TM    FLAG2,FLGFH         ARE WE READING FH SUPERLEDGER                
         BO    *+8                                                              
         L     RF,DPTBKFR          READ FR BUCKETS TABLE                        
         USING DPTD,RF                                                          
PSTA70   CLI   DPTNUMB,EOT                                                      
         BNE   *+6                                                              
         DC    H'0'                CAN'T MATCH DEPARTMENT                       
         CLC   DPTNUMB,SRDEPT                                                   
         BE    *+12                                                             
         LA    RF,DPTDLN(RF)                                                    
         B     PSTA70                                                           
*                                                                               
         LR    RE,R4               RECORD ADDRESS                               
         LR    R1,R4                                                            
         AH    R1,DPTHDISP         POINTS TO DEPT HOURS ACCUM                   
         AH    RE,DPTCDISP         POINTS TO DEPT COST ACCUM                    
         AP    0(TCABKLN,R1),SDPRDHR                                            
         AP    0(TCABKLN,RE),SDTOTAL                                            
         DROP  RF                                                               
*                                                                               
         GOTO1 BUFFALO,DMCB,(L'PUTC,PUTC),ADBUFF,(R4)                           
         MVC   TCAPYYMM,=X'FFFF'                                                
         BASR  RE,RF                                                            
         DROP  R4                                                               
*                                                                               
* POST TO QUARTER REPORT                                                        
*                                                                               
PSTA80   DS    0H                                                               
         CLI   QOPT8,C'S'                                                       
         BE    PSTA100             NO TIME COST REPORT                          
         CLI   SDMNTH,X'FF'                                                     
         BE    PSTA100             NO TOTAL SECTION                             
*                                                                               
         USING TCQD,R4                                                          
         L     R4,ATCQWK           CLEAR COST WORK AREA                         
         MVC   TCQKEY,XSPACES                                                   
         MVI   TCQTYP,TCQEQU       RECORD TYPE                                  
         MVC   TCQDEPT,SRDEPT      APG DEPT                                     
         MVC   TCQTITL,SRTITLE     APG SUBDEPT                                  
*                                                                               
         OC    SDMNTH,SDMNTH                                                    
         BZ    PSTA100             GET NEXT MONTH                               
*                                                                               
         L     RE,QTRMTAB          QTRS TABLE                                   
         USING QTRD,RE                                                          
PSTA90   CLI   QTRMTH,EOT                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   SDMNTH+1(1),QTRMTH  WHAT QTR DOES MONTH BELONG TO?               
         BE    *+12                                                             
         LA    RE,QTRDLN(RE)                                                    
         B     PSTA90                                                           
*                                                                               
         SR    RF,RF                                                            
         IC    RF,QTRNUMB                                                       
         BCTR  RF,0                                                             
         MH    RF,=Y(TCQBKLN)                                                   
         STH   RF,HALF                                                          
         DROP  RE                                                               
*                                                                               
         LA    R1,TCQBK                                                         
         AH    R1,HALF             POINTS TO QTR ACCUM                          
         AP    0(TCQBKLN,R1),SDTOTAL                                            
         AP    TCTOT,SDTOTAL          ADD TO TOTAL                              
*                                                                               
         MVC   TCQDEPT(3),=X'FF0000'                                            
         GOTO1 BUFFALO,DMCB,(L'PUTC,PUTC),ADBUFF,ATCQWK                         
         XC    TCQPRSN,TCQPRSN     COST TOTALS                                  
         XC    TCQSUB,TCQSUB                                                    
         MVC   TCQSUB(2),=X'FF00'  DEPT TOTALS FOR COST                         
         MVC   TCQTITL,=X'FFFF'    DEPT TOTALS FOR COST                         
         BASR  RE,RF               COST TOTALS                                  
*                                                                               
PSTA100  LA    R6,SDLEQ(R6)        NEXT MONTH                                   
         LA    R3,SDDTLQ(R3)                                                    
         BCT   R2,PSTA20                                                        
*                                                                               
         TM    SRSTAT,SRADJ        WAS PERSON ADJUSTED?                         
         BNO   PSTAX               NO - NOTHING WAS DONE                        
*                                                                               
*        TM    FLAG2,FLGNEG        DID SALARY GET ADJUSTED BACKWARDS?           
*        BO    PSTA110             DONT BOTHER ADDING TO GRP TABLE              
         GOTO1 =A(ADDPHR),DMCB,(RC)  MARK BINTABLE AS ADJUSTED                  
*                                                                               
         USING BUFD,R4                                                          
PSTA110  L     R4,ABUFWRK                                                       
         MVC   BUFKEY,SVBUFKEY     RESET READS                                  
         GOTO1 BUFFALO,DMCB,(L'HIGHC,HIGHC),ADBUFF,(R4),0                       
*                                                                               
         GOTO1 =A(TOTIT),DMCB,(RC)   ADD TO DEPARTMENT/REQ TOTALS               
*                                                                               
         BAS   RE,ADDTOT            PROCESS AS DETAIL RECORD                    
         GOTO1 CLRSORT,DMCB,ADPTREC CLEAR DEPT RECORD                           
         L     R5,ASRTREC          SET R5 BACK TO PERSON SORT REC               
*                                                                               
PSTAX    XIT1                                                                   
         DROP  R4,R5,R6                                                         
         EJECT                                                                  
**********************************************************************          
* ADD NEW DEPT TOTALS TO SAVED TOTALS                                *          
**********************************************************************          
         SPACE 1                                                                
         USING SRTD,R5                                                          
         USING SDTD,R6                                                          
ADDTOT   NTR1                                                                   
         L     R5,ADPTREC          THE DEPT TOTAL RECORD                        
         LA    R6,SRDTL            MONTHLY DETAIL                               
*                                                                               
         CLC   SVDPT,XSPACES       ANY DEPARTMENT?                              
         BE    ADDTX                                                            
*                                                                               
         L     R3,ADUPREC          SAVED DEPT TOTALS FROM FIRST REPORT          
ADDT10   OC    SRDEPT-SRTD(L'SRDEPT,R3),SRDEPT-SRTD(R3)                         
         BNZ   *+6                                                              
         DC    H'0'                DEPT MUST BE IN TABLE                        
         CLC   SVDPT,SRDEPT-SRTD(R3)                                            
         BE    *+12                                                             
         LA    R3,SRTLEN(R3)                                                    
         B     ADDT10                                                           
*                                                                               
         LA    R2,MAXMTHS          INCLUDE TOTAL RECORD                         
         LA    R3,SRDTL-SRTD(R3)   BUMP R3 TO DETAIL LINE                       
ADDT20   LA    RE,SDSAL            1ST BUCKET                                   
         LA    RF,SDSAL-SDTD(R3)                                                
         LA    R0,SDNUMBK          NUMBER OF BUCKETS                            
         AP    0(SDBUKLN,RF),0(SDBUKLN,RE)                                      
         LA    RE,SDBUKLN(RE)                                                   
         LA    RF,SDBUKLN(RF)                                                   
         BCT   R0,*-14                                                          
*                                                                               
         LA    RF,SRTOTAL          UPDATE TOTAL LINE                            
         L     R4,SUMPST                                                        
         USING SUMPD,R4                                                         
ADDT30   LR    R1,RF                                                            
         LR    RE,R6               DETAIL LINE                                  
         CLI   SUMPDISP,EOT                                                     
         BE    ADDT40                                                           
         AH    R1,SUMPDISP                  POINTS TO SORT ACCUMS               
         AH    RE,SUMPDISP                                                      
         TM    SUMPSTAT,SUMPADD                                                 
         BZ    *+14                                                             
         AP    0(SDBUKLN,R1),0(SDBUKLN,RE) ADD OR ZAP PER TABLE STAT            
         B     *+10                                                             
         ZAP   0(SDBUKLN,R1),0(SDBUKLN,RE)                                      
         LA    R4,SUMPLN(R4)                                                    
         B     ADDT30                                                           
*                                                                               
ADDT40   LA    R6,SDLEQ(R6)                                                     
         LA    R3,SDLEQ(R3)                                                     
         BCT   R2,ADDT20                                                        
*                                                                               
ADDTX    XIT1                                                                   
         DROP  R5,R6                                                            
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* TOTAL UP TO DEPT /REQUEST                                          *          
**********************************************************************          
         SPACE 1                                                                
TOTIT    DS    0D                                                               
         NMOD1 0,**TOTI**                                                       
         L     RC,0(R1)                                                         
         USING SRTD,R5                                                          
DETLINE  USING SDTD,R6                                                          
TOTLINE  USING SDTD,R4                                                          
         L     R5,ASRTREC          SORT RECORD                                  
         LA    R6,SRDTL            MONTHLY DETAIL                               
         L     R5,ADPTREC          DEPT TOTAL RECORD                            
         LA    R4,SRDTL            DEPT DETAIL                                  
         BAS   RE,TOT20                                                         
*                                                                               
         TM    FLAG,FLGGRP         ONLY NEED FOR GROUP REPORT                   
         BNO   TOT10                                                            
         L     R5,ASRTREC          SORT RECORD                                  
         CLC   SRDEPT,=C'00'       UNKNOWN                                      
         BE    TOTITX              DON'T ADD UNKNOWN TO REQ TOTAL               
         LA    R6,SRDTL            MONTHLY DETAIL                               
         L     R5,AGRPREC          GROUP TOTAL RECORD                           
         LA    R4,SRDTL            GROUP DETAIL                                 
         BAS   RE,TOT20                                                         
*                                                                               
TOT10    TM    FLAG,FLGLDA         DON'T ADD TO REQUEST TO FOR LDA              
         BO    TOTITX                                                           
         L     R5,ASRTREC          SORT RECORD                                  
         CLC   SRDEPT,=C'00'       UNKNOWN                                      
         BE    TOTITX              DON'T ADD UNKNOWN TO REQ TOTAL               
         LA    R6,SRDTL            MONTHLY DETAIL                               
         L     R5,AREQREC          REQ TOTAL RECORD                             
         LA    R4,SRDTL            REQ DETAIL                                   
         BAS   RE,TOT20                                                         
         B     TOTITX                                                           
*                                                                               
TOT20    ST    RE,SAVERE                                                        
         LA    R0,MAXMTHS          ALL MONTHS                                   
TOT30    LA    RE,DETLINE.SDBUK    DETAIL ACCUMS                                
         LA    RF,TOTLINE.SDBUK    TOTAL ACCUMS                                 
         LA    R1,SDNUMBK          NUMBER OF BUCKETS                            
TOT40    AP    0(SDBUKLN,RF),0(SDBUKLN,RE)                                      
         LA    RF,SDBUKLN(RF)                                                   
         LA    RE,SDBUKLN(RE)                                                   
         BCT   R1,TOT40                                                         
         ZAP   TOTLINE.SDYPCT,ZEROS CLEAR PCTS AND HR RATES                     
         ZAP   TOTLINE.SDHRRAT,ZEROS                                            
         LA    R4,SDLEQ(R4)                                                     
         LA    R6,SDLEQ(R6)                                                     
         BCT   R0,TOT30                                                         
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
TOTITX   XIT1                                                                   
         DROP  R5                                                               
         DROP  DETLINE,TOTLINE                                                  
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* DOWNLOAD ROUTINE                                                   *          
*          R7 = A(PRINT LINE)                                        *          
**********************************************************************          
         SPACE 1                                                                
         USING PLINED,R7                                                        
DWNRTE   DS    0D                                                               
         NMOD1 0,**DWNR**                                                       
         L     RC,0(R1)                                                         
         LA    R7,SVPRNT                                                        
         MVI   FORCEHED,C'Y'                                                    
         LA    RE,XP                                                            
         LA    RF,SVPRNLNQ                                                      
         SR    R1,R1                                                            
         ICM   R1,8,=X'40'                                                      
         MVCL  RE,R0                                                            
         MVC   SVSUBPRG,RCSUBPRG   SAVE OFF REPORT NUMBER                       
*                                                                               
         LHI   R0,4                FOUR PRINT LEVELS                            
DWNR10   STC   R0,SVCNT                                                         
         CLC   0(10,R7),=CL10' '   ARE THEY SKIPPING LINE                       
         BE    DWNR360                                                          
         CLI   5(R7),X'BF'         IS IT UNDERLINING                            
         BE    DWNR360             YES GET NEXT LINE                            
         MVC   SVSUBPRG,RCSUBPRG   SAVE OFF REPORT NUMBER                       
         MVI   RCSUBPRG,9          INVALID REPORT NUM FOR DOWNLOAD              
         CLI   SVSUBPRG,6          IF WE ARE DOING RECAP REPORT                 
         BNE   DWNR20                 CHECK IF TOTALING                         
         CLC   4(2,R7),=C'12'      IF DOING 12 MONTH TOTAL DON'T DWNLD          
         BE    DWNR60                 REPORT NUMBER YET                         
         CLI   5(R7),X'BF'         IS IT UNDERLINING                            
         BE    DWNR60                                                           
*                                                                               
* ALWAYS DOWNLOAD REPORT NUMBER FIRST                                           
*                                                                               
DWNR20   LA    R1,RPTTAB                                                        
DWNR30   CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                HAS TO BE THERE                              
         CLC   SVSUBPRG,0(R1)                                                   
         BE    *+12                                                             
DWNR40   LA    R1,L'RPTTAB(R1)                                                  
         B     DWNR30                                                           
*                                                                               
         CLI   SVSUBPRG,6          CHECK IF IT'S A GROUP REPORT FOR             
         BE    *+12                   BOTH FORMAT 6 AND 7                       
         CLI   SVSUBPRG,7                                                       
         BNE   DWNR50                                                           
*                                                                               
         MVI   MASK,EQUAL          EQUAL                                        
         TM    FLAG,FLGGRP         GROUP REPORT?                                
         BNO   *+8                                                              
         MVI   MASK,NOTEQUAL       NOT EQUAL                                    
         MVC   COND+1(1),MASK                                                   
*                                                                               
         CLI   2(R1),FLGGRP                                                     
COND     BC    0,DWNR40                                                         
*                                                                               
DWNR50   CLI   1(R1),C'0'                                                       
         BE    *+10                                                             
         MVC   SVRPT,1(R1)         1(R1) IS THE REPORT NUMBER                   
         OC    SVRPT,SVRPT         IF NO REPORT WAS DONE THEN                   
         BZ    DWNXIT              DON'T DO JUST THE TOTALS                     
*                                                                               
         MVI   PRTSIZE,0                  SET COLUMN LEN=0-NO PADDING           
         MVC   DWNFLD,XSPACES             SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'SVRPT),SVRPT      MOVE RPT # TO DWN FLD                 
         LA    R1,L'SVRPT                 RPT # LENGTH                          
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
DWNR60   CLI   SVSUBPRG,7          ARE WE DOING ANALYSIS                        
         BE    DWNR140             GO DO ANALYSIS AND GROUP                     
         CLI   SVSUBPRG,6          ARE WE DOING RECAP                           
         BE    DWNR300             GO DO RECAP AND GROUP                        
*                                                                               
* ALWAYS DOWNLOAD ALPHAID (AGENCY LEVEL REPORTING)                              
*                                                                               
         MVI   PRTSIZE,0                  SET COLUMN LEN=0-NO PADDING           
         MVC   DWNFLD,XSPACES             SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'ALPHAID),ALPHAID  MOVE ID TO DWN FLD                    
         LA    R1,L'ALPHAID               ID'S LENGTH                           
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
         LA    RE,LEVTABP          ASSUME PERSON REPORT                         
         TM    FLAG,FLGGRP         ARE WE DOING GROUP?                          
         BNO   *+8                                                              
         LA    RE,LEVTABPG         POINT TO PERSON GROUP REPORT                 
*                                                                               
DWNR70   CLI   0(RE),X'FF'         FIND WHAT REPORT IS DOWNLOADING              
         BE    DWNXIT                                                           
         CLC   SVSUBPRG,0(RE)      MATCH ON REPORT                              
         BE    *+12                                                             
         LA    RE,L'LEVTABP(RE)                                                 
         B     DWNR70                                                           
*                                                                               
         SR    R1,R1                                                            
         IC    R1,1(RE)            SAVE DISPL TO CODE IN CODE TAB               
*                                                                               
         USING CODAD,R5                                                         
         LA    R5,CODADRP          ASSUME PERSON REPORT                         
         LHI   R0,CODNUMP          # OF LEVS TO LOOP-PER RPT                    
         TM    FLAG,FLGGRP         ARE WE DOING GROUP?                          
         BNO   *+12                                                             
         LHI   R0,CODNUMPG         # OF LEVS TO LOOP -PER/GRP RPT               
         LA    R5,CODADRPG         POINT TO PERSON GROUP REPORT                 
         STC   R0,LEVCNT           SAVE NUMBER OF LEVELS                        
*                                                                               
         AR    R5,R1               BUMP TO APPROPRIATE FIELD                    
*                                                                               
         SR    R0,R0                                                            
DWNR80   CLI   0(R5),X'FF'                END OF TABLE?                         
         BE    DWNR90                                                           
         AHI   R0,1                INCREMENT LOOP CONTER                        
         SR    R1,R1                                                            
         ICM   R1,3,CODDSP                GET THE DISPL TO FIELD                
         LA    R2,LOCAL                   R2=A(BEGINNING OF STORAGE)            
         AR    R2,R1                                                            
         MVI   PRTSIZE,0                  SET COLUMN LEN=0-NO PADDING           
         MVC   DWNFLD,XSPACES             SPACE OUT DOWNLOAD FIELD              
         SR    R1,R1                                                            
         IC    R1,CODLEN                                                        
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   DWNFLD(0),0(R2)            MOVE FIELD TO DWN FLD                 
         MVC   PRTSIZE,CODLEN                                                   
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
         LA    R5,CODLNQ(R5)                                                    
         B     DWNR80                                                           
         DROP  R5                                                               
*                                                                               
*        TM    DWNSTAT,DWNHDLN     WERE THE HEADLINES DOWNLOADED?               
*        BO    *+8                                                              
*        BAS   RE,DWNHEAD          DOWNLOAD HEADLINES                           
*                                                                               
DWNR90   DS    0H                  DOWNLOAD ADDITIONAL LEVELS                   
         SR    R1,R1                  AS EMPTY FIELDS                           
         IC    R1,LEVCNT                                                        
         SR    R1,R0                                                            
         BNP   DWNR110                                                          
*                                                                               
         LR    R0,R1                                                            
DWNR100  MVI   PRTSIZE,0                  SET COLUMN LEN=0-NO PADDING           
         MVC   DWNFLD,XSPACES             SPACE OUT DOWNLOAD FIELD              
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
         BCT   R0,DWNR100                                                       
*                                                                               
DWNR110  MVI   PRTSIZE,0                  SET COLUMN LEN=0-NO PADDING           
         MVC   DWNFLD,XSPACES             SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'PMNTH),PMNTH      MOVE MONTH TO DWN FLD                 
         LA    R1,L'PMNTH                 MONTH'S LENGTH                        
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
         MVC   DWNFLD,XSPACES             SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'PMSTAT),PMSTAT    MOVE ADJ STATUS TO DWN FLD            
         LA    R1,L'PMSTAT                LENGTH PMSTAT                         
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
         MVI   PRTSIZE,0                  RESET LENGTH=0-NO PADDING             
*                                                                               
         MVC   DWNFLD,XSPACES             SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'PSAL),PSAL        MOVE SALARY TO DWN FLD                
         LA    R1,L'PSAL                  LENGTH SALARY FIELD                   
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNNUM     DOWNLOAD NUMBER                       
*                                                                               
         MVC   DWNFLD,XSPACES             SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'PPHRS),PPHRS      MOVE PRODUCT HRS TO DWN FLD           
         LA    R1,L'PPHRS                 PAD DIVISION NAME COLUMN              
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNNUM     DOWNLOAD NUMBER                       
         MVI   PRTSIZE,0                  RESET LENGTH=0-NO PADDING             
*                                                                               
         MVC   DWNFLD,XSPACES             SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'PTHRS),PTHRS      MOVE TOTAL HRS TO DWN FLD             
         LA    R1,L'PTHRS                 LENGTH TOTAL HOURS                    
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNNUM     DOWNLOAD NUMBER                       
*                                                                               
         MVC   DWNFLD,XSPACES             SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'PCHRS),PCHRS      MOVE CAT NAME TO DWN FLD              
         LA    R1,L'PCHRS                 LENGTH CLIENT HOURS                   
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNNUM     DOWNLOAD NUMBER                       
         MVI   PRTSIZE,0                  RESET LENGTH=0-NO PADDING             
*                                                                               
         MVC   DWNFLD,XSPACES             SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'PYPHRS),PYPHRS    MOVE YTD PRD HRS TO DWN FLD           
         LA    R1,L'PYPHRS                YTD PROD HRS FIELD LENGTH             
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNNUM     DOWNLOAD NUMBER                       
*                                                                               
         TM    FLAG,FLGGRP         ARE WE DOWNLOADING GROUP REPORT              
         BO    DWNR120                                                          
*                                                                               
         MVC   DWNFLD,XSPACES             SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'PYPCT),PYPCT      MOVE YTD ACTL PCT TO DWN FLD          
         LA    R1,L'PYPCT                 YTD ACTL PCT FIELD LENGTH             
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
         MVC   DWNFLD,XSPACES             SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'PYRATE),PYRATE    MOVE HOURLY RATE TO DWN FLD           
         LA    R1,L'PYRATE                HOURLY RATE FIELD LENGTH              
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNNUM     DOWNLOAD NUMBER                       
         B     DWNR130                                                          
*                                                                               
DWNR120  DS    0H                                                               
         MVC   DWNFLD,XSPACES             SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'PYTPHR),PYTPHR    YTD TOT PRD HRS FOR GRP RECDS         
         LA    R1,L'PYTPHR                FIELD LENGTH                          
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNNUM     DOWNLOAD NUMBER                       
*                                                                               
         MVC   DWNFLD,XSPACES             SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'PYGRPPCT),PYGRPPCT ACTUAL PCT FOR GRP REP               
         LA    R1,L'PYGRPPCT              FIELD LENGTH                          
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    PCT IS TEXT                           
*                                                                               
DWNR130  DS    0H                                                               
*                                                                               
         MVC   DWNFLD,XSPACES             SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'PYCOST),PYCOST    SALARY COST TO DWNLD FLD              
         LA    R1,L'PYCOST                FIELD LENGTH                          
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNNUM     DOWNLOAD NUMBER                       
*                                                                               
         MVC   DWNFLD,XSPACES             SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'PMCOST),PMCOST    MONTHLY COST TO DWNLD FLD             
         LA    R1,L'PMCOST                FIELD LENGTH                          
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNNUM     DOWNLOAD NUMBER                       
*                                                                               
         MVC   DWNFLD,XSPACES             SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'PFIN),PFIN        FINAL CHARGE TO DWNLD FLD             
         LA    R1,L'PFIN                  FIELD LENGTH                          
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNNUM     DOWNLOAD NUMBER                       
         B     DWNR350                    GO DWNLD EOL                          
*                                                                               
* START ANALYSIS OR ANALYSIS GROUP DOWNLOAD                                     
*                                                                               
DWNR140  DS    0H                                                               
         USING TCQD,R4                                                          
         L     R4,ATCQWK                CLEAR HOURS WORK AREA                   
*                                                                               
*        ALWAYS DOWNLOAD DPT-CODE,DPT-NAME,TITLE                                
*                                                                               
         MVC   DWNFLD,XSPACES             SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'SVGRP),SVGRP      MOVE GRP CODE TO DWN FLD              
         LA    R1,L'SVGRP                 GRP CODE'S LENGTH                     
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
         MVC   DWNFLD,XSPACES             SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'SVGRPNM),SVGRPNM  MOVE GRP NAME TO DWN FLD              
         LA    R1,L'SVGRPNM               GRP NAME'S LENGTH                     
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
         MVC   DWNFLD,XSPACES             SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'SVDPT),SVDPT      MOVE DPT CODE TO DWN FLD              
         LA    R1,L'SVDPT                 DPT CODE'S LENGTH                     
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
         MVC   DWNFLD,XSPACES             SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'SVDPTNME),SVDPTNME  MOVE DPT NAME TO DWN FLD            
         LA    R1,L'SVDPTNME              DPT NAME'S LENGTH                     
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
         OC    SVAPGDPT,SVAPGDPT   ARE WE DONE WITH THE TOTALS OF DPT           
         BNZ   *+16                                                             
         MVC   SVDPT,XSPACES       DO NOT CARRY DPT CODE TO NEXT GROUP          
         MVC   SVDPTNME,XSPACES    DO NOT CARRY DPT NAME TO NEXT GROUP          
*                                                                               
         MVC   DWNFLD,XSPACES             SPACE OUT DOWNLOAD FIELD              
         OC    SVSUBNME,SVSUBNME                                                
         BZ    *+10                                                             
         MVC   DWNFLD(L'SVSUBNME),SVSUBNME MOVE TITLE    TO DWN FLD             
         LA    R1,L'SVSUBNME              TITLE'S LENGTH                        
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
         OC    SVAPGSUB,SVAPGSUB   ARE WE DONE WITH THE TOTALS OF SUB           
         BNZ   *+10                                                             
         MVC   SVSUBNME,XSPACES    DO NOT CARRY SUB NAME TO NEXT GROUP          
*                                                                               
         MVC   DWNFLD,XSPACES             SPACE OUT DOWNLOAD FIELD              
         CLC   5(10,R7),=C'TOTALS FOR'    IF TOTAL LINE DWN TOTALS              
         BNE   *+14                                                             
         MVC   DWNFLD(6),=C'TOTALS'                                             
         B     *+10                                                             
         MVC   DWNFLD(L'SVPERSON),SVPERSON MOVE PERSON TO DWN FLD               
         LA    R1,L'SVPERSON              PERSON  LENGTH                        
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
         MVC   DWNFLD,XSPACES              SPACE OUT DOWNLOAD FIELD             
         LA    R1,L'PANQTR1                                                     
         LA    R2,PANQTR1                                                       
DWNR150  CLI   0(R2),C' '                                                       
         BE    *+12                                                             
         CLI   0(R2),C'$'                                                       
         BNE   DWNR160                                                          
         LA    R2,1(R2)                                                         
         BCT   R1,DWNR150                                                       
         MVI   DWNFLD,C'0'         NO SIGNIF CHAR SEND ZERO                     
         MVI   PRTSIZE,1                                                        
         B     DWNR170                                                          
*                                                                               
DWNR160  STC   R1,PRTSIZE                                                       
         SHI   R1,1                                                             
         EX    R1,*+4                                                           
         MVC   DWNFLD(0),0(R2)             MOVE QTR1     TO DWN FLD             
DWNR170  GOTO1 ADWNL,DMCB,(RC),DWNNUM      DOWNLOAD TEXT                        
*                                                                               
         MVC   DWNFLD,XSPACES              SPACE OUT DOWNLOAD FIELD             
         LA    R1,L'PANQTR2                                                     
         LA    R2,PANQTR2                                                       
DWNR180  CLI   0(R2),C' '                                                       
         BE    *+12                                                             
         CLI   0(R2),C'$'                                                       
         BNE   DWNR190                                                          
         LA    R2,1(R2)                                                         
         BCT   R1,DWNR180                                                       
         MVI   DWNFLD,C'0'         NO SIGNIF CHAR SEND ZERO                     
         MVI   PRTSIZE,1                                                        
         B     DWNR200                                                          
*                                                                               
DWNR190  STC   R1,PRTSIZE                                                       
         SHI   R1,1                                                             
         EX    R1,*+4                                                           
         MVC   DWNFLD(0),0(R2)             MOVE QTR2     TO DWN FLD             
DWNR200  GOTO1 ADWNL,DMCB,(RC),DWNNUM      DOWNLOAD TEXT                        
*                                                                               
         MVC   DWNFLD,XSPACES              SPACE OUT DOWNLOAD FIELD             
         LA    R1,L'PANQTR3                                                     
         LA    R2,PANQTR3                                                       
DWNR210  CLI   0(R2),C' '                                                       
         BE    *+12                                                             
         CLI   0(R2),C'$'                                                       
         BNE   DWNR220                                                          
         LA    R2,1(R2)                                                         
         BCT   R1,DWNR210                                                       
         MVI   DWNFLD,C'0'         NO SIGNIF CHAR SEND ZERO                     
         MVI   PRTSIZE,1                                                        
         B     DWNR230                                                          
*                                                                               
DWNR220  STC   R1,PRTSIZE                                                       
         SHI   R1,1                                                             
         EX    R1,*+4                                                           
         MVC   DWNFLD(0),0(R2)             MOVE QTR3     TO DWN FLD             
DWNR230  GOTO1 ADWNL,DMCB,(RC),DWNNUM      DOWNLOAD TEXT                        
*                                                                               
         MVC   DWNFLD,XSPACES              SPACE OUT DOWNLOAD FIELD             
         LA    R1,L'PANQTR4                                                     
         LA    R2,PANQTR4                                                       
DWNR240  CLI   0(R2),C' '                                                       
         BE    *+12                                                             
         CLI   0(R2),C'$'                                                       
         BNE   DWNR250                                                          
         LA    R2,1(R2)                                                         
         BCT   R1,DWNR240                                                       
         MVI   DWNFLD,C'0'         NO SIGNIF CHAR SEND ZERO                     
         MVI   PRTSIZE,1                                                        
         B     DWNR260                                                          
*                                                                               
DWNR250  STC   R1,PRTSIZE                                                       
         SHI   R1,1                                                             
         EX    R1,*+4                                                           
         MVC   DWNFLD(0),0(R2)            MOVE QTR4     TO DWN FLD              
DWNR260  GOTO1 ADWNL,DMCB,(RC),DWNNUM     DOWNLOAD TEXT                         
*                                                                               
         MVC   DWNFLD,XSPACES             SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'PANPCT),PANPCT    MOVE YTD ACTL PCT TO DWN FLD          
         LA    R1,L'PANPCT                YTD ACTL PCT FIELD LENGTH             
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
         MVC   DWNFLD,XSPACES              SPACE OUT DOWNLOAD FIELD             
         LA    R1,L'PANTOT                                                      
         LA    R2,PANTOT                                                        
DWNR270  CLI   0(R2),C' '                                                       
         BE    *+12                                                             
         CLI   0(R2),C'$'                                                       
         BNE   DWNR280                                                          
         LA    R2,1(R2)                                                         
         BCT   R1,DWNR270                                                       
         MVI   DWNFLD,C'0'         NO SIGNIF CHAR SEND ZERO                     
         MVI   PRTSIZE,1                                                        
         B     DWNR290                                                          
*                                                                               
DWNR280  STC   R1,PRTSIZE                                                       
         SHI   R1,1                                                             
         EX    R1,*+4                                                           
         MVC   DWNFLD(0),0(R2)            MOVE TOTALS   TO DWN FLD              
DWNR290  GOTO1 ADWNL,DMCB,(RC),DWNNUM     DOWNLOAD TEXT                         
         B     DWNR350                    DOWNLOAD END OF LINE                  
*                                                                               
DWNR300  DS    0H                START RECAP/GROUP RECAP REPORT                 
         CLI   5(R7),X'BF'       ARE THEY UNDERLINING                           
         BE    DWNR360                                                          
*                                                                               
* ALWAYS DOWNLOAD MONTH  OR  TOTAL MESSAGE IF TOTALS                            
*                                                                               
         CLC   4(2,R7),=C'12'                                                   
         BNE   *+14                                                             
         MVC   SVMNTH,=CL15'12 MONTH TOTAL'                                     
         B     DWNR360                                                          
*                                                                               
         MVC   DWNFLD,XSPACES             SPACE OUT DOWNLOAD FIELD              
         TM    FLAG,FLGGRP                ARE WE DOING GROUP?                   
         BNO   *+10                                                             
         MVC   DWNFLD(L'SVGRP),SVGRP                                            
         LA    R1,L'SVGRP                                                       
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT     DOWNLOAD TEXT                        
*                                                                               
         MVC   DWNFLD,XSPACES             SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'SVMNTH),SVMNTH                                          
         LA    R1,L'SVMNTH                                                      
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT     DOWNLOAD TEXT                        
*                                                                               
         LA    R1,19                                                            
         CLC   PRCOST,=C'COST'     ARE WE DOING COST?                           
         BNE   *+8                                                              
         LA    R1,6                                                             
*                                                                               
         MVC   DWNFLD,XSPACES             SPACE OUT DOWNLOAD FIELD              
         AHI   R1,-1                                                            
         EX    R1,*+4                                                           
         MVC   DWNFLD(0),PRDESC           MOVE 'DESC'   TO DWN FLD              
         LA    R1,L'PRDESC                ALWAYS 19 BYTES                       
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
         LA    R0,9                9 BUCKETS TO LOOP 8 DPTS 1 TOTAL             
         LA    R2,PRCSTHR1         ASSUME ITS NOT MANPOWER                      
         LA    R1,L'PRCSTHR1                                                    
         CLC   PRDESC(8),=CL8'MANPOWER'                                         
         BNE   *+8                                                              
         LA    R2,PRMANHR1         IT IS MANPOWER ADJUST ACCORDINGLY            
         CLC   PRDESC(9),=CL9'% OF TIME'                                        
         BNE   DWNR315                                                          
         LA    R2,27(R7)           THIS IS WHERE DATA FOR PERCENT IS            
*                                                                               
DWNR310  MVC   DWNFLD,XSPACES             SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'PRCSTHR1),0(R2)   MOVE HOURS TO DWN FLD                 
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD PCT AS TEXT                  
         LA    R1,L'PRCSTHR1                                                    
         LA    R2,L'PRCSTHR1(R2)                                                
         BCT   R0,DWNR310                                                       
         B     DWNR350                                                          
*                                                                               
DWNR315  DS    0H                                                               
         MVC   DWNFLD,XSPACES             SPACE OUT DOWNLOAD FIELD              
*                                                                               
DWNR320  CLI   0(R2),C' '          FIND 1ST SIGNIF CHAR                         
         BE    *+12                                                             
         CLI   0(R2),C'$'          SKIP $ SIGNS                                 
         BNE   DWNR330                                                          
         LA    R2,1(R2)                                                         
         BCT   R1,DWNR320                                                       
         MVI   DWNFLD,C'0'         NO SIGNIF CHAR SEND ZERO                     
         MVI   PRTSIZE,1                                                        
         B     DWNR340                                                          
*                                                                               
DWNR330  AHI   R1,-1                                                            
         EX    R1,*+4                                                           
         MVC   DWNFLD(0),0(R2)            MOVE HOURS TO DWN FLD                 
         STC   R1,PRTSIZE                                                       
         LA    R2,1(R1,R2)         BUMP TO NEXT FLD IN XP LINE                  
DWNR340  GOTO1 ADWNL,DMCB,(RC),DWNNUM     DOWNLOAD NUMBER                       
         LA    R1,L'PRCSTHR1                                                    
         BCT   R0,DWNR315                                                       
*                                                                               
DWNR350  MVC   DWNFLD,XSPACES             SPACE OUT DOWNLOAD FIELD              
         GOTO1 ADWNL,DMCB,(RC),DWNEOL     DOWNLOAD EOL MARKER                   
DWNR360  LA    R7,L'SVPRNT(R7)                                                  
         MVC   RCSUBPRG,SVSUBPRG                                                
         SR    R0,R0                                                            
         IC    R0,SVCNT                                                         
         BCT   R0,DWNR10                  DO IT FOUR TIMES                      
*                                                                               
DWNXIT   XMOD1                                                                  
         DROP  R4,R7                                                            
         EJECT                                                                  
**********************************************************************          
* DOWNLOAD HEADLINES (ONCE PER REPORT)                               *          
**********************************************************************          
         SPACE 1                                                                
*DWNHEAD  NTR1                                                                  
*         OI    DWNSTAT,DWNHDLN     SET SWITCH TO SHOW HDLNS WERE DWNLD         
*                                                                               
*         LA    R0,HD1LNQ                 NUMBER OF HEADINGS IN LINE 1          
*         L     R2,AHD1TAB                FIRST HEADLINE TABLE                  
*DWNH10   MVC   DWNFLD,XSPACES            SPACE OUT DOWNLOAD FIELD              
*         MVC   DWNFLD(L'HD1TAB),0(R2)    FIRST HEADLINE FIELDS                 
*         GOTO1 ADWNL,DMCB,(RC),DWNTEXT   DOWNLOAD TEXT                         
*         LA    R2,L'HD1TAB(R2)                                                 
*         BCT   R0,DWNH10                                                       
*                                                                               
*         MVC   DWNFLD,XSPACES            SPACE OUT DOWNLOAD FIELD              
*         GOTO1 ADWNL,DMCB,(RC),DWNEOL    DOWNLOAD EOL MARKER                   
*                                                                               
*         LA    R0,HD2LNQ                 NUMBER OF HEADINGS IN LINE 2          
*         L     R2,AHD2TAB                SECOND HEADLINE TABLE                 
*DWNH20   MVC   DWNFLD,XSPACES            SPACE OUT DOWNLOAD FIELD              
*         MVC   DWNFLD(L'HD2TAB),0(R2)    FIRST HEADLINE FIELDS                 
*                                                                               
*         GOTO1 ADWNL,DMCB,(RC),DWNTEXT   DOWNLOAD TEXT                         
*         LA    R2,L'HD2TAB(R2)                                                 
*         BCT   R0,DWNH20                                                       
*                                                                               
*         MVC   DWNFLD,XSPACES            SPACE OUT DOWNLOAD FIELD              
*         GOTO1 ADWNL,DMCB,(RC),DWNEOL    DOWNLOAD EOL MARKER                   
*                                                                               
*DWNHX    B     DWNXIT                                                          
         EJECT                                                                  
***********************************************************************         
* TABLES/CONSTANTS                                                    *         
***********************************************************************         
         SPACE 1                                                                
RPTTAB   DS    0CL3                REPORT NUMBER TABLE                          
         DC    X'00',C'1',X'00'                                                 
         DC    X'01',C'0',X'00'    DEPARTMENT TOTAL                             
         DC    X'02',C'3',X'00'    GROUP TOTAL PAGE                             
         DC    X'03',C'0',X'00'                                                 
         DC    X'04',C'2',X'00'    MULTILOCATION #2 QOPT=B                      
         DC    X'05',C'3',X'00'    GROUP PERSON #3 QOPT=C                       
         DC    X'06',C'4',X'10'    GROUP RECAP #4,QOPT=D                        
         DC    X'07',C'5',X'10'    GROUP ANALYSIS #5,QOPT=E                     
         DC    X'06',C'6',X'00'    RECAP REPORT #6,QOPT=F                       
         DC    X'07',C'7',X'00'    ANALYSIS REPORT #7,QOPT=G                    
         DC    X'08',C'3',X'00'                                                 
         DC    X'FF'                                                            
*                                                                               
LEVTABP  DS    0CL2                LEVELS OF CODE PERSON REPORT                 
         DC    X'00',AL1(CODAPER-CODADRP)                                       
         DC    X'01',AL1(CODAAPG-CODADRP)                                       
         DC    X'03',AL1(CODAEND-CODADRP)                                       
         DC    X'04',AL1(CODAPER-CODADRP)                                       
         DC    X'FF'                                                            
*                                                                               
LEVTABPG DS    0CL2                LEVEL OF CODES PER/GRP REPORT                
         DC    X'01',AL1(CODAAPGG-CODADRPG)                                     
         DC    X'02',AL1(CODAGRPG-CODADRPG)                                     
         DC    X'03',AL1(CODAENDG-CODADRPG)                                     
         DC    X'05',AL1(CODAPERG-CODADRPG)                                     
         DC    X'08',AL1(CODAPERG-CODADRPG)                                     
         DC    X'FF'                                                            
*                                                                               
* PERSON REPORT TABLE                                                           
*                                                                               
CODADRP  DS    0CL3                ADDRESS TABLE OF PERSON LEVELS               
CODAPER  DC    AL2(APGDPT-WORKD),AL1(2)                                         
         DC    AL2(DPTCODE-WORKD),AL1(4)                                        
         DC    AL2(SUBCODE-WORKD),AL1(2)                                        
         DC    AL2(PRSCODE-WORKD),AL1(L'PRSCODE)                                
CODNUMP  EQU   (*-CODADRP)/CODLNQ                                               
CODAEND  DC    X'FF'                                                            
*                                                                               
CODAAPG  DC    AL2(APGDPT-WORKD),AL1(2)                                         
         DC    X'FF'                                                            
*                                                                               
* GROUP REPORT TABLE                                                            
*                                                                               
CODADRPG DS    0CL3                ADDRESS TABLE OF PER/GRP LEVELS              
CODAPERG DC    AL2(SVGRP-WORKD),AL1(2)                                          
         DC    AL2(APGDPT-WORKD),AL1(2)                                         
         DC    AL2(DPTCODE-WORKD),AL1(4)                                        
         DC    AL2(SUBCODE-WORKD),AL1(2)                                        
         DC    AL2(PRSCODE-WORKD),AL1(L'PRSCODE)                                
CODNUMPG EQU   (*-CODADRPG)/CODLNQ                                              
CODAENDG DC    X'FF'                                                            
*                                                                               
CODAAPGG DC    AL2(SVGRP-WORKD),AL1(2)                                          
         DC    AL2(APGDPT-WORKD),AL1(2)                                         
         DC    X'FF'                                                            
*                                                                               
CODAGRPG DC    AL2(SVGRP-WORKD),AL1(2)                                          
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* DOWNLOAD MODULE                                                    *          
*          PARM1 - RC                                                *          
*          PARM2 - ACTION                                            *          
**********************************************************************          
         SPACE 1                                                                
DWNL     DS    0D                                                               
         NMOD1 0,**DOWN**                                                       
         L     RC,0(R1)                                                         
         L     RF,4(R1)                                                         
         STC   RF,DWNMODE          SAVE CURRENT MODE                            
         USING DLCBD,R5                                                         
         L     R5,ADWNBUF                                                       
*                                                                               
         CLI   DWNMODE,DWNINIT     INITIALIZE                                   
         BE    DWNL10                                                           
         CLI   DWNMODE,DWNTEXT     DOWN-LOAD TEXT                               
         BE    DWNL20                                                           
         CLI   DWNMODE,DWNNUM      DOWN-LOAD NUMBER                             
         BE    DWNL30                                                           
         CLI   DWNMODE,DWNPACK     DOWN-LOAD NUMBER (PACKED)                    
         BE    DWNL40                                                           
         MVI   DLCBACT,DLCBEOL                                                  
         CLI   DWNMODE,DWNEOL      END OF LINE                                  
         BE    DWNL50                                                           
         MVI   DLCBACT,DLCBEOR                                                  
         CLI   DWNMODE,DWNEOR      END OF REPORT                                
         BE    DWNL50                                                           
         DC    H'0'                                                             
*                                                                               
* INITIALIZATION                                                                
*                                                                               
DWNL10   TM    DWNSTAT,DWNINTZ     HAS IT ALREADY BEEN INITIALIZED?             
         BO    DWNLX               YES - EXIT                                   
         MVI   DLCBACT,DLCBINIT    DOWN LOAD ACTION IS START                    
         LA    RE,XP               PRINT LINE                                   
         ST    RE,DLCBAPL                                                       
         LA    RE,DWNHOOK          POINT TO HOOK FOR APPLICATION                
         ST    RE,DLCBAPR                                                       
         MVC   DLCXMAXL,=Y(L'XP)                                                
         MVI   DLCXDELC,C' '       DELIMITER                                    
         MVI   DLCXEOTC,C'"'       TEXT DELIMITER                               
         MVI   DLCXEOTA,C''''      ALTERNATE TEXT DELIMITER                     
         MVI   DLCXEOLC,X'5E'      SEMI-COLON, END-OF-LINE                      
         MVI   DLCXEORC,C':'       END-OF-REPORT                                
         GOTO1 DLFLD,(R5)                                                       
         MVI   FORCEHED,C'Y'       EXCEPT FIRST TIME IN                         
         GOTO1 ACREPORT                                                         
         MVC   DLCBFLD,XSPACES     MUST CLEAR FIRST TIME IN                     
*                                  TURN OFF DOWN-LOAD ROW FLDS AS C' '          
         OI    DWNSTAT,DWNINTZ     TURN ON INITIALIZED BYTE                     
         B     DWNLX               EXIT                                         
*                                                                               
* DOWNLOAD A RECORD - TEXT                                                      
*                                                                               
DWNL20   MVC   DLCBLEN,PRTSIZE     LEN OF FIELD-SET TO 0 (NO PADDING)           
         MVI   DLCBACT,DLCBPUT     ACTION IS PUT                                
         MVI   DLCBTYP,DLCBTXT     TYPE   IS TEXT                               
         MVC   DLCBFLD,XSPACES                                                  
         MVC   DLCBFLD(L'DWNFLD),DWNFLD                                         
         B     DWNL50              DOWN-LOAD FIELD                              
*                                                                               
* DOWNLOAD A RECORD - NUMBER                                                    
*                                                                               
DWNL30   MVI   DLCBLEN,0           LEN OF FIELD-SET TO 0 (NO PADDING)           
         OI    DLCBFLG1,DLCBFTRM   DOWN-LOAD WITH TRAILING MINUS                
         MVI   DLCBACT,DLCBPUT     ACTION IS PUT                                
         MVI   DLCBTYP,DLCBNUM     TYPE   IS NUMBER                             
         MVI   DLCBLEN,16          YES, USE MAXIMUM LENGTH OF NUMERICS          
         MVC   DLCBFLD,XSPACES                                                  
         MVC   DLCBFLD(L'DWNFLD),DWNFLD                                         
         B     DWNL50              DOWN-LOAD FIELD                              
*                                                                               
* DOWNLOAD A RECORD - NUMBER (PACKED)                                           
*                                                                               
DWNL40   MVI   DLCBTYP,DLCBPACF    PACKED DATA                                  
         OI    DLCBFLG1,DLCBFTRM   DOWN-LOAD WITH TRAILING MINUS                
         MVI   DLCBACT,DLCBPUT     ACTION IS PUT                                
         MVI   DLCBLEN,8        L'PKFLDS YES, USE MAX LEN OF NUMERICS           
         XC    DLCBFLD,DLCBFLD     CLEAN DWNLOAD FIELD TO 0'S                   
         MVC   DLCBFLD(L'DWNFLD),DWNFLD                                         
         NC    DLCBFLD,DLCBFLD     YES, MAKE SURE NUMERIC FLD NOT ZEROS         
         BNZ   DWNL50              NOT  ZERO, DOWN-LOAD FIELD                   
         MVI   DLCBLEN,1           ZERO, SET LENGTH 1 TO DOWN-LOAD A 0          
*                                                                               
* END OF LINE/END OF RECORD                                                     
*                                                                               
DWNL50   GOTO1 DLFLD,(R5)          DOWN-LOAD FIELD                              
*                                                                               
DWNLX    XMOD1                                                                  
         DROP  R5                                                               
         EJECT                                                                  
**********************************************************************          
* DOWNLOAD HOOK                                                      *          
**********************************************************************          
         SPACE 1                                                                
DWNHOOK  MVI   FORCEHED,C'N'       NEVER HEAD UP                                
         MVI   SPACING,1                                                        
         MVI   LINE,1                                                           
         L     RF,ACREPORT                                                      
         BR    RF                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* GET APG NAME                                                       *          
*     NOTE:R5 SET TO THE ADDR OF TABLE FOR LOOKUP                    *          
**********************************************************************          
         SPACE 1                                                                
         DS    0H                                                               
GTANME   NMOD1 0,**GTANM*                                                       
         L     RC,0(R1)                                                         
         C     R5,TIBUFF           LOOK IN TITLE/PERSON TABLE?                  
         BE    GTA20                                                            
         C     R5,APGBUFF                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         USING BIND,R5                                                          
         L     R0,BININ            R0 TO NUMBER IN TABLE                        
         LA    R2,BINTABLE         R2 TO APG TABLE                              
         USING APGD,R2                                                          
         CLC   APGREC,WORK                                                      
         BE    GTA10               FOUND APG CODE IN TABLE                      
         LA    R2,APGLEN(R2)                                                    
         BCT   R0,*-14                                                          
         MVC   WORK,XSPACES                                                     
         MVC   WORK(7),=C'UNKNOWN'                                              
         B     GTAX                 NOT IN TABLE                                
GTA10    MVC   WORK(36),APGNME      RETURN NAME                                 
         B     GTAX                                                             
*                                                                               
         USING BIND,R5                                                          
GTA20    L     R0,BININ            R0 TO NUMBER IN TABLE                        
         LA    R2,BINTABLE         R2 TO TITLE TABLE                            
         USING TITD,R2                                                          
         CLC   TITD(TIKLEN),WORK                                                
         BE    GTA30               FOUND TITLE IN TABLE                         
         LA    R2,TILEN(R2)                                                     
         BCT   R0,*-14                                                          
         MVC   WORK,XSPACES                                                     
         MVC   WORK(7),=C'UNKNOWN'                                              
         B     GTAX                 NOT IN TABLE                                
GTA30    MVC   WORK(36),TINAME      RETURN NAME                                 
*                                                                               
GTAX     XIT1                                                                   
         DROP  R2,R5                                                            
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* ADD YTD PROUCT HOURS TO CURRENT BIN TABLE ENTRY FOR GRPREP ROUTINE *          
**********************************************************************          
         SPACE 1                                                                
         DS    0H                                                               
ADDPHR   NMOD1 0,**ADDP**                                                       
         L     RC,0(R1)                                                         
         USING SRTD,R5                                                          
         L     R5,ASRTREC          SORT RECORD                                  
*                                                                               
         CLC   SRDEPT,=C'00'       DON'T ADD FOR UNKNOWN DEPT                   
         BE    ADDPX                                                            
*                                                                               
         USING BIND,RE                                                          
         L     RE,AGRPBUFF         RE=GROUP TABLE                               
         ICM   R0,15,BININ                                                      
         BZ    ADDPX                                                            
         USING GRPD,R4                                                          
         LA    R4,BINTABLE                                                      
         DROP  RE                                                               
*                                                                               
ADDP10   CLC   SRPERSON,GRPPERS    SAME PERSON?                                 
         BNE   ADDP30                                                           
*                                                                               
         OC    GRPSTAT,SRSTAT      SAVE OFF STATUS BYTE                         
         TM    ACCSTUS,DUPLOCAT                                                 
         BNO   *+8                                                              
         OI    GRPSTAT,GRPDUPLC    SET FOR DUP LOCATION                         
*                                                                               
         USING SDTD,R6                                                          
         LA    R6,SRTOTAL          MONTHLY DETAIL                               
         AP    GRPYTPHR,SDYPHRS    YTD PRODUCT HOURS                            
         AP    GRPTPST,SDYPST      YTD TOTAL POSTED                             
*                                                                               
         USING GRPDTD,R3                                                        
         LA    R3,GRPDTL           POINT R3 TO DETAIL INFO                      
         LA    R2,MAXMTHS          ALL MONTHS                                   
*                                                                               
ADDP20   AP    GRPYTCHR,SDYCHRS    YTD CLIENT  HOURS                            
*                                                                               
         LA    R3,GRPDTLQ(R3)                                                   
         BCT   R2,ADDP20                                                        
*                                                                               
ADDP30   LA    R4,GRPLEN(R4)                                                    
         BCT   R0,ADDP10                                                        
*                                                                               
ADDPX    XIT1                                                                   
         DROP  R3,R4,R5,R6                                                      
         EJECT                                                                  
**********************************************************************          
* POST SORT RECORD TO BUFFALO                                        *          
**********************************************************************          
         SPACE 1                                                                
         USING SRTD,R5                                                          
         DS    0H                                                               
POST     NMOD1 0,**POST**                                                       
         L     RC,0(R1)                                                         
         L     R5,ASRTREC          SORT RECORD                                  
         TM    ACCSTUS,DUPLOCAT    CHECK IF DUPLICATE LOCATION                  
         BZ    POSTX               IF NOT DON'T POST                            
         CLC   SRDEPT,=C'00'       UNKNOWN                                      
         BE    POSTX               DON'T POST TO DUP TABLE                      
*                                                                               
         USING BUFD,R4                                                          
         L     R4,ABUFWRK                                                       
         MVC   BUFKEY(BUFKLEN),XSPACES  CLEAR BUFFALO AREA                      
         LA    R1,BUFBK                                                         
         LA    R0,BBKCNT                                                        
         ZAP   0(BUFBKLN,R1),ZEROS                                              
         LA    R1,BUFBKLN(R1)                                                   
         BCT   R0,*-10                                                          
*                                                                               
         USING SDTD,R6                                                          
         LA    R6,SRDTL            MONTHLY DETAIL                               
         MVC   SVKEY,XSPACES                                                    
         MVI   BUFTYPE,BUFDUP      DUP REPORT TYPE                              
         MVC   BUFPERS,SRPERSON                                                 
         MVC   BUFDEPT,SRDEPT                                                   
         MVC   BUFOFDPT,SROFFICE                                                
         MVC   BUFTITLE,SRTITLE                                                 
         MVI   BUFLINE,BUFDETL     DETAIL LINE                                  
         MVC   SVKEY(BUFKLEN),BUFKEY                                            
         LA    R2,MAXMTHS+1        ALL MONTHS PLUS TOTAL                        
*                                                                               
POST10   MVC   BUFKEY,SVKEY        RESTORE BUFKEY                               
         OC    SDMNTH,SDMNTH                                                    
         BZ    POST40              GET NEXT MONTH                               
         CLI   SDMNTH,X'FF'        TOTAL LINE?                                  
         BNE   *+8                                                              
         MVI   BUFLINE,BUFTOTL     MARK AS TOTAL LINE                           
         MVC   BUFMNTH,SDMNTH      MONTH                                        
         LA    R1,BUFBK            CLEAR                                        
         LA    R0,BBKCNT                                                        
         ZAP   0(BUFBKLN,R1),ZEROS                                              
         LA    R1,BUFBKLN(R1)                                                   
         BCT   R0,*-10                                                          
         CLI   BUFMNTH,X'FF'        TOTAL LINE?                                 
         BE    POST30                                                           
         L     RE,SUMPST                                                        
         USING SUMPD,RE                                                         
POST20   LA    R1,BUFD             BUFFALO ACCUMS                               
         LA    RF,SDTD             SORT ACCUMS                                  
         CLI   SUMPDISP,EOT                                                     
         BE    POST30                                                           
         AH    R1,SUMPBDIS         POINTS TO ACCUMS                             
         AH    RF,SUMPDISP                                                      
         CLC   SUMPBDIS,=H'0'                                                   
         BE    *+10                                                             
         AP    0(BUFBKLN,R1),0(SDBUKLN,RF)                                      
         LA    RE,SUMPLN(RE)                                                    
         B     POST20                                                           
*                                                                               
POST30   DS    0H                  PUT DUP TO BUFFALO                           
*        MVC   MSG1,=CL10'BUFKEY 1'                                             
*        GOTO1 ADUMP,DMCB,(RC),BUFKEY,L'BUFKEY                                  
*                                                                               
         GOTO1 BUFFALO,DMCB,(L'PUTC,PUTC),ADBUFF,(R4)                           
         MVC   BUFLOCA(BUFLOCLN),FOXES                                          
         BASR  RE,RF                                                            
*                                                                               
POST40   LA    R6,SDLEQ(R6)        NEXT MONTH                                   
         BCT   R2,POST10                                                        
*                                                                               
POSTX    DS    0H                                                               
         DROP  R4,R5,R6,RE                                                      
         EJECT                                                                  
**********************************************************************          
* POST TO RECAP REPORT                                               *          
**********************************************************************          
         SPACE 1                                                                
         USING SRTD,R5                                                          
         USING SDTD,R6                                                          
PSTCAP   DS    0H                                                               
         CLI   QOPT6,C'S'                                                       
         BE    PSTCXX              NO RECAP REPORT                              
         L     R5,ASRTREC          SORT RECORD                                  
         CLC   SRDEPT,=C'00'       UNKNOWN                                      
         BE    PSTCXX                                                           
         CLI   SRACCT,X'FF'                                                     
         BE    PSTCXX              DONT PROCESS TOTAL RECORDS                   
*                                                                               
         LA    R6,SRDTL            MONTHLY DETAIL                               
         USING TCAPD,R4                                                         
         L     R4,ATCAPWK                                                       
         MVC   TCAPKEY(TCAKLEN),XSPACES  CLEAR RECAP AREA                       
         LA    R1,TCABK                                                         
         LA    R0,TCACNT                                                        
         ZAP   0(TCABKLN,R1),ZEROS                                              
         LA    R1,TCABKLN(R1)                                                   
         BCT   R0,*-10                                                          
*                                                                               
         LA    R2,MAXMTHS          ALL MONTHS                                   
         MVC   SVKEY,XSPACES                                                    
         MVI   TCAPTYP,TCAPEQU     RECORD TYPE                                  
         MVC   SVKEY(TCAKLEN),TCAPKEY                                           
PSTC20   MVC   TCAPKEY,SVKEY       RESTORE KEY                                  
         OC    SDMNTH,SDMNTH                                                    
         BZ    PSTC40              GET NEXT MONTH                               
         CLC   SDMNTH,SVSTART                                                   
         BL    PSTC40              GET NEXT MONTH                               
         MVC   TCAPYYMM,SDMNTH     YEAR/MONTH                                   
         LA    R1,TCABK                                                         
         LA    R0,TCACNT                                                        
         ZAP   0(TCABKLN,R1),ZEROS                                              
         LA    R1,TCABKLN(R1)                                                   
         BCT   R0,*-10                                                          
         AP    TCAPHT,SDPRDHR      ADD MNTHLY HRS AND COST TO TOTAL             
         AP    TCAPCT,SDTOTAL                                                   
*                                                                               
         L     RF,DPTBUCK          READ FH BUCKETS TABLE                        
         TM    FLAG2,FLGFH         ARE WE READING FH SUPERLEDGER                
         BO    *+8                                                              
         L     RF,DPTBKFR          READ FR BUCKETS TABLE                        
         USING DPTD,RF                                                          
PSTC30   CLI   DPTNUMB,EOT                                                      
         BNE   *+6                                                              
         DC    H'0'                CAN'T MATCH DEPARTMENT                       
         CLC   DPTNUMB,SRDEPT                                                   
         BE    *+12                                                             
         LA    RF,DPTDLN(RF)                                                    
         B     PSTC30                                                           
         LR    R3,R4               RECORD ADDRESS                               
         LR    R1,R4                                                            
         AH    R1,DPTHDISP         POINTS TO DEPT HOURS ACCUM                   
         AH    R3,DPTCDISP         POINTS TO DEPT COST ACCUM                    
         AP    0(TCABKLN,R1),SDPRDHR                                            
         AP    0(TCABKLN,R3),SDTOTAL                                            
         DROP  RF                                                               
         GOTO1 BUFFALO,DMCB,(L'PUTC,PUTC),ADBUFF,(R4)                           
         MVC   TCAPYYMM,=X'FFFF'                                                
         BASR  RE,RF                                                            
PSTC40   LA    R6,SDLEQ(R6)        NEXT MONTH                                   
         BCT   R2,PSTC20                                                        
PSTCXX   DS    0H                                                               
         DROP  R4,R5,R6                                                         
         EJECT                                                                  
**********************************************************************          
* POST TO QTRLY REPORT                                               *          
**********************************************************************          
         SPACE 1                                                                
         USING SRTD,R5                                                          
         USING SDTD,R6                                                          
PSTQTR   DS    0H                                                               
         CLI   QOPT8,C'S'                                                       
         BE    PSTQXX              NO TIME COST REPORT                          
         L     R5,ASRTREC          SORT RECORD                                  
         CLC   SRDEPT,=C'00'       UNKNOWN                                      
         BE    PSTQXX                                                           
         CLI   SRACCT,X'FF'                                                     
         BE    PSTCXX              DONT PROCESS TOTAL RECORDS                   
*                                                                               
         LA    R6,SRDTL            MONTHLY DETAIL                               
HRS      USING TCQD,R4                                                          
         L     R4,ATCQWK           CLEAR HOURS WORK AREA                        
         MVC   HRS.TCQKEY(TCQKLEN),XSPACES  CLEAR  AREA                         
         LA    R1,HRS.TCQBK                                                     
         BAS   RE,PSTQ10                                                        
COST     USING TCQD,R3                                                          
         L     R3,ATCQWK2          CLEAR COST WORK AREA                         
         MVC   COST.TCQKEY(TCQKLEN),XSPACES                                     
         LA    R1,COST.TCQBK                                                    
         BAS   RE,PSTQ10                                                        
         B     PSTQ20                                                           
*                                                                               
PSTQ10   LA    R0,TCQCNT                                                        
         ZAP   0(TCQBKLN,R1),ZEROS                                              
         LA    R1,TCQBKLN(R1)                                                   
         BCT   R0,*-10                                                          
         BR    RE                                                               
*                                                                               
PSTQ20   LA    R2,MAXMTHS+1        ALL MONTHS PLUS TOTAL                        
         MVC   SVKEY,XSPACES                                                    
         MVI   HRS.TCQTYP,TCQEQU   RECORD TYPE                                  
         MVC   HRS.TCQDEPT,SRDEPT  APG DEPT                                     
         MVC   HRS.TCQTITL,SRTITLE APG SUBDEPT                                  
         MVC   HRS.TCQSUB,SUBNAME  TITLE                                        
         MVC   HRS.TCQPRSN,SRPERSON    PERSON                                   
         MVC   SVKEY(TCQKLEN),HRS.TCQKEY                                        
         MVC   COST.TCQKEY,HRS.TCQKEY                                           
PSTQ30   MVC   HRS.TCQKEY,SVKEY        RESTORE KEY                              
         MVC   COST.TCQKEY,SVKEY                                                
         OC    SDMNTH,SDMNTH                                                    
         BZ    PSTQ60              GET NEXT MONTH                               
         CLC   SDMNTH,SVSTART                                                   
         BL    PSTQ60              GET NEXT MONTH                               
         CLI   SDMNTH,X'FF'                                                     
         BE    PSTQ50                                                           
         L     RE,QTRMTAB          QTRS TABLE                                   
         USING QTRD,RE                                                          
PSTQ40   CLI   QTRMTH,EOT                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   SDMNTH+1(1),QTRMTH  WHAT QTR DOES MONTH BELONG TO?               
         BE    *+12                                                             
         LA    RE,QTRDLN(RE)                                                    
         B     PSTQ40                                                           
         SR    RF,RF                                                            
         IC    RF,QTRNUMB                                                       
         BCTR  RF,0                                                             
         MH    RF,=Y(TCQBKLN)                                                   
         STH   RF,HALF                                                          
         LA    R1,HRS.TCQBK                                                     
         AH    R1,HALF             POINTS TO QTR ACCUM                          
         DROP  RE                                                               
*                                                                               
*                                  * * * H 0 U R S * * *                        
         AP    0(TCQBKLN,R1),SDPRDHR        ADD TO QTR                          
         AP    HRS.TCTOT,SDPRDHR            ADD TO TOTAL                        
         B     *+14                                                             
PSTQ50   ZAP   HRS.TCTIME,SDYPCT       % OF TIME ON TOYOTA(TOT REC)             
         B     PSTQ60                                                           
*                                                                               
*                                  * * * C O S T * * *                          
         LA    R1,COST.TCQBK                                                    
         AH    R1,HALF             POINTS TO QTR ACCUM                          
         AP    0(TCQBKLN,R1),SDTOTAL                                            
         AP    COST.TCTOT,SDTOTAL     ADD TO TOTAL                              
*                                                                               
PSTQ60   LA    R6,SDLEQ(R6)        NEXT MONTH                                   
         BCT   R2,PSTQ30                                                        
                                                                                
*                                  HOURS RECORD                                 
         GOTO1 BUFFALO,DMCB,(L'PUTC,PUTC),ADBUFF,ATCQWK                         
         XC    HRS.TCQPRSN,HRS.TCQPRSN                                          
         MVI   HRS.TCQPRSN,X'FE'                                                
         BASR  RE,RF               SUB-DEPT TOTAL                               
         XC    HRS.TCQSUB,HRS.TCQSUB                                            
         MVC   HRS.TCQSUB(2),=X'FE00'                                           
         MVC   HRS.TCQTITL,=X'FFFF'                                             
         BASR  RE,RF               DEPT TOTALS                                  
         MVC   HRS.TCQDEPT(3),=X'FF0000'                                        
         BASR  RE,RF               REPORT TOTALS                                
*                                                                               
*                                  COST RECORD                                  
         XC    COST.TCQPRSN,COST.TCQPRSN                                        
         XC    COST.TCQSUB,COST.TCQSUB                                          
         MVC   COST.TCQSUB(2),=X'FF00' DEPT TOTALS FOR COST                     
         MVC   COST.TCQTITL,=X'FFFF'                                            
         GOTO1 BUFFALO,DMCB,(L'PUTC,PUTC),ADBUFF,ATCQWK2                        
PSTQXX   DS    0H                                                               
         XIT1                                                                   
         DROP  R5,R6                                                            
         DROP  HRS,COST                                                         
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* TABLES AND CONSTANTS                                               *          
**********************************************************************          
         SPACE 1                                                                
         DS    0D                                                               
CIO      DS    CL(IOSIZE)         IO AREA                                       
         DS    0D                                                               
CIO2     DS    CL(IOSIZE)         IO AREA 2                                     
         DS    0D                                                               
CDIO     DS    CL100              DIRECTORY IO AREA                             
         DS    0D                                                               
CSALAREA DS    CL(SALLNQ)         SALARY AREA  (ACSALHST)                       
         DS    0D                                                               
CSALARE2 DS    CL(SLRLEN)         SALARY AREA  (ACSLRY)                         
         DS    0D                                                               
*                                                                               
CCOMTAB  DS    0D                  TBLE FOR COMMON WORK NMOD ENTRIES            
         DC    AL2(COMMON-WORKD),AL2(COM1-WORKD),AL1(COMNUM1)                   
         DC    AL2(COMMON2-WORKD),AL2(COM2-WORKD),AL1(COMNUM2)                  
         DC    AL1(EOT)                                                         
*                                                                               
*                                                                               
*                                  TABLE FOR POSTING PERSON TOTALS              
CSUMPST  DS    0D                                                               
         DC    Y(SDSAL-SDTD),AL1(SUMPADD),AL1(0),Y(BUFSAL-BUFD)                 
         DC    Y(SDSALY-SDTD),AL1(SUMPADD),AL1(0),Y(BUFSALY-BUFD)               
         DC    Y(SDPRDHR-SDTD),AL1(SUMPADD),AL1(0),Y(BUFPRDHR-BUFD)             
         DC    Y(SDCLIHR-SDTD),AL1(SUMPADD),AL1(0),Y(BUFCLIHR-BUFD)             
         DC    Y(SDTHRS-SDTD),AL1(SUMPADD),AL1(0),Y(BUFTHRS-BUFD)               
         DC    Y(SDYPHRS-SDTD),AL1(SUMPZAP),AL1(0),Y(BUFYPHRS-BUFD)             
         DC    Y(SDYCHRS-SDTD),AL1(SUMPZAP),AL1(0),Y(BUFYCHRS-BUFD)             
         DC    Y(SDYTHRS-SDTD),AL1(SUMPZAP),AL1(0),Y(BUFYTHRS-BUFD)             
         DC    Y(SDYPCT-SDTD),AL1(SUMPZAP),AL1(0),Y(BUFYPCT-BUFD)               
         DC    Y(SDHRRAT-SDTD),AL1(SUMPZAP),AL1(0),Y(BUFHRRAT-BUFD)             
         DC    Y(SDPST-SDTD),AL1(SUMPADD),AL1(0),Y(BUFPST-BUFD)                 
         DC    Y(SDYPST-SDTD),AL1(SUMPZAP),AL1(0),Y(BUFYPST-BUFD)               
         DC    Y(SDFACA-SDTD),AL1(SUMPADD),AL1(0),Y(BUFFACA-BUFD)               
         DC    Y(SDTOTAL-SDTD),AL1(SUMPADD),AL1(0),Y(BUFTOTAL-BUFD)             
         DC    AL1(EOT)                                                         
*                                                                               
*                                                                               
*             SUPERLEDGER FH       TABLE OF DEPTS FOR RECAP                     
CDPTBUCK DS    0F                                                               
         DC    CL2'10',AL2(TCAPH10-TCAPD),AL2(TCAPC10-TCAPD)                    
         DC    AL2(DPT10-DPTTOTS)                                               
         DC    CL2'11',AL2(TCAPH11-TCAPD),AL2(TCAPC11-TCAPD)                    
         DC    AL2(DPT11-DPTTOTS)                                               
         DC    CL2'12',AL2(TCAPH12-TCAPD),AL2(TCAPC12-TCAPD)                    
         DC    AL2(DPT12-DPTTOTS)                                               
         DC    CL2'20',AL2(TCAPH20-TCAPD),AL2(TCAPC20-TCAPD)                    
         DC    AL2(DPT20-DPTTOTS)                                               
         DC    CL2'25',AL2(TCAPH25-TCAPD),AL2(TCAPC25-TCAPD)                    
         DC    AL2(DPT25-DPTTOTS)                                               
         DC    CL2'13',AL2(TCAPH13-TCAPD),AL2(TCAPC13-TCAPD)                    
         DC    AL2(DPT13-DPTTOTS)                                               
         DC    CL2'14',AL2(TCAPH14-TCAPD),AL2(TCAPC14-TCAPD)                    
         DC    AL2(DPT14-DPTTOTS)                                               
         DC    CL2'15',AL2(TCAPH15-TCAPD),AL2(TCAPC15-TCAPD)                    
         DC    AL2(DPT15-DPTTOTS)                                               
         DC    AL1(EOT)                                                         
*             SUPERLEDGER FR       TABLE OF DEPTS FOR RECAP                     
CDPTBKFR DS    0F                                                               
         DC    CL2'05',AL2(TCAPH10-TCAPD),AL2(TCAPC10-TCAPD)                    
         DC    AL2(DPT10-DPTTOTS)                                               
         DC    CL2'10',AL2(TCAPH11-TCAPD),AL2(TCAPC11-TCAPD)                    
         DC    AL2(DPT11-DPTTOTS)                                               
         DC    CL2'15',AL2(TCAPH12-TCAPD),AL2(TCAPC12-TCAPD)                    
         DC    AL2(DPT12-DPTTOTS)                                               
         DC    CL2'20',AL2(TCAPH20-TCAPD),AL2(TCAPC20-TCAPD)                    
         DC    AL2(DPT20-DPTTOTS)                                               
         DC    CL2'25',AL2(TCAPH25-TCAPD),AL2(TCAPC25-TCAPD)                    
         DC    AL2(DPT25-DPTTOTS)                                               
         DC    CL2'30',AL2(TCAPH13-TCAPD),AL2(TCAPC13-TCAPD)                    
         DC    AL2(DPT13-DPTTOTS)                                               
         DC    CL2'35',AL2(TCAPH14-TCAPD),AL2(TCAPC14-TCAPD)                    
         DC    AL2(DPT14-DPTTOTS)                                               
         DC    CL2'40',AL2(TCAPH15-TCAPD),AL2(TCAPC15-TCAPD)                    
         DC    AL2(DPT15-DPTTOTS)                                               
         DC    AL1(EOT)                                                         
*                                                                               
*                                  TABLE OF QRTLY MONTHS                        
*                                                                               
CQTRMTAB DC    X'10',CL15'OCTOBER, XXXX',X'01'                                  
         DC    X'11',CL15'NOVEMBER, XXXX',X'01'                                 
         DC    X'12',CL15'DECEMBER, XXXX',X'01'                                 
         DC    X'01',CL15'JANUARY, XXXX',X'02'                                  
         DC    X'02',CL15'FEBRUARY, XXXX',X'02'                                 
         DC    X'03',CL15'MARCH, XXXX',X'02'                                    
         DC    X'04',CL15'APRIL, XXXX',X'03'                                    
         DC    X'05',CL15'MAY, XXXX',X'03'                                      
         DC    X'06',CL15'JUNE, XXXX',X'03'                                     
         DC    X'07',CL15'JULY, XXXX',X'04'                                     
         DC    X'08',CL15'AUGUST, XXXX',X'04'                                   
         DC    X'09',CL15'SEPTEMBER, XXXX',X'04'                                
         DC    AL1(EOT)                                                         
*                                                                               
AMAINTAB DS    0F                                                               
         DC    CL8'**PROD** '                                                   
         DC    AL2(APRDBUFF-WORKD) STORED ADDR OF THE TABLE (APGD)              
         DC    AL2(PRDLNQ)         RECORD LENGTH                                
         DC    AL2(PRDKLNQ)        DISP IN REC/KEY LENGTH                       
         DC    AL2(PRDMAX)         MAX NUMBER OF TAB ENTRIES                    
         DC    AL1(0)              NUMBER OF BUCKETS                            
         DC    AL1(0)              DISP TO BUCKETS                              
         DC    AL4(PRDSIZE)        SIZE OF THE TABLE                            
*                                                                               
         DC    CL8'**APGN** '                                                   
         DC    AL2(APGBUFF-WORKD)  STORED ADDR OF THE TABLE (APGD)              
         DC    AL2(APGLEN)         RECORD LENGTH                                
         DC    AL2(APGKLEN)        DISP IN REC/KEY LENGTH                       
         DC    AL2(APGMAX)         MAX NUMBER OF TAB ENTRIES                    
         DC    AL1(0)              NUMBER OF BUCKETS                            
         DC    AL1(0)              DISP TO BUCKETS                              
         DC    AL4(APGSIZE)        SIZE OF THE TABLE                            
*                                                                               
         DC    CL8'**APGG** '                                                   
         DC    AL2(APGGBUF-WORKD)  STORED ADDR OF THE TABLE (APGGD)             
         DC    AL2(APGGLEN)        RECORD LENGTH                                
         DC    AL2(APGGLN)         DISP IN REC/KEY LENGTH                       
         DC    AL2(APGGMAX)        MAX NUMBER OF TAB ENTRIES                    
         DC    AL1(0)              NUMBER OF BUCKETS                            
         DC    AL1(0)              DISP TO BUCKETS                              
         DC    AL4(APGGSIZE)       SIZE OF THE TABLE                            
*                                                                               
         DC    CL8'**NAME** '                                                   
         DC    AL2(NMEBUFF-WORKD)  STORED ADDR OF THE TABLE (NMED)              
         DC    AL2(NMELEN)         RECORD LENGTH                                
         DC    AL2(NMEKLEN)        DISP IN REC/KEY LENGTH                       
         DC    AL2(NMEMAX)         MAX NUMBER OF TAB ENTRIES                    
         DC    AL1(0)              NUMBER OF BUCKETS                            
         DC    AL1(0)              DISP TO BUCKETS                              
         DC    AL4(NMESIZE)        SIZE OF THE TABLE                            
*                                                                               
         DC    CL8'**DUPL** '                                                   
         DC    AL2(DUPBUFF-WORKD)  STORED ADDR OF THE TABLE (DUPD)              
         DC    AL2(DUPLEN)         RECORD LENGTH                                
         DC    AL2(DUPKLEN)        DISP IN REC/KEY LENGTH                       
         DC    AL2(DUPMAX)         MAX NUMBER OF TAB ENTRIES                    
         DC    AL1(0)              NUMBER OF BUCKETS                            
         DC    AL1(0)              DISP TO BUCKETS                              
         DC    AL4(DUPSIZE)        SIZE OF THE TABLE                            
*                                                                               
         DC    CL8'**TIT*** '                                                   
         DC    AL2(TIBUFF-WORKD)   STORED ADDR OF THE TABLE (TITD)              
         DC    AL2(TILEN)          RECORD LENGTH                                
         DC    AL2(TIKLEN)         DISP IN REC/KEY LENGTH                       
         DC    AL2(TIMAX)          MAX NUMBER OF TAB ENTRIES                    
         DC    AL1(0)              NUMBER OF BUCKETS                            
         DC    AL1(0)              DISP TO BUCKETS                              
         DC    AL4(TISIZE)         SIZE OF THE TABLE                            
*                                                                               
         DC    CL8'**GRP*** '                                                   
         DC    AL2(AGRPBUFF-WORKD) STORED ADDR OF THE TABLE (GRPD)              
         DC    AL2(GRPLEN)         RECORD LENGTH                                
         DC    AL2(GRPKLEN)        DISP IN REC/KEY LENGTH                       
         DC    AL2(GRPMAX)         MAX NUMBER OF TAB ENTRIES                    
         DC    AL1(GRPNUMDT)       NUMBER OF BUCKETS                            
         DC    AL1(GRPBKDSP)       DISP TO BUCKETS                              
         DC    AL4(GRPSIZE)        SIZE OF THE TABLE                            
*                                                                               
MAINNUM  EQU   (*-AMAINTAB)/MAINLNQ NUMBER OF BINARY TABLES                     
*                                                                               
*                                                                               
*                                     **ACQUIRED WORK AREAS    **               
*                                     **NOTE: UPDATE LENSIZE!!!**               
AWORKTAB DS    0F                                                               
         DC    AL2(AAPGWK-WORKD),AL2(APGLEN)                                    
         DC    AL2(ASRTREC-WORKD),AL2(SRTLEN)                                   
         DC    AL2(AGRPREC-WORKD),AL2(SRTLEN)                                   
         DC    AL2(ADPTREC-WORKD),AL2(SRTLEN)                                   
         DC    AL2(ADUPREC-WORKD),AL2(SRTLEN*MAXDPTS)                           
         DC    AL2(AREQREC-WORKD),AL2(SRTLEN)                                   
         DC    AL2(AWRKREC-WORKD),AL2(SRTLEN)                                   
         DC    AL2(ABUFWRK-WORKD),AL2(BLEN)                                     
         DC    AL2(ATCAPWK-WORKD),AL2(TCAPLNQ)                                  
         DC    AL2(ARCAPWK-WORKD),AL2(TCAPLNQ)                                  
         DC    AL2(ARCAPTOT-WORKD),AL2(TCAPLNQ)                                 
         DC    AL2(ATCQWK-WORKD),AL2(TCLNQ)                                     
         DC    AL2(ATCQWK2-WORKD),AL2(TCLNQ)                                    
MWRKNUM  EQU   (*-AWORKTAB)/WORKLNQ NUMBER OF WORK AREAS                        
*    BINARY TABLE EQUATES                                                       
* **NOTE: UPDATE LENSIZE!!!**                                                   
*                                                                               
APGMAX   EQU   50                                                               
APGSIZE  EQU   L'MAINEYE+BINLENQ+(APGMAX*APGLEN)                                
PRDMAX   EQU   200                                                              
PRDSIZE  EQU   L'MAINEYE+BINLENQ+(PRDMAX*PRDLNQ)                                
APGGMAX  EQU   300                                                              
APGGSIZE EQU   L'MAINEYE+BINLENQ+(APGGMAX*APGLEN)                               
NMEMAX   EQU   5100                                                             
NMESIZE  EQU   L'MAINEYE+BINLENQ+(NMEMAX*NMELEN)                                
DUPMAX   EQU   2000                                                             
DUPSIZE  EQU   L'MAINEYE+BINLENQ+(DUPMAX*DUPLEN)                                
TIMAX    EQU   2000                                                             
TISIZE   EQU   L'MAINEYE+BINLENQ+(TIMAX*TILEN)                                  
GRPMAX   EQU   4000                                                             
GRPSIZE  EQU   L'MAINEYE+BINLENQ+(GRPMAX*GRPLEN)                                
         EJECT                                                                  
**********************************************************************          
* DEPARTMENT TOTALS                                                  *          
**********************************************************************          
         SPACE 1                                                                
DPTTOTS  DS    PL(DPTQBLN)                                                      
         ORG   DPTTOTS                                                          
DPT10    DS    PL(DPTQBLN)         4 QUARTERS AND TOTAL                         
         DS    PL(DPTQBLN)                                                      
         DS    PL(DPTQBLN)                                                      
         DS    PL(DPTQBLN)                                                      
         DS    PL(DPTQBLN)                                                      
DPT11    DS    PL(DPTQBLN)         4 QUARTERS AND TOTAL                         
         DS    PL(DPTQBLN)                                                      
         DS    PL(DPTQBLN)                                                      
         DS    PL(DPTQBLN)                                                      
         DS    PL(DPTQBLN)                                                      
DPT12    DS    PL(DPTQBLN)         4 QUARTERS AND TOTAL                         
         DS    PL(DPTQBLN)                                                      
         DS    PL(DPTQBLN)                                                      
         DS    PL(DPTQBLN)                                                      
         DS    PL(DPTQBLN)                                                      
DPT20    DS    PL(DPTQBLN)         4 QUARTERS AND TOTAL NEW INSERTION           
         DS    PL(DPTQBLN)                                                      
         DS    PL(DPTQBLN)                                                      
         DS    PL(DPTQBLN)                                                      
         DS    PL(DPTQBLN)                                                      
DPT25    DS    PL(DPTQBLN)         4 QUARTERS AND TOTAL NEW INSERTION           
         DS    PL(DPTQBLN)                                                      
         DS    PL(DPTQBLN)                                                      
         DS    PL(DPTQBLN)                                                      
         DS    PL(DPTQBLN)                                                      
DPT13    DS    PL(DPTQBLN)         4 QUARTERS AND TOTAL                         
         DS    PL(DPTQBLN)                                                      
         DS    PL(DPTQBLN)                                                      
         DS    PL(DPTQBLN)                                                      
         DS    PL(DPTQBLN)                                                      
DPT14    DS    PL(DPTQBLN)         4 QUARTERS AND TOTAL                         
         DS    PL(DPTQBLN)                                                      
         DS    PL(DPTQBLN)                                                      
         DS    PL(DPTQBLN)                                                      
         DS    PL(DPTQBLN)                                                      
DPT15    DS    PL(DPTQBLN)         4 QUARTERS AND TOTAL                         
         DS    PL(DPTQBLN)                                                      
         DS    PL(DPTQBLN)                                                      
         DS    PL(DPTQBLN)                                                      
         DS    PL(DPTQBLN)                                                      
DPTTOTN  EQU   (*-DPTTOTS)/(DPTQBLN)                                            
         EJECT                                                                  
*                                                                               
**********************************************************************          
* BUFFER                                                             *          
**********************************************************************          
         SPACE 1                                                                
LENBUFF  EQU   APGGSIZE+APGSIZE+NMESIZE+BUFSPACE+DUPSIZE+TISIZE+GRPSIZEX        
               +APGLEN+(4*SRTLEN)+BLEN+(3*TCAPLNQ)+(2*TCLNQ)+100                
         EJECT                                                                  
**********************************************************************          
* DSECT FOR WORKING STORAGE                                          *          
**********************************************************************          
         SPACE 1                                                                
WORKD    DSECT                                                                  
LOCAL    DS    0C                  BEGINNING OF LOCAL STORAGE                   
VTYPES   DS    0A                  EXTERNALS                                    
ACSLRY   DS    V                                                                
ACSALHST DS    V                                                                
COVAIL   DS    V                                                                
UNDERLIN DS    V                                                                
HELLO    DS    V                                                                
PRNTBL   DS    V                   PRINT DATA                                   
DLFLD    DS    V                   DOWNLOAD MODULE                              
*                                                                               
COMTAB   DS    A                   ADDR ON NMODS                                
SUMPST   DS    A                   POST PERSON TOTALS                           
DPTBUCK  DS    A                   DEPTS FOR RECAP REPORT FH LEDGER             
DPTBKFR  DS    A                   DEPTS FOR RECAP REPORT FR LEDGER             
QTRMTAB  DS    A                   TABLE OF MONTHS IN A QTR                     
MAINTAB  DS    A                   ADDR OF MAIN TABLE                           
WORKTAB  DS    A                   ADDR OF WORK AREA TABLE                      
COMMON   DS    A                   COMMON ROUTINES NMOD #1                      
COMMON2  DS    A                   COMMON ROUTINES NMOD #2                      
AIO      DS    A                   IO AREA                                      
AIO2     DS    A                   IO AREA 2                                    
DIO      DS    A                   DIRECTORY IO AREA                            
SALAREA  DS    A                   SALARY IO AREA (SALHST)                      
SALAREA2 DS    A                   SALARY IO AREA (ACSALARY)                    
ADUMP    DS    A                   PRINTABLE ROUTINE                            
ADWNL    DS    A                   DOWNLOAD                                     
ADWNRTE  DS    A                   DOWNLOAD ROUTINE                             
ADWNBUF  DS    A                   DOWNLOAD BUFFER                              
ANFISTAB DS    A                   FISCAL YEAR TABLE                            
ADPTTOTS DS    A                   STORAGE DEPARTMENT TOTALS                    
VTYPLNQ  EQU   *-VTYPES                                                         
*                                  BINARY TABLE(GETMAIN)                        
ABUFF    DS    A                   START OF GETMAIN STORAGE                     
APRDBUFF DS    A                   LEXUS PRODUCT TABLE                          
APGBUFF  DS    A                   ADDR OF APG ACCOUNT NAMES TABLE              
APGGBUF  DS    A                   ADDR OF APG GIVING ACCOUNT TABLE             
NMEBUFF  DS    A                   ADDR OF 1R NAME TABLE                        
DUPBUFF  DS    A                   ADDR OF 1R DUPLICATE LOCATION TABLE          
TIBUFF   DS    A                   ADDR OF TITLE TABLE                          
AGRPBUFF DS    A                   ADDR OF GROUP TABLE                          
*                                                                               
COM1     DS    0A                  *** COMMON NMOD # 1 ***                      
MAIN     DS    V                   GETMAIN WORK AREA AND STORAGE                
WRAP     DS    V                   FREEMAIN WORK AREA AND STORAGE               
SUPBLD   DS    V                   BUILD SUPERLEDGER TABLE                      
GETNAME  DS    V                   GET NAME FROM ACCOUNT OR CONTRA HED          
BINADD   DS    V                   BUILD BINARY TABLES                          
BINSRC   DS    V                   SEARCH BINARY TABLES                         
CLRSORT  DS    V                   CLEAR SORT AREA                              
RESERVE  DS    7V                                                               
COMSPARE EQU (*-RESERVE)/L'COM1    NUMBER OF SPARE ADDRESSES                    
COMNUM1  EQU   (*-COM1)/L'COM1     NUMBER OF ROUTINES                           
*                                                                               
COM2     DS    0A                  *** COMMON NMOD # 2 ***                      
REPORTS  DS    V                   DO THE REPORTS                               
RESERVE2 DS    7V                                                               
COMSPAR2 EQU (*-RESERVE)/L'COM2    NUMBER OF SPARE ADDRESSES                    
COMNUM2  EQU   (*-COM2)/L'COM2     NUMBER OF ROUTINES                           
*                                                                               
* STORAGE AND TABLES                                                            
*                                                                               
ADBUFF   DS    A                   BUFFALO                                      
AAPGWK   DS    A                   APG WORK AREA                                
ASRTREC  DS    A                   SORT RECORD                                  
AGRPREC  DS    A                   GROUP RECORD                                 
ADPTREC  DS    A                   DEPT TOTAL RECORD                            
ADUPREC  DS    A                   DEPT TOTAL RECORD FOR DUPREP                 
AREQREC  DS    A                   REQUEST TOTAL RECORD                         
AWRKREC  DS    A                   WORK SORT RECORD (FOR CREATIVE)              
ABUFWRK  DS    A                   BUFFALO RECORD WORK AREA                     
ATCAPWK  DS    A                                                                
ARCAPWK  DS    A                                                                
ARCAPTOT DS    A                                                                
ATCQWK   DS    A                                                                
ATCQWK2  DS    A                                                                
*                                                                               
ADDRSAV  DS    A                   GENERAL SAVE ADDR                            
SAVERE   DS    A                   SAVE RE ADDR                                 
SAVSRT   DS    A                   SAVE ADDR OF SRTREC                          
*                                                                               
*                                                                               
LITVALS  DS    0X                  LITERAL VALUES                               
ZEROS    DS    PL1                 P'0'                                         
NONCLI   DS    CL2                 C'1N'                                        
CLILEG   DS    CL2                 C'1C'                                        
EMPLEDG  DS    CL2                 C'1R'                                        
DIR      DS    CL6                 C'ACCDIR'                                    
MST      DS    CL6                 C'ACCMST'                                    
GETREC   DS    CL8                 C'GETREC  '                                  
ADDREC   DS    CL8                 C'ADDREC  '                                  
PUTREC   DS    CL8                 C'PUTREC  '                                  
FOXES    DS    XL12                12X'FF'                                      
SETC     DS    CL5                 C'SET  '                                     
PUTC     DS    CL5                 C'PUT  '                                     
ADDC     DS    CL5                 C'ADD  '                                     
GETC     DS    CL5                 C'GET  '                                     
SEQC     DS    CL5                 C'SEQ  '                                     
ENDC     DS    CL5                 C'END  '                                     
HIGHC    DS    CL5                 C'HIGH '                                     
CLEARC   DS    CL5                 C'CLEAR'                                     
LITVALSL EQU   *-LITVALS                                                        
*                                                                               
LLEVELS  EQU   *                                                                
LLEVA    DS    CL(L'ACLVLEN)         1R LEV A LENGTH                            
LLEVANAM DS    CL(L'ACLVDESC)        1R LEV A NAME                              
LLEVALN  EQU   *-LLEVELS                                                        
LLEVB    DS    CL(L'ACLVLEN)         1R LEV B LENGTH (A+B)                      
LLEVBNAM DS    CL(L'ACLVDESC)        1R LEV B NAME                              
LLEVC    DS    CL(L'ACLVLEN)         1R LEV C LENGTH (A+B+C)                    
LLEVCNAM DS    CL(L'ACLVDESC)        1R LEV C NAME                              
LLEVD    DS    CL(L'ACLVLEN)         1R LEV D LENGTH (A+B+C+D)                  
LLEVDNAM DS    CL(L'ACLVDESC)        1R LEV D NAME                              
LLEVELLN EQU   *-LLEVELS                                                        
LLEVLNUM EQU   LLEVELLN/LLEVALN                                                 
*                                                                               
LENLEVLS EQU   *                                                                
LENLEVA  DS    XL1                 REAL LENGTH OF LEVEL A                       
LENLEVB  DS    XL1                 REAL LENGTH OF LEVEL B                       
LENLEVC  DS    XL1                 REAL LENGTH OF LEVEL C                       
LENLEVD  DS    XL1                 REAL LENGTH OF LEVEL D                       
LENLEVLN EQU   *-LENLEVLS                                                       
LENLVNUM EQU   LENLEVLN/L'LENLEVA                                               
*                                                                               
ACCSTUS  DS    XL1                 CURRENT ACCOUNT STATUS                       
WANTIT   EQU   X'80'               WE WANT THIS CONTRA ACCOUNT                  
REQCLI   EQU   X'40'               THE REQUESTED CLIENT                         
ACTIVE   EQU   X'20'               THIS PERSON ACTIVE IN REQUEST PERIOD         
REQLDA   EQU   X'10'               CURRENT PRODUCT IS LDA CREATIVE              
REQCLI1  EQU   X'08'               THE REQUESTED CLIENT AT LEAST ONCE           
DUPLOCAT EQU   X'02'               DUPLICATE LOCATION                           
REQPRD   EQU   X'01'               THE REQUESTED PRODUCT                        
*                                                                               
FLAG     DS    XL1                 FLAG FOR MNTHLY ADJ TABLE                    
FLGPER   EQU   X'80'               DON'T CLEAR TABLE - HAVE A PERSON            
FLGADJ   EQU   X'40'               MARK AS ADJUSTED                             
FLGSAL   EQU   X'20'               MARK AS SALARY WAS LOOKED UP                 
FLGGRP   EQU   X'10'               SET FLAG TO SHOW GROUP REPORT                
FLGNONME EQU   X'08'               NO NAME FLAG                                 
FLGLDA   EQU   X'04'               CURRENTLY DOING GRP REP AND LDA              
FLGYTD   EQU   X'02'               INTERNAL YTD IN USE                          
FLGICLI  EQU   X'01'               INVALID CLIENT                               
*                                                                               
FLAG2    DS    XL1                 FLAG2                                        
FLGNEG   EQU   X'80'               PERSON HAD SALARY ADJUSTED BACKWARDS         
FLGFH    EQU   X'40'               READING FH HISTORIC SUPERLEDGER              
*                                                                               
REPBYTE  DS    XL1                 BYTE FOR DOWNLOAD ROUTINE                    
REPA     EQU   X'80'               PERSON     REPORT FOR DOWNLOAD               
REPB     EQU   X'40'               MULTI      REPORT FOR DOWNLOAD               
REPC     EQU   X'20'               GRP PERSON REPORT FOR DOWNLOAD               
REPD     EQU   X'10'               GROUP RECAP                                  
REPE     EQU   X'08'               GROUP ANALYSIS                               
REPF     EQU   X'04'               RECAP REPORT                                 
REPG     EQU   X'02'               ANALYSIS REPORT                              
REPBYTE1 DS    XL1                 BYTE1 FOR DOWNLOAD ROUTINE                   
REPDWN   EQU   X'80'               DOWNLOAD IS ACTIVE                           
*                                                                               
MSGS     DS    CL35                                                             
MSG1     DS    CL10                                                             
*                                                                               
LSTGRP   DS    CL2                 SAVED AREA FOR PREVIOUS GROUP                
SVGRP    DS    CL2                 SAVED AREA FOR GRP FROM PROD TABLE           
SVGRPNM  DS    CL20                SAVED AREA FOR GRP NAME FROM PRD TAB         
SVPRDNM  DS    CL36                SAVED AREA FOR PROD NAME-PROD REQ            
SVPERSON DS    CL36                                                             
SVSTAT   DS    XL1                 SAVE CURRENT ACCOUNT STATUS                  
SVPERCD  DS    CL6                 SAVED AREA FOR 1R PERSON                     
ACTIVITY DS    CL1                 SORT ACTIVITY? Y OR N                        
OFFLN    DS    XL1                 OFFICE LENGTH 1 OR 2 BYTES                   
COMMAND  DS    CL8                                                              
DKEY     DS    CL50                DIRECTORY KEY                                
DIRAREA  DS    CL64                DIRECTORY RECORD                             
DA       DS    F                                                                
CUL      DS    CL3                 COMPANY UNIT LEDGER                          
SVKEY    DS    CL50                KEY SAVE AREA                                
SVBUFKEY DS    CL(BUFKLEN)         KEY SAVE AREA                                
PACK16   DS    PL16                PACKED WORK AREA                             
PACK8    DS    PL8                 PACKED WORK AREA                             
MINUS1   DS    PL8                 YTD-1 FOR CALC                               
COUNT    DS    PL2                 GENERAL COUNTER                              
LEVCNT   DS    XL1                 LEVEL COUNTER FOR DOWNLOAD                   
SVRPT    DS    XL1                                                              
SVCNT    DS    XL1                                                              
*                                                                               
METHOD   DS    CL1                 COSTING METHOD (DEFAULT IS 1)                
*                                                                               
RQDETAIL DS    0CL16                                                            
RQOFFICE DS    CL2                 REQUESTED OFFICE                             
RQCLIPRD DS    0CL6                REQUESTED CLIENT AND PRODUCT                 
RQCLIENT DS    CL3                 REQUESTED CLIENT                             
RQPROD   DS    CL3                 REQUESTED PRODUCT                            
RQDEPT   DS    CL2                 REQUESTED APG DEPT                           
RQGRP    DS    CL6                 REQUESTED GROUP                              
MASK     DS    XL1                                                              
*                                                                               
SAVELEVS DS    0C                                                               
APGDPT   DS    CL8                 APG DEPT                                     
APGNAME  DS    CL36                APG DEPT NAME                                
DPTCODE  DS    CL4                 COMBINED OFF/DPT                             
DPTNAME  DS    CL36                DEPARTMENT NAME                              
SUBCODE  DS    CL2                 SUBDEPT TITLECODE                            
SUBCODEC DS    CL6                 OFF/DEPT/SUB CODE                            
SUBNAME  DS    CL36                SUBDEPT NAME                                 
PRSCODE  DS    CL6                 PERSON CODE                                  
PRSCODEC DS    CL12                OFF/DPT/SUB/PERSON                           
PRSNAME  DS    CL36                PERSON NAME                                  
SAVLVLN  EQU   *-SAVELEVS                                                       
*                                                                               
PRDWRK   DS    CL(PRDLNQ)          WORK AREA FOR PROD TABLE                     
GRPWRK   DS    CL(GRPLEN)          WORK AREA FOR GROUP TABLE                    
*                                                                               
OFFIC    DS    CL6                 OFFICE CODE                                  
OFFICN   DS    CL36                OFFICE NAME                                  
DEPART   DS    CL6                 DEPARTMENT CODE                              
*                                                                               
YTDQSTR  DS    CL6                 YTD QSTART                                   
FISCSTR  DS    PL2                 FISCAL YEAR START DATE                       
FISCEND  DS    PL2                 FISCAL YEAR END   DATE                       
SVSTART  DS    PL2                 INTERNAL YTD PACKED START DATE               
START    DS    CL2                 START DATE YYMM(PACKED)                      
END      DS    CL2                 END DATE YYMM(PACKED)                        
SRQMNTHS DS    XL1                 NUMBER OF MONTHS IN REQUEST                  
RQMNTHS  DS    XL1                 NUMBER OF MONTHS IN REQUEST                  
MNTHS    DS    CL(MTHLEN*MAXMTHS)  START THRU END YYMM (PACKED)                 
MNTHSLNQ EQU   *-RQMNTHS                                                        
SVMNTHS  DS    CL(MTHLEN*MAXMTHS)  START THRU END YYMM (PACKED)                 
ADJHOURS DS    PL6                 P'180000' OR P'090000'                       
MANHOURS DS    PL6                 P'154000' OR P'077000' MANYR EQUIV           
*                                  OR P'161000'                                 
*                                                                               
SVQTR    DS    CL1                                                              
TOTSW    DS    CL1                                                              
*                                                                               
ELIST    DS    3F                  FOR GETL, ADDEL, DELEL                       
ELERR    DS    CL1                                                              
         ORG   ELERR               ERROR CODE FROM HELLO                        
ELADDR   DS    F                   ADDRESS OF ELEMENT FROM HELLO                
         DS    2F                                                               
*                                                                               
SVDPT    DS    CL2                 SAVED AREA FOR DEPARTMENT                    
SVAPGC   DS    0CL4                                                             
SVAPGDPT DS    CL2                                                              
SVAPGSUB DS    CL29                SAME LENGTH AS TCQSUB FIELD                  
SVSUBNME DS    CL36                                                             
SVDPTNME DS    CL36                                                             
*                                                                               
EFFDTNUM DS    XL1                 CLI EFFECTIVE DATE TABLE ENTRY NUM           
EFFDTTAB DS    20PL(EFFDTLNQ)      CLI EFFECTIVE DATE TABLE                     
EFFDT1LN EQU   *-EFFDTTAB                                                       
*                                                                               
ADJTAB   DS    (MAXMTHS*ADJLNQ)C                                                
*                                                                               
SVSRTDTL DS    (MAXMTHS*SDDTLQ)C   SAVED AREA FOR SORT DTL TOTS PRE             
SVSRTTOT DS    (SDDTLQ)C           SAVED AREA FOR SORT TOTS PRE                 
*                                                                               
DWNFLD   DS    CL36                SAVED AREA FOR FIELD TO BE DWNLOADED         
*                                                                               
PRTSIZE  DS    XL1                 PRINT AREA LENGTH                            
*                                                                               
HDLINE1  DS    0CL20               HEADLINE # 1 INFO (FORMAT)                   
HDLNFRM  DS    CL1                 FORMAT OF REPORT - QOPT1                     
         DS    CL3                 SPACE                                        
HDLNDSC  DS    CL16                FORMAT DESCRIPTION                           
HDLINE2  DS    CL30                HEADLINE # 2 INFO DATES                      
HDLINE3  DS    CL25                HEADLINE # 3 INFO (LEDGER BREAKDOWN)         
*                                                                               
DWNSTAT  DS    XL1                 DOWNLOAD STATUS                              
DWNINTZ  EQU   X'80'               DOWNLOAD INITIALIZED                         
DWNHDLN  EQU   X'40'               DOWNLOAD HEADLINES                           
*                                                                               
DWNMODE  DS    XL1                 DOWNLOAD MODE                                
DWNINIT  EQU   1                      DOWN-LOAD INITIALIZATION                  
DWNEOL   EQU   2                      MARK END OF LINE                          
DWNEOR   EQU   3                      MARK END OF REPORT                        
DWNTEXT  EQU   4                      DOWN-LOAD TEXT                            
DWNNUM   EQU   5                      DOWN-LOAD NUMBER                          
DWNPACK  EQU   6                      DOWN-LOAD NUMBER (PACKED)                 
*                                                                               
SVSUBPRG DS    CL1                   SAVED AREA FOR DOWNLOADING                 
SVMNTH   DS    CL15                  SAVED AREA FOR MONTH                       
SVPRNT   DS    CL(L'XP)              SAVED AREA FOR PRINT LINE                  
SVPRNT2  DS    CL(L'XP)              SAVED AREA FOR PRINT LINE 2                
SVPRNT3  DS    CL(L'XP)              SAVED AREA FOR PRINT LINE 3                
SVPRNT4  DS    CL(L'XP)              SAVED AREA FOR PRINT LINE 4                
SVPRNLNQ EQU   *-SVPRNT                                                         
         EJECT                                                                  
**********************************************************************          
* CLIENT EFFECTIVE DATE TABLE ENTERED THROUGH THE PE SCREEN IN $FIL  *          
**********************************************************************          
         SPACE 1                                                                
EFFDTD   DSECT                                                                  
EFFDTSTR DS    PL2                 EFFECTIVE START DATE                         
EFFDTEND DS    PL2                 EFFECTIVE END DATE                           
EFFDTLNQ EQU   *-EFFDTD                                                         
         EJECT                                                                  
**********************************************************************          
* NEW FISCAL YEAR TABLE DSECT  COVERS  NFISTAB                       *          
**********************************************************************          
         SPACE 1                                                                
NFISTABD DSECT                                                                  
NFSTART  DS    0XL2                FISCAL START DATE ADJUSTED                   
NFSTRYR  DS    XL1                 FISCAL START YEAR X'YY'                      
NFSTRMM  DS    XL1                 FISCAL START MONTH X'MM'                     
NFEND    DS    0XL2                FISCAL END DATE ADJUSTED                     
NFENDYR  DS    XL1                 FISCAL END YEAR X'YY'                        
NFENDMM  DS    XL1                 FISCAL END MONTH X'MM'                       
NFHOURS  DS    PL6                 1800 OR 900                                  
NFMANHR  DS    PL6                 P'154000' 0R P'077000' MANYEAR HRS           
NFSUPLDG DS    CL1                 HISTORIC OR CURRENT SUPERLEDGER              
NFISTABQ EQU   *-NFISTABD                                                       
         EJECT                                                                  
**********************************************************************          
* CODADR TABLE                                                       *          
**********************************************************************          
         SPACE 1                                                                
CODAD    DSECT                                                                  
CODDSP   DS    AL2                 DISPLACEMENT TO FLD IN STORAGE               
CODLEN   DS    XL1                 LENGTH OF FIELD                              
CODLNQ   EQU   *-CODAD                                                          
         EJECT                                                                  
**********************************************************************          
* LEXUS/LDA PRODUCT TABLE DSECT                                      *          
**********************************************************************          
         SPACE 1                                                                
PRDTBD   DSECT                                                                  
PRDGRP   DS    CL2                 GROUP CODE                                   
PRDCP    DS    0CL6                CLI/PRD                                      
PRDCLI   DS    CL3                    CLIENT CODE                               
PRDPRD   DS    CL3                    PRODUCT CODE                              
PRDKLNQ  EQU   *-PRDTBD            KEY LENGTH                                   
PRDGRPNM DS    CL20                GROUP NAME                                   
PRDLNQ   EQU   *-PRDTBD                                                         
         EJECT                                                                  
**********************************************************************          
* BIN DSECT                                                          *          
**********************************************************************          
         SPACE 1                                                                
BIND     DSECT                                                                  
BININ    DS    F                   NUMBER IN TABLE                              
BINLEN   DS    F                   RECORD LENGTH                                
BINDISPK DS    F                   KEY LENGTH                                   
BINMAX   DS    F                   MAXIMUM NUMBER IN TABLE                      
BINKLEN  EQU   *-BIND                                                           
BINNUMB  DS    CL1                 NUMBER OF BUCKETS                            
BINFRST  DS    CL1                 DISP. TO FIRST BUCKET                        
BINLENQ  EQU   *-BIND                                                           
BINTABLE DS    0CL1                                                             
*                                                                               
* DSECT FOR MAINTAB FOR ACQUIRED STORAGE (GETMAIN)                              
*                                                                               
MAIND    DSECT                                                                  
MAINEYE  DS    CL8                 TABLE EYE CATCHER IN DUMP                    
MAINAST  DS    AL2                 ADDR TO STORE A(TABLE)                       
MAINLEN  DS    AL2                 RECORD LENGTH                                
MAINDISK DS    AL2                 DISP/KEY LENGTH                              
MAINMAX  DS    AL2                 MAXIMUM NUMBER IN TABLE                      
MAINNUMB DS    AL1                 NUMBER OF BUCKETS                            
MAINFRST DS    AL1                 DISP TO FIRST BUCKET                         
MAINSIZE DS    AL4                 TABLE SIZE                                   
MAINLNQ  EQU   *-MAIND                                                          
*                                                                               
* DSECT FOR ADDITIONAL WORK FROM ACQUIRED STORAGE(GETMAIN)                      
*                                                                               
WORKMD   DSECT                                                                  
WORKAST  DS    AL2                 ADDR TO STORE A(WORK AREA)                   
WORKSIZE DS    AL2                 WORK AREA SIZE                               
WORKLNQ  EQU   *-WORKMD                                                         
         EJECT                                                                  
**********************************************************************          
* DSECT FOR COMMON WORK NMOD  ENTRIES                                *          
**********************************************************************          
         SPACE 1                                                                
COMD     DSECT                                                                  
COMRELO  DS    AL2                 RELOCATED ADDR OF COMMON NMOD                
COMENT   DS    AL2                 ADDR OF FIRST ENTERABLE ROUTINE              
COMNUMB  DS    AL1                 NUMBER OF ENTERALBE ROUTINES IN NMOD         
COMLEN   EQU   *-COMD                                                           
*                                                                               
* DSECT FOR PERSON TOTALS                                                       
*                                                                               
SUMPD    DSECT                                                                  
SUMPDISP DS    Y                   DISP TO ACCUM                                
SUMPSTAT DS    XL1                 STATUS                                       
SUMPADD  EQU   X'80'               ADD TO                                       
SUMPZAP  EQU   X'40'               ZAP INTO                                     
         DS    XL1                 SPARE                                        
SUMPBDIS DS    Y                   DISP TO BUFFALO ACCUM                        
SUMPLN   EQU   *-SUMPD                                                          
*                                                                               
* DSECT FOR DEPT ACCUMS                                                         
*                                                                               
DPTAD    DSECT                                                                  
DPTACCUM DS    PL(DPTQBLN)         4 QUARTERS AND TOTAL                         
*                                                                               
* DSECT FOR DEPT TABLE FOR RECAP                                                
*                                                                               
DPTD     DSECT                                                                  
DPTNUMB  DS    CL2                 APG DEPT CODE                                
DPTHDISP DS    Y                   DISP TO HOUR ACCUM                           
DPTCDISP DS    Y                   DISP TO COST ACCUM                           
DPTTDISP DS    Y                   DISP TO DEPT COST TOTAL ACCUM                
DPTDLN   EQU   *-DPTD                                                           
*                                                                               
* DSECT FOR QTR TABLE FOR RECAP                                                 
*                                                                               
QTRD     DSECT                                                                  
QTRMTH   DS    XL1                 MONTH FOR COMPARE                            
QTRMNAME DS    CL15                MONTH NAME,19XX                              
QTRNUMB  DS    XL1                 QTR NUMBER                                   
QTRDLN   EQU   *-QTRD                                                           
*                                                                               
* DSECT FOR SORT TO BUFFALO                                                     
*                                                                               
BFUPD    DSECT                                                                  
BFUPSORT DS    Y                   DISP TO SORT ACCUM                           
BFUPBUFF DS    Y                   DISP TO BUFF ACCUM                           
BFUPLN   EQU   *-BFUPD                                                          
         EJECT                                                                  
**********************************************************************          
* DSECT FOR THE SORT RECORD                                          *          
**********************************************************************          
         SPACE 1                                                                
SRTD     DSECT                                                                  
SRACC    DS    0C                                                               
SRDEPT   DS    CL2                 APG DEPT                                     
SRACCT   DS    0C                                                               
SROFFICE DS    CL4                 1R OFFICE DEPT                               
SRTITLE  DS    CL2                 1R TITLE                                     
SRPERSON DS    CL6                 1R PERSON                                    
SRAKLNQ  EQU   *-SRACCT                                                         
SRKLNQ   EQU   *-SRACC                                                          
*                                                                               
SRSTAT   DS    XL1                 STATUS BYTE FOR ADJUSTMENTS                  
SRHRS    EQU   X'80'               PERSON REACHED 1800 HOURS (PRD TIME)         
SRSPACE  EQU   X'40'               KEY IS SPACE PADED DON'T USE                 
SRADJ    EQU   X'20'               PERSON WAS ADJUSTED                          
*                                                                               
SR1R     DS    CL15                ACCOUNT CODE COMPANY/1R...                   
SRDLEN   EQU   *-SRTD              DATA LENGTH                                  
*                                                                               
SRBUD    DS    PL6                 BUDGET PCT                                   
         ORG   SRBUD                                                            
SRYTPHR  DS    PL(SDBUKLN)         YTD ACTUAL PRODUCT HOURS                     
SRDTL    DS    (MAXMTHS*SDLEQ)C    DETAIL THIS CLIENT BY MONTH                  
SRTOTAL  DS    (SDLEQ)C            TOTALS                                       
SRTLEN   EQU   *-SRTD              RECORD LENGTH                                
         EJECT                                                                  
**********************************************************************          
* MINI DSECT ONE PER MONTH                                           *          
**********************************************************************          
         SPACE 1                                                                
SDTD     DSECT                                                                  
SDMNTH   DS    CL2                 YEAR/MONTH(PACKED)                           
SDMNUM   DS    XL1                 MONTH NUMBER                                 
SDSTAT   DS    XL1                 MONTH STATUS                                 
SDADJUST EQU   X'80'               ADJUSTED MONTH                               
SDKEYLN  EQU   *-SDMNTH                                                         
SDBUK    DS    PL6                                                              
SDBUKLN  EQU   *-SDBUK                                                          
         ORG   SDBUK                                                            
SDSAL    DS    PL(SDBUKLN)         MONTHLY SALARY                               
SDSALY   DS    PL(SDBUKLN)         YTD SALARY                                   
SDPRDHR  DS    PL(SDBUKLN)         HOURS WORKED ON PRODUCT                      
SDCLIHR  DS    PL(SDBUKLN)         HOURS WORKED ON CLIENT(TOYOTA/LEX)           
SDTHRS   DS    PL(SDBUKLN)         TOTAL HOURS WORKED                           
SDYPHRS  DS    PL(SDBUKLN)         YTD HOURS WORKED ON PRODUCT                  
SDYCHRS  DS    PL(SDBUKLN)         YTD HOURS WORKED ON CLIENT                   
SDYTHRS  DS    PL(SDBUKLN)         YTD TOTAL HOURS WORKED                       
SDYPCT   DS    PL(SDBUKLN)         PERCENT TOYOTA TO TOTAL                      
SDHRRAT  DS    PL(SDBUKLN)         HOURLY RATE                                  
SDPST    DS    PL(SDBUKLN)         MONTHLY POSTED TO TOYOTA CLIENT              
SDYPST   DS    PL(SDBUKLN)         YTD POSTED TO TOYOTA CLIENT                  
SDFACA   DS    PL(SDBUKLN)         FACTOR A                                     
         ORG   SDFACA                                                           
SDTPST   DS    PL(SDBUKLN)         YTD TOTAL POSTED                             
SDTOTAL  DS    PL(SDBUKLN)         TOTAL CHARGE                                 
SDYTCHR  DS    PL(SDBUKLN)         YTD ACTUAL CLIENT HOURS                      
SDNUMBK  EQU   (*-SDBUK)/(SDBUKLN) NUMBER OF BUCKETS                            
SDDTLQ   EQU   *-SDBUK                                                          
SDLEQ    EQU   *-SDTD              LENGTH OF MINI EL                            
         EJECT                                                                  
**********************************************************************          
* DSECT TO MONTHLY ADJUSTMENT TABLE                                  *          
**********************************************************************          
         SPACE 1                                                                
ADJD     DSECT                                                                  
ADJMNTH  DS    CL2                 YEAR/MONTH(PACKED)                           
ADJLOC   DS    0CL8                MONTHLY DPT/OFF/TITLE/GRP                    
ADJMNDPT DS    CL2                 MONTHLY DEPARTMENET                          
ADJMNOFF DS    CL4                 MONTHLY 1R OFFICE DEPT                       
ADJMNTLE DS    CL2                 MONTHLY 1R TITLE                             
ADJMNGRP DS    CL2                 MONTHLY GROUP                                
ADJLNQ   EQU   *-ADJD                                                           
         EJECT                                                                  
**********************************************************************          
* DSECT TO MONTH TABLE                                               *          
**********************************************************************          
         SPACE 1                                                                
MTHD     DSECT                                                                  
MTHCODE  DS    0XL2                YYMM                                         
MTHYEAR  DS    XL1                 YY                                           
MTHMNTH  DS    XL1                 MM                                           
MTHNUM   DS    XL1                 MONTH NUMBER                                 
MTHHRS   DS    PL3                 PERSONS HOURS IN THE MONTH                   
MTHLEN   EQU   *-MTHD                                                           
*                                                                               
* DSECT TO COVER SUPERLEDGER NAMES                                              
*                                                                               
APGD     DSECT                                                                  
APGREC   DS    CL12                APG ACCOUNT                                  
APGKLEN  EQU   *-APGD                                                           
APGNME   DS    CL36                NAME                                         
APGLEN   EQU   *-APGD                                                           
*                                                                               
* DSECT TO COVER SUPERLEDGER RULES                                              
*                                                                               
APGGD    DSECT                                                                  
APGGIV   DS    CL14                GIVING CODE                                  
APGGLN   EQU   *-APGGD                                                          
APGGREC  DS    CL12                RECEIVING CODE                               
APGGLEN  EQU   *-APGGD                                                          
*                                                                               
* DSECT TO COVER NAME TABLE                                                     
*                                                                               
NMED     DSECT                                                                  
NMEREC   DS    CL12                1R CODE                                      
NMEKLEN  EQU   *-NMED                                                           
NMENAME  DS    CL36                NAME                                         
NMELEN   EQU   *-NMED                                                           
*                                                                               
* DSECT TO COVER DUPLICATE LOCATION TABLE                                       
*                                                                               
DUPD     DSECT                                                                  
DUPPER   DS    CL6                 PERSON CODE                                  
DUPKLEN  EQU   *-DUPD                                                           
DUPSTAT  DS    XL1                 STATUS                                       
DUPLOC   EQU   X'80'               PERSON WORKED IN MORE THAN 1 LOC             
DUPLEN   EQU   *-DUPD                                                           
*                                                                               
* DSECT TO COVER PERSON NAME TABLE                                              
*                                                                               
TITD     DSECT                                                                  
TIPREC   DS    CL6                 PERSON CODE                                  
TIKLEN   EQU   *-TITD                                                           
TINAME   DS    CL36                NAME                                         
TILEN    EQU   *-TITD                                                           
         EJECT                                                                  
**********************************************************************          
* DSECT FOR A GROUP TABLE                                            *          
**********************************************************************          
         SPACE 1                                                                
GRPD     DSECT                                                                  
GRPKEY   DS    0C                                                               
GRPGRP   DS    CL2                 GROUP CODE                                   
GRPLOC   DS    0CL14               LOCATION                                     
GRPDPT   DS    CL2                 APG DEPT                                     
GRPOFF   DS    CL4                 1R OFFICE DEPT                               
GRPTITLE DS    CL2                 1R TITLE                                     
GRPPERS  DS    CL6                 1R PERSON                                    
GRPKLEN  EQU   *-GRPKEY                                                         
GRPSTAT  DS    XL1                 STATUS BYTE (SEE SRHRS-X'80')                
GRPDUP   EQU   X'08'               PERSON IN MULTIPLE GROUPS                    
GRPDUPLC EQU   X'04'               PERSON IN MULTIPLE LOCATIONS                 
GRPBKDSP EQU   *-GRPKEY                                                         
GRPBUK   DS    0C                                                               
GRPYTPHR DS    PL(SDBUKLN)         YTD ACTUAL PRODUCT HOURS                     
GRPTPST  DS    PL(SDBUKLN)         YTD ACTUAL TOTAL POSTED                      
GRPDTL   DS    (MAXMTHS*GRPDTLQ)C  DETAIL THIS CLIENT BY MONTH                  
GRPNUMDT EQU   (*-GRPDTL)/SDBUKLN                                               
GRPNUMBK EQU   (*-GRPBUK)/SDBUKLN                                               
GRPLEN   EQU   *-GRPD              RECORD LENGTH                                
         EJECT                                                                  
**********************************************************************          
* DSECT FOR A GROUP RECORD MONTHLY DETAIL BUCKETS                    *          
**********************************************************************          
         SPACE 1                                                                
GRPDTD   DSECT                                                                  
GRPSAL   DS    PL(SDBUKLN)         MONTHLY SALARY                               
GRPSALY  DS    PL(SDBUKLN)         YTD SALARY                                   
GRPPRDHR DS    PL(SDBUKLN)         HOURS WORKED ON PRODUCT                      
GRPCLIHR DS    PL(SDBUKLN)         HOURS WORKED ON CLIENT(TOYOTA/LEX)           
GRPTHRS  DS    PL(SDBUKLN)         TOTAL HOURS WORKED                           
GRPYPHRS DS    PL(SDBUKLN)         YTD HOURS WORKED ON PRODUCT                  
GRPYCHRS DS    PL(SDBUKLN)         YTD HOURS WORKED ON CLIENT                   
GRPYTHRS DS    PL(SDBUKLN)         YTD TOTAL HOURS WORKED                       
GRPDTLQ1 EQU   (*-GRPDTD)/SDBUKLN  BUCKETS THAT MATCH POSITION W/SDTD           
GRPLDAHR DS    PL(SDBUKLN)         HOURS WORKED ON LDA CRATIVE                  
GRPYLHRS DS    PL(SDBUKLN)         YTD HOURS WORKED ON LDA CREATIVE             
GRPPST   DS    PL(SDBUKLN)         MONTHLY POSTED TO TOYOTA CLIENT              
GRPYPST  DS    PL(SDBUKLN)         YTD POSTED TO TOYOTA CLIENT                  
GRPYTCHR DS    PL(SDBUKLN)         YTD ACTUAL CLIENT HOURSS                     
GRPDTLQ  EQU   *-GRPDTD                                                         
         EJECT                                                                  
**********************************************************************          
* DSECT FOR A WORK (BUFFALO) RECORD TYPE 2                           *          
*       3 BUFFALO LEVELS                                             *          
*       LEVEL 1 = PERSON                                             *          
*       LEVEL 2 = DEPARTMENT                                         *          
*       LEVEL 3 = REQUEST                                            *          
**********************************************************************          
         SPACE 1                                                                
BUFD     DSECT                                                                  
BUFKEY   DS    0CL40                                                            
BUFTYPE  DS    CL1                                                              
BUFDUP   EQU   1                   DUP LOCATION TYPE                            
BUFPERS  DS    CL6                 1R PERSON ORDER                              
BUFLOCA  DS    0C                                                               
BUFDEPT  DS    CL2                 APG DEPT                                     
BUFRLOC  DS    0C                  REAL LOCATION                                
BUFOFDPT DS    CL4                 1R OFFICE DEPT                               
BUFTITLE DS    CL2                 1R TITLE                                     
BUFLOCLN EQU   *-BUFLOCA                                                        
BUFRLLEN EQU   *-BUFRLOC                                                        
BUFSKLEN EQU   *-BUFKEY            SUB KEY LENGTH                               
BUFLINE  DS    CL1                 LINE TYPES                                   
BUFDETL  EQU   1                   1 = DETAIL(MONTH) LINE                       
BUFTOTL  EQU   2                   2 = TOTAL                                    
BUFMNTH  DS    CL2                 YEAR/MONTH PACKED                            
         DS    CL24                SPARE                                        
BUFKLEN  EQU   *-BUFD                                                           
*                                                                               
BUFBK    DS    PL8                                                              
BUFBKLN  EQU   *-BUFBK                                                          
         ORG   BUFBK                                                            
BUFSAL   DS    PL(BUFBKLN)         MONTHLY SALARY                               
BUFSALY  DS    PL(BUFBKLN)         YTD SALARY                                   
BUFPRDHR DS    PL(BUFBKLN)         HOURS WORKED CLIENT                          
BUFCLIHR DS    PL(BUFBKLN)         HOURS WORKED TOYOTA                          
BUFTHRS  DS    PL(BUFBKLN)         TOTAL HOURS WORKED                           
BUFYPHRS DS    PL(BUFBKLN)         YTD HOURS WORKED PRODUCT                     
BUFYCHRS DS    PL(BUFBKLN)         YTD HOURS WORKED TOYOTA CLIENT               
BUFYTHRS DS    PL(BUFBKLN)         YTD TOTAL HOURS WORKED                       
BUFYPCT  DS    PL(BUFBKLN)         YTD PERCENT TOYOTA HRS TO TOTAL              
BUFHRRAT DS    PL(BUFBKLN)         HOURLY RATE                                  
BUFPST   DS    PL(BUFBKLN)         MONTHLY POSTED TO TOYOTA CLIENT              
BUFYPST  DS    PL(BUFBKLN)         YTD POSTED TO TOYOTA CLIENT                  
BUFFACA  DS    PL(BUFBKLN)         FACTOR A ADJUSTMENT (% X YTD COST)           
BUFTOTAL DS    PL(BUFBKLN)         TOTAL                                        
BUF16H   DS    PL(BUFBKLN)         DEPARTMENT 16 HOURS                          
BUF16C   DS    PL(BUFBKLN)         DEPARTMENT 16 COST                           
BUF17H   DS    PL(BUFBKLN)         DEPARTMENT 17 HOURS                          
BUF17C   DS    PL(BUFBKLN)         DEPARTMENT 17 COST                           
BBKCNT   EQU   (*-BUFBK)/BUFBKLN   NUMBER OF BUCKETS                            
BLEN     EQU   *-BUFD                                                           
         EJECT                                                                  
*********************************************************************           
* DSECT FOR REPORT 1 RECORDS (TIME & COST RECAP)                    *           
*       TYPE/YY/MM/                TOTALS BY MONTH                  *           
*       TYPE/FF/FF/                12 MONTH TOTAL ROW               *           
*********************************************************************           
         SPACE 1                                                                
TCAPD    DSECT                                                                  
TCAPKEY  DS    0CL40                                                            
TCAPTYP  DS    CL1                 TYPE                                         
TCAPEQU  EQU   3                   RECAP REPORT-REGULAR                         
TCAPGPQ  EQU   5                   RECAP REPORT-GROUP                           
TCAPGRP  DS    CL2                 GROUP CODE                                   
TCAPYYMM DS    0CL2                YYMM                                         
TCAPYR   DS    CL1                 YEAR PACKED                                  
TCAPMNTH DS    CL1                 MONTH PACKED                                 
         DS    CL37                SPARE                                        
TCAKLEN  EQU   *-TCAPD                                                          
TCABK    DS    PL8                                                              
TCABKLN  EQU   *-TCABK                                                          
         ORG   TCABK                                                            
TCAPH10  DS    PL(TCABKLN)         HOURS DEPT 10                                
TCAPC10  DS    PL(TCABKLN)         COST  DEPT 10                                
TCAPDACM EQU   *-TCABK             DEPT 10'S ACCUMS                             
TCAPH11  DS    PL(TCABKLN)         HOURS DEPT 11                                
TCAPC11  DS    PL(TCABKLN)         COST  DEPT 11                                
TCAPH12  DS    PL(TCABKLN)         HOURS DEPT 12                                
TCAPC12  DS    PL(TCABKLN)         COST  DEPT 12                                
TCAPH20  DS    PL(TCABKLN)         HOURS DEPT 20 NEW INSERTION                  
TCAPC20  DS    PL(TCABKLN)         COST  DEPT 20 NEW INSERTION                  
TCAPH25  DS    PL(TCABKLN)         HOURS DEPT 25 NEW INSERTION                  
TCAPC25  DS    PL(TCABKLN)         COST  DEPT 25 NEW INSERTION                  
TCAPH13  DS    PL(TCABKLN)         HOURS DEPT 13                                
TCAPC13  DS    PL(TCABKLN)         COST  DEPT 13                                
TCAPH14  DS    PL(TCABKLN)         HOURS DEPT 14                                
TCAPC14  DS    PL(TCABKLN)         COST  DEPT 14                                
TCAPH15  DS    PL(TCABKLN)         HOURS DEPT 15                                
TCAPC15  DS    PL(TCABKLN)         COST  DEPT 15                                
TCADPCNT EQU   (*-TCABK)/TCAPDACM  NUMBER OF OF DEPARTMENTS                     
TCAPHT   DS    PL(TCABKLN)         HOURS TOTAL                                  
TCAPCT   DS    PL(TCABKLN)         COST  TOTAL                                  
TCACNT   EQU   (*-TCABK)/TCABKLN   NUMBER OF BUCKETS                            
TCAPLNQ  EQU   *-TCAPD                                                          
         EJECT                                                                  
*********************************************************************           
* DSECT FOR REPORT 2 RECORDS (TIME & COST)                          *           
*       TYPE/DEPT/SUB/PERSON       PERSON TOTALS                    *           
*       TYPE/DEPT/SUB/FE           SUB-DEPT TOTALS                  *           
*       TYPE/DEPT/FE/00            DEPT TOTALS HOURS                *           
*       TYPE/DEPT/FF/00            DEPT TOTALS COSTS                *           
*       TYPE/FF/00/00              REPORT TOTALS                    *           
*********************************************************************           
         SPACE 1                                                                
TCQD     DSECT                                                                  
TCQKEY   DS    0CL40                                                            
TCQTYP   DS    CL1                 TYPE                                         
TCQEQU   EQU   4                   TIME AND COST REPORT-REGULAR                 
TCQGPQ   EQU   6                   TIME AND COST REPORT-GROUP                   
TCQGRP   DS    CL2                 GROUP CODE FOR GROUP REPORT                  
TCQDEPT  DS    CL2                 APG DEPARTMENT CODE                          
TCQTITL  DS    CL2                 APG SUB-DEPT   CODE                          
TCQSUB   DS    CL29                TITLE                                        
TCQPRSN  DS    CL6                 DEPT (10=CONTACT & PLANNING)                 
TCQKLEN  EQU   *-TCQD                                                           
TCQBK    DS    PL8                                                              
TCQBKLN  EQU   *-TCQBK                                                          
         ORG   TCQBK                                                            
TCQTR1   DS    PL(TCQBKLN)         1ST QTR HOURS                                
TCQTR2   DS    PL(TCQBKLN)         2ND QTR HOURS                                
TCQTR3   DS    PL(TCQBKLN)         3RD QTR HOURS                                
TCQTR4   DS    PL(TCQBKLN)         4TH QTR HOURS                                
TCTIME   DS    PL(TCQBKLN)         PERCENT OF TIME                              
TCTOT    DS    PL(TCQBKLN)         TOTAL HOURS                                  
         DS    12PL(TCQBKLN)       SPARE                                        
TCQCNT   EQU   (*-TCQBK)/TCQBKLN   NUMBER OF BUCKETS                            
TCLNQ    EQU   *-TCQD                                                           
         EJECT                                                                  
*                                                                               
* DEPARTMENT QTRLY TOTALS                                                       
*                                                                               
DPTTD    DSECT                                                                  
DPTQBUK  DS    PL6                                                              
DPTQBLN  EQU   *-DPTQBUK                                                        
         ORG   DPTQBUK                                                          
DPTQ1    DS    PL(DPTQBLN)         1ST QTR COSTS                                
DPTQ2    DS    PL(DPTQBLN)         2ND QTR COSTS                                
DPTQ3    DS    PL(DPTQBLN)         3RD QTR COSTS                                
DPTQ4    DS    PL(DPTQBLN)         4TH QTR COSTS                                
DPTTOTL  DS    PL(DPTQBLN)         DEPT TOTAL                                   
DPTQCNT  EQU   (*-DPTQBUK)/DPTQBLN NUMBER OF BUCKETS                            
DPTQLN   EQU   *-DPTTD                                                          
         EJECT                                                                  
*********************************************************************           
* DSECT FOR A PRINT LINE                                            *           
*********************************************************************           
         SPACE 1                                                                
PLINED   DSECT                                                                  
PL       DS    0CL(L'XP)                                                        
PMNTH    DS    CL6                 MMM/YY                                       
PMSTAT   DS    CL1                 ADJUSTED STATUS *                            
*                                                                               
* *** M O N T H L Y***                                                          
*                                                                               
PSAL     DS    CL14                MONTHLY SALARY 999,999.99-                   
         DS    CL1                                                              
PPHRS    DS    CL11                PRODUCT HOURS 99999.99-                      
         DS    CL1                                                              
PTHRS    DS    CL11                TOTAL HOURS 99999.99-                        
         DS    CL1                                                              
PCHRS    DS    CL11                CLIENT HOURS 99999.99-                       
         DS    CL1                                                              
*                                                                               
*                                  *** YEAR TO DATE ***                         
*                                                                               
PYPHRS   DS    CL11                YTD PRODUCT HOURS 99999.99-                  
         DS    CL1                                                              
PYPCT    DS    CL9                 YTD ACTUAL PCT                               
         DS    CL1                                                              
PYRATE   DS    CL7                 HOURLY RATE 999.99-                          
         DS    CL1                                                              
         ORG   PYPCT                                                            
PYTPHR   DS    CL9                 YTD TOTAL PROD HRS FOR GRP REP               
         DS    CL1                                                              
PYGRPPCT DS    CL8                 YTD ACTUAL PCT FOR GRP REP                   
         DS    CL1                                                              
PYCOST   DS    CL13                SALARY COST 9,999,999.99-                    
         DS    CL1                                                              
*                                                                               
*                                  *** M O N T H L Y***                         
*                                                                               
PMCOST   DS    CL13                MONTHLY COST 9,999,999.99-                   
         DS    CL1                                                              
PFIN     DS    CL14                FINAL CHARGE 99,999,999.99-                  
PCOMLEN  EQU   *-PYCOST                                                         
         ORG   PYCOST                                                           
PCOMMENT DS    CL(PCOMLEN)                                                      
PLEN     EQU   *-PLINED                                                         
*                                                                               
* RECAP PRINT LINE                                                              
*                                                                               
         ORG   PL                                                               
         DS    CL4                                                              
PRMNTH   DS    CL15                MONTH,19XX OR 20XX                           
*                                                                               
         ORG   PL                                                               
         DS    CL4                                                              
PRDESC   DS    0CL19               DESCRIPTION FIELD                            
PRMAN    DS    CL19                MANPOWE EQUIV DESCRIPTION                    
         ORG   PRDESC                                                           
PRPERC   DS    CL15                MANPOWE EQUIV DESCRIPTION                    
         DS    CL4                                                              
         ORG   PRDESC                                                           
         DS    CL2                                                              
PRHOUR   DS    CL5                 HOURS DESCRIPTION                            
         DS    CL12                                                             
         ORG   PRDESC                                                           
         DS    CL2                                                              
PRCOST   DS    CL4                 COST DESCRIPTION                             
         DS    CL13                                                             
*                                                                               
         ORG   PL                                                               
         DS    CL19                COST HOURS                                   
PRCSTHR1 DS    CL15                -ACCOUNT MANAGEMENT                          
PRCSTHR2 DS    CL15                -EVENT MARKETING                             
PRCSTHR3 DS    CL15                -CREATIVE                                    
PRCSTHR4 DS    CL15                -PRINT SERVICES                              
PRCSTHR5 DS    CL15                -BROADCAST PROD/B'CAST TRAFFIC               
PRCSTHR6 DS    CL15                -ACCOUNT PLANNING                            
PRCSTHR7 DS    CL15                -MEDIA                                       
PRCSTHR8 DS    CL15                -BUSINESS AND LEGAL                          
PRCSTHR9 DS    CL15                -TOTAL                                       
         ORG   PL                                                               
         DS    CL25                MANPOWER EQUIV HOURS                         
PRMANHR1 DS    CL5                 -ACCOUNT MANAGEMENT                          
         DS    CL10                                                             
PRMANHR2 DS    CL5                 -EVENT MARKETING                             
         DS    CL10                                                             
PRMANHR3 DS    CL5                 -CREATIVE                                    
         DS    CL10                                                             
PRMANHR4 DS    CL5                 -PRINT SERVICES                              
         DS    CL10                                                             
PRMANHR5 DS    CL5                 -BROADCAST PROD/ B'CAST TRAFFIC              
         DS    CL10                                                             
PRMANHR6 DS    CL5                 -ACCOUNT PLANNING                            
         DS    CL10                                                             
PRMANHR7 DS    CL5                 -MEDIA                                       
         DS    CL10                                                             
PRMANHR8 DS    CL5                 -BUSINESS AND LEGAL                          
         DS    CL10                                                             
PRMANHR9 DS    CL5                 -TOTAL                                       
*                                                                               
* GROUP ANALYSIS PRINT LINE                                                     
*                                                                               
         ORG   PL                                                               
         DS    CL2                                                              
PGANGRP  DS    CL2                 GROUP CODE                                   
         DS    CL1                                                              
PGANGRPN DS    CL20                GROUP NAME                                   
*                                                                               
         ORG   PL                                                               
         DS    CL2                                                              
PANDPT   DS    CL2                 DEPT CODE                                    
         DS    CL1                                                              
PANDPTNM DS    CL36                DEPT NAME                                    
*                                                                               
         ORG   PANDPTNM                                                         
PANTITLE DS    CL36                TITLE                                        
*                                                                               
         ORG   PANDPTNM                                                         
         DS    CL2                                                              
PANPRSN  DS    CL36                PERSON NAME                                  
*                                                                               
         ORG   PL                                                               
         DS    CL40                                                             
PANQTRS  DS    0CL11               QTRS                                         
PANQTR1  DS    CL11                QTR 1 AMOUNT                                 
         DS    CL4                                                              
PANQTR2  DS    CL11                QTR 2 AMOUNT                                 
         DS    CL4                                                              
PANQTR3  DS    CL11                QTR 3 AMOUNT                                 
         DS    CL4                                                              
PANQTR4  DS    CL11                QTR 4 AMOUNT                                 
         DS    CL4                                                              
*                                                                               
         ORG   PL                                                               
         DS    CL107                                                            
PANPCT   DS    CL4                 PERCENTAGE                                   
*                                                                               
         ORG   PL                                                               
         DS    CL115                                                            
PANTOT   DS    CL11                TOTAL FOR FOUR QTRS                          
*                                                                               
         ORG   PL                                                               
         DS    CL5                                                              
PANGTOT  DS    CL10                MSG 'TOTALS FOR'                             
         DS    CL1                                                              
PANGTOTD DS    CL23                TOTALS FOR ???? DESCRIPTION                  
         EJECT                                                                  
********* EQUATES ********************************                              
*                                                                               
*                                                                               
ALL      EQU   X'FF'               X'FF' USED IN "NI" TURN SWITCH OFF           
PASSDEL  EQU   X'08'               PASS BACK DELETED RECORDS (DATAMGR)          
SPACE    EQU   X'40'               SPACE                                        
BRANCH   EQU   X'F0'               UNCONDITIONAL BRANCH                         
EQUAL    EQU   X'80'               BRANCH EQUAL                                 
NOTEQUAL EQU   X'70'               BRANCH NOT EQUAL                             
HOURS    EQU   C'H'                H                                            
FEES     EQU   C'F'                FEE'S                                        
BENEFIT  EQU   C'B'                BENEFIT                                      
SALARY   EQU   C'1'                SALARY BUCKETS                               
EOT      EQU   X'FF'               END OF TABLE                                 
EOF      EQU   X'80'               END OF FILE                                  
MAXMTHS  EQU   12                  MAXIMUM NUMBER OF MONTHS                     
MAXLEN   EQU   12                  MAXIMUM LENGTH OF ACCOUNT                    
OMAX     EQU   6                   MAX OTHER LOCATIONS                          
ZERO     EQU   0                   BINARY ZERO TO CLEAR A BYTE                  
IOSIZE   EQU   2008                I/O AREA SIZE                                
MAXDPTS  EQU   9                   MAXIMUM NUMBER OF DUP DEPT TOTS              
*                                                                               
*                                                                               
*                                           BUFFALO EQUATES                     
*                                                                               
BUFROW   EQU   2                            NUMBER OF BUFFALO ROWS              
BDATLN   EQU   BUFROW*BBKCNT*BUFBKLN        BUFFALO DATA LENGTH                 
BRECLN   EQU   BDATLN+BUFKLEN               BUFFALO RECORD LENGTH               
BUFMAX   EQU   1000                         MAX NUMBER IN CORE                  
BUFCORE  EQU   BUFMAX*BRECLN                BUFFALO CORE                        
BUFSPACE EQU   L'BUFFCNTL+BRECLN+BUFCORE    TOTAL BUFFALO CORE TAKEN            
ASCEND   EQU   C'A'                         KEY IN ASENDING ORDER               
FILINDC  EQU   C'A'                         BUFFALO FILE INDICATOR              
PACKED   EQU   C'P'                         BUFF ACCUMS PACKED FORMAT           
BINARY   EQU   C'B'                         BUFF ACCUMS BINARY FORMAT           
         EJECT                                                                  
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* ACREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
* ACGENMODES                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
* ACMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
* ACSALHSTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACSALHSTD                                                      
         PRINT ON                                                               
* ACQD                                                                          
         PRINT OFF                                                              
       ++INCLUDE ACQD                                                           
         PRINT ON                                                               
* DDMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
* DDLOGOD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDLOGOD                                                        
         PRINT ON                                                               
* DDSLRD                                                                        
         PRINT OFF                                                              
       ++INCLUDE DDSLRD                                                         
         PRINT ON                                                               
* DDBUFFALOD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDBUFFALOD                                                     
         PRINT ON                                                               
* DDREPXTRAD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDREPXTRAD                                                     
         PRINT ON                                                               
* DDLCB                                                                         
         PRINT OFF                                                              
       ++INCLUDE DDDLCB                                                         
         PRINT ON                                                               
* ACBIGPRNTD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACBIGPRNTD                                                     
         PRINT ON                                                               
* DDBIGBOX                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'037ACREPFR02X07/22/02'                                      
         END                                                                    
