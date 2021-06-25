*          DATA SET BUFIL40    AT LEVEL 094 AS OF 05/01/02                      
*PHASE T50240A                                                                  
*INCLUDE BUSETRLS                                                               
*INCLUDE PUBEDIT                                                                
*INCLUDE BUPPER                                                                 
         TITLE 'T50240 - BUDGET EXTRACT CONTROL PROGRAM'                        
T50240   CSECT                                                                  
         PRINT NOGEN                                                            
         SPACE 2                                                                
         NMOD1 0,T50240,RR=R8                                                   
         LA    RA,2048(RB)                                                      
         LA    RA,2048(RA)                                                      
         USING T50240+4096,RA                                                   
         ST    R8,RELO                                                          
*                                                                               
         L     RC,0(R1)            RC=A(GENCON WORKING STORAGE)                 
         USING GEND,RC                                                          
*                                                                               
         L     R9,ASYSD            R9=A(SYSTEM WORKING STORAGE)                 
         USING SYSD,R9                                                          
*                                                                               
         L     R3,ATWA             R3=A(TWA)                                    
         USING T502FFD,R3                                                       
*                                                                               
         STM   RA,RB,T50240RA                                                   
         ST    RD,T50240RD                                                      
*                                                                               
         GOTO1 VSETADD             RESET TASK DEPENDENT ADDRESSES               
         L     RE,ANODBLK                                                       
         LA    R0,NDHK                                                          
         ST    R0,NDHOOK-NDKEY(RE)                                              
*                                                                               
         CLI   OFFLINE,C'Y'        TEST OFFLINE PROCESSING                      
         BE    EXT1                                                             
         B     EXT2                IN NODBLK                                    
         EJECT                                                                  
* ALLOCATE STORAGE FOLLOWING OVERLAY FOR OFFLINE PROCESSING *                   
         SPACE 1                                                                
EXT1     L     RE,VADUMMY                                                       
*                                                                               
         MVC   0(8,RE),=C'*ESTTAB*'                                             
         LA    RE,8(RE)                                                         
         ST    RE,AESTTAB                                                       
         AH    RE,=H'4096'                                                      
*                                                                               
         MVC   0(8,RE),=C'*WKBUFF*'                                             
         LA    RE,8(RE)                                                         
         ST    RE,AWKBUFF                                                       
         AH    RE,=H'4096'                                                      
*                                                                               
         LA    RE,16(RE)           BUMP TO NEXT DBWD BOUNDARY                   
         SRL   RE,3                                                             
         SLL   RE,3                                                             
         ST    RE,AEXTOVLY                                                      
*                                                                               
         LA    RE,CALLBUP                                                       
         ST    RE,VBUPPER                                                       
*                                                                               
         LA    RE,DATAHD                                                        
         ST    RE,VDATAHD                                                       
*                                                                               
         LA    RE,PRTRULE          SET RULE PRINT ROUTINE ADDRESS               
         ST    RE,VPRTRULE                                                      
*                                                                               
         CLI   MODE,VALKEY                                                      
         BNE   EXT1A                                                            
         L     R0,LENMEM           ACQUIRE 700K STORAGE FOR RULE TABLE          
         GETMAIN R,LV=(0)                                                       
         ST    R1,ARULETAB                                                      
         L     RE,TWAMASTC                                                      
         USING MASTD,RE                                                         
         STCM  R1,15,MCUSRDMP                                                   
         A     R1,LENMEM                                                        
         STCM  R1,15,MCUSRDMP+4                                                 
         DROP  RE                                                               
         EJECT                                                                  
* BUILD BUPPER INTERFACE BLOCK *                                                
         SPACE 1                                                                
EXT1A    CLC   BUPBLOCK-8(8),=C'*BUPBLK*' TEST ALREADY INITIALIZED              
         BE    EXT2                                                             
         MVC   BUPBLOCK-8(8),=C'*BUPBLK*'                                       
*                                                                               
         LA    R2,BUPBLOCK                                                      
         USING BUPBLKD,R2                                                       
         XC    BUPBLOCK,BUPBLOCK                                                
*                                                                               
         L     RF,TWADCONS                                                      
         USING TWADCOND,RF                                                      
*                                                                               
         MVC   BUPADMGR,TDMGR                                                   
         MVC   BUPAHELO,THELLO                                                  
         DROP  RF                                                               
*                                                                               
         MVC   BUPABUFF,AWKBUFF    SET 4K BUFFER ADDRESS                        
         LA    RE,ELEM             SET IO AREA ADDRESS = ELEM                   
         ST    RE,BUPAREC                                                       
         MVI   BUPORIG,BUACTEXT                                                 
         MVC   BUPBDATE,BTODAY                                                  
*                                                                               
         MVC   BUIKUSER,TWAORIG                                                 
         L     RE,TWAMASTC                                                      
         USING MASTD,RE                                                         
         L     R1,MCUTL                     R1=A(UTL)                           
         MVC   BUIKDSYS,4(R1)               EXTRACT SE NUMBER                   
         MVC   BUIKOPGM,=C'EX'              SET ORIG PROG ID                    
         MVC   BUIKBDAY,BTODAY+2            SET DAY NUMBER                      
*                                                                               
         DROP  R2,RE                                                            
*                                                                               
EXT2     CLI   MODE,VALKEY                                                      
         BE    EXT10                                                            
*                                                                               
         CLI   MODE,PRINTREP                                                    
         BNE   EXT4                                                             
*                                                                               
         L     R8,ASPOOLD          INITIALIZE FOR PRINTING                      
         USING SPOOLD,R8                                                        
         L     R1,=A(HEDSPECS)                                                  
         A     R1,RELO                                                          
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         CLI   SVTERMSW,YES        TEST IF REQUEST TERMINATED                   
         BNE   EXT100              NO-GO PROCESS IT                             
*                                                                               
         OC    AERRULE,AERRULE     TEST FOR ERROR RULE                          
         BZ    EXT3                                                             
         GOTO1 VPRTRULE,PARAS,AERRULE                                           
*                                                                               
EXT3     MVC   P+10(L'SVERRMSG),SVERRMSG                                        
         GOTO1 SPOOL,PARAS,(R8)                                                 
         GOTO1 (RF),(R1),(R8)                                                   
         MVC   P+10(31),=C'** REPORT ENDED DUE TO ERROR **'                     
         GOTO1 (RF),(R1),(R8)                                                   
         L     R1,ARULETAB                                                      
         L     R0,LENMEM                                                        
         FREEMAIN R,A=(1),LV=(0)                                                
         B     EXIT                                                             
*                                                                               
EXT4     CLI   MODE,RUNLAST                                                     
         BNE   EXIT                                                             
         SPACE 1                                                                
* AT RUNLAST,CLOSE BUPPER INTERFACE *                                           
         SPACE 1                                                                
EXT140   DS    0H                                                               
         L     R1,ARULETAB                                                      
         L     R0,LENMEM                                                        
         FREEMAIN R,A=(1),LV=(0)                                                
         TM    WHEN,X'20'          TEST SOON PROCESSING                         
         BZ    EXIT                NO - NO RUNLAST PROCESSING                   
         MVI   DMCB,BUPCLOSE                                                    
         BAS   RE,CALLBUP                                                       
         B     EXIT                                                             
*                                                                               
RELO     DS    A                                                                
         EJECT                                                                  
* VALIDATE CLIENT *                                                             
         SPACE 1                                                                
EXT10    DS    0H                                                               
         MVI   RULESW,0            INHIBIT RULE PROCESSING                      
         MVI   SVTERMSW,NO                                                      
*                                                                               
         LA    R2,EXTCLTH                                                       
         BAS   RE,CLRNAME                                                       
         SPACE 1                                                                
         GOTO1 ANY                                                              
*                                                                               
         XC    NODKEY,NODKEY                                                    
         MVC   NODKEY(3),WORK                                                   
*                                                                               
         GOTO1 VNODIO,DMCB,ANODBLK,=C'READ',NODKEY                              
*                                                                               
         MVI   ERROR,NOTFOUND                                                   
         CLI   SVCLTEL,BUCLTELQ         TEST FOR CLT DESC ELEM                  
         BNE   EXTERR                                                           
         LA    R6,SVCLTEL                                                       
         USING BUCLTD,R6                                                        
         MVC   EXTCLNM,BUCLTNAM                                                 
         DROP  R6                                                               
*                                                                               
         EJECT                                                                  
* VALIDATE PRODUCT *                                                            
         SPACE 1                                                                
EXT20    LA    R2,EXTPRDH                                                       
         SPACE 1                                                                
         GOTO1 ANY                                                              
*                                                                               
         BAS   RE,SETKEY           POINT R1 TO END OF KEY                       
         MVC   0(3,R1),WORK                                                     
*                                                                               
         GOTO1 VNODIO,DMCB,ANODBLK,=C'READ',NODKEY                              
*                                                                               
         MVI   ERROR,NOTFOUND                                                   
         CLI   SVPRDEL,BUPROELQ          TEST FOR PRD DESC ELEM                 
         BNE   EXTERR                                                           
         LA    R6,SVPRDEL                                                       
         USING BUPROD,R6                                                        
         MVC   EXTPRNM,BUPRONAM                                                 
         DROP  R6                                                               
         EJECT                                                                  
* VALIDATE PLAN *                                                               
         SPACE 1                                                                
EXT30    LA    R2,EXTPLANH                                                      
         SPACE 1                                                                
         GOTO1 ANY                                                              
*                                                                               
         BAS   RE,SETKEY           POINT R1 TO END OF KEY                       
         MVI   SVNFOV,0                                                         
         XC    SVFOV,SVFOV         CLEAR FISCAL YEAR OVERRIDES + COUNT          
         MVC   0(3,R1),WORK                                                     
*                                                                               
         GOTO1 VNODIO,DMCB,ANODBLK,=C'READ',NODKEY                              
*                                                                               
         MVI   ERROR,NOTFOUND                                                   
         CLI   SVPLANEL,BUPLNELQ         TEST FOR PLN DESC ELEM                 
         BNE   EXTERR                                                           
         MVC   SVNKEY,NODKEY       SAVE PLAN'S NODAL KEY                        
         LA    R6,SVPLANEL                                                      
         USING BUPLND,R6                                                        
         MVC   EXTPLNM(L'BUPLNNAM),BUPLNNAM                                     
         GOTO1 VPEROUT,PARAS,(1,BUPLNST),WORK                                   
         LA    R1,EXTPLNM+L'BUPLNNAM+1                                          
         MVC   0(13,R1),WORK       DISPLAY PLAN PERIOD ON SCREEN                
         MVC   EXST,BUPLNST        SET PLAN START/END AS                        
         MVC   EXEND,BUPLNEND      EXTRACT START/END                            
         MVI   MULTIYR,C'Y'        SET MULTI YEAR PLAN SWITCH                   
         CLI   BUPLNST,0           TEST OPEN ENDED PLAN                         
         BE    *+8                                                              
         MVI   MULTIYR,C'N'                                                     
         DROP  R6                                                               
*                                                                               
         B     EXT32                                                            
         SPACE 2                                                                
* SUBR TO POINT R1 TO END OF NODKEY *                                           
         SPACE 1                                                                
SETKEY   LA    R1,NODKEY+L'NODKEY-1    POINT TO LAST BYTE                       
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         MVI   1(R1),C'.'                                                       
         LA    R1,2(R1)                                                         
         BR    RE                                                               
         EJECT                                                                  
* READ THE DATA TYPE SUB-RECORDS *                                              
         SPACE 1                                                                
EXT32    L     R8,ANODBLK                                                       
         USING NODBLKD,R8                                                       
         LA    RE,NDLVTABL*3+NDLVTAB    POINT TO LEVEL 3 ENTRY                  
         USING NDLVTABD,RE                                                      
         MVC   KEY,NDLVKEY                                                      
         MVI   KEY+BUDSUB-BUKEY,BUDSUBQ                                         
         DROP  RE                                                               
*                                                                               
         LA    R4,SVDTYPES                                                      
         LR    RE,R4                                                            
         LA    RF,L'SVDTYPES                                                    
         SR    R1,R1                                                            
         MVCL  RE,R0               CLEAR DATA TYPE TABLE                        
         USING SVDTD,R4                                                         
*                                                                               
         GOTO1 HIGH                                                             
         B     EXT34A                                                           
*                                                                               
EXT34    DS    0H                                                               
         GOTO1 SEQ                                                              
*                                                                               
EXT34A   CLC   KEY(BUDSUB+L'BUDSUB-BUKEY),KEYSAVE                               
         BNE   EXT38                                                            
*                                                                               
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         USING BURECD,R6                                                        
         LA    R6,BUFRSTEL                                                      
EXT35    CLI   0(R6),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R6),BUDTELQ       FIND DATA ELEMENT                            
         BE    EXT36                                                            
         SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
         B     EXT35                                                            
*                                                                               
         USING BUDTD,R6                                                         
EXT36    CLI   BUDTEX,0            TEST DO NOT EXTRACT                          
         BE    EXT34                                                            
         LA    R0,MAXDTYP                                                       
         LA    R4,SVDTYPES                                                      
*                                                                               
EXT36A   CLI   SVDTEX,0            TEST FOR EOT                                 
         BE    EXT37                                                            
         CLC   BUDTEX,SVDTEX       TEST FOR DUPLICATE EXTRACT TYPES             
         BE    EXT34               YES-SKIP THIS RECORD                         
         LA    R4,SVDTL(R4)                                                     
         BCT   R0,EXT36A                                                        
*                                                                               
         MVI   ERROR,EXTHIERR      TOO MANY EXTRACT TYPES                       
         CLI   OFFLINE,C'Y'                                                     
         BNE   EXTERR                                                           
         MVI   SVTERMSW,YES        TERMINATE REPORT                             
         MVI   SVERRMSG,C' '                                                    
         MVC   SVERRMSG+1(L'SVERRMSG-1),SVERRMSG                                
         MVC   SVERRMSG(40),=C'* ERROR - TOO MANY EXTRACTABLE DTYPES *'         
         B     EXIT                                                             
*                                                                               
EXT37    MVC   SVDTEX,BUDTEX               SAVE DATA TYPE                       
         MVC   SVDTCOD,KEY+(BUDTYP-BUKEY)     AND DATA TYPE CODE                
         B     EXT34                                                            
*                                                                               
EXT38    MVI   ERROR,NODTYPES                                                   
         OC    SVDTYPES,SVDTYPES   TEST ANY DATA TYPES FOUND                    
         BNZ   EXT40                                                            
*                                                                               
         CLI   OFFLINE,C'Y'                                                     
         BNE   EXTERR                                                           
*                                                                               
         MVI   SVTERMSW,YES        TERMINATE REPORT AND SET ERROR MSG           
         MVI   SVERRMSG,C' '                                                    
         MVC   SVERRMSG+1(L'SVERRMSG-1),SVERRMSG                                
         MVC   SVERRMSG(40),=C'** ERROR - NO EXTRACTABLE DATA TYPES **'         
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
* VALIDATE OUTLINE *                                                            
         SPACE 1                                                                
EXT40    LA    R2,EXTOUTH                                                       
         SPACE 1                                                                
         GOTO1 VGETFLD,PARAS,(X'FF',(R2))                                       
         CLI   FLDH+5,0            TEST FOR ANY INPUT                           
         BNE   EXT41               YES-VALIDATE CODE INPUT                      
*                                                                               
         CLI   OFFLINE,YES         TEST IF OFFLINE                              
         BNE   EXT45               NO                                           
         GOTO1 =V(SETRULES),DMCB,0,ARULETAB,RR=RELO                             
         MVI   RULESW,1            SET RULE TABLE INITIALIZED                   
         B     EXT45                                                            
*                                                                               
EXT41    GOTO1 VFINDOUT,PARAS,FLD,AIO1                                          
         BNE   EXTERR                                                           
         GOTO1 VNODIO,DMCB,ANODBLK,=C'TRACE',NODKEY                             
*                                                                               
         CLI   OFFLINE,C'Y'        TEST OFFLINE PROCESSING                      
         BNE   EXT42                                                            
         GOTO1 =V(SETRULES),DMCB,0,ARULETAB,RR=RELO                             
         MVI   RULESW,1            SET TO PROCESS RULE ELEMENTS                 
*                                                                               
EXT42    CLI   OFFLINE,YES         TEST OFFLINE                                 
         BNE   *+8                                                              
         BAS   RE,RDPAR            READ PARENTS OF REQUESTED OUTLINE            
*                                                                               
         GOTO1 VNODIO,DMCB,ANODBLK,=C'READ',NODKEY                              
*                                                                               
         MVI   ERROR,NOTFOUND                                                   
         CLI   SVOUTEL,BUOUTELQ    TEST FOR OUTLINE ELEMENT                     
         BNE   EXTERR                                                           
*                                                                               
         GOTO1 VGETVAL                                                          
         LA    R6,SVOUTEL                                                       
         USING BUOUTD,R6                                                        
         MVC   EXTOTNM,BUOUTNAM                                                 
         DROP  R6                                                               
         SPACE 1                                                                
* NOW PROCESS ALL CHILDREN OF THIS PLAN/OUTLINE *                               
         SPACE 1                                                                
EXT45    CLI   OFFLINE,C'Y'                                                     
         BNE   EXT50                                                            
*                                                                               
         L     R8,ANODBLK                                                       
         USING NODBLKD,R8                                                       
*                                                                               
         GOTO1 VNODIO,DMCB,ANODBLK,=C'LSEQ',NODKEY                              
         EJECT                                                                  
* VALIDATE EXTRACT MODE AND TEST OPTIONS *                                      
         SPACE 1                                                                
EXT50    DS    0H                                                               
         LA    R2,EXTMODEH                                                      
         MVI   SVTEST,0            CLEAR SWITCHES                               
         MVI   SVTRACE,0           X'80'=DATA,X'40'=BUFFALO                     
*                                                                               
         CLI   OFFLINE,C'Y'                                                     
         BNE   EXT51                                                            
         L     RF,TWAMASTC                                                      
         USING MASTD,RF                                                         
         CLI   MCWRITE,C'N'        TEST WRITE = NO                              
         BNE   *+8                 NO                                           
         MVI   SVTEST,C'Y'         SET TEST=YES IF WRITE=NO                     
         DROP  RF                                                               
*                                                                               
EXT51    GOTO1 VGETFLD,PARAS,(R2)                                               
         MVI   ERROR,MISSING                                                    
         CLI   FLDH+5,0                                                         
         BE    EXTERR                                                           
         CLI   FLDH+5,1            TEST FOR INPUT LEN OF 1                      
         BNE   EXT52                                                            
*                                                                               
         CLI   FLD,C'E'            TEST FOR MODE=EXTRACT                        
         BNE   *+12                                                             
         MVI   SVTEST,NO                                                        
         B     EXT60                                                            
*                                                                               
         CLI   FLD,C'T'            MODE=TEST                                    
         BNE   *+12                                                             
         MVI   SVTEST,YES                                                       
         B     EXT60                                                            
*                                                                               
EXT52    CLI   FLDH+5,2            TEST FOR INPUT LEN OF 2                      
         BNE   EXTERR                                                           
         CLC   FLD(2),=C'TA'       TEST=TRACE ALL                               
         BNE   EXT52A                                                           
         MVI   SVTRACE,X'C0'                                                    
         MVI   SVTEST,C'Y'                                                      
         B     EXT60                                                            
*                                                                               
EXT52A   CLC   FLD(2),=C'TB'       TEST=TRACE BUFFALO                           
         BNE   EXT52B                                                           
         MVI   SVTRACE,X'40'                                                    
         MVI   SVTEST,C'Y'                                                      
         B     EXT60                                                            
*                                                                               
EXT52B   CLC   FLD(2),=C'TD'       TEST=TRACE DATA                              
         BNE   EXT52C                                                           
         MVI   SVTRACE,X'80'                                                    
         MVI   SVTEST,C'Y'                                                      
         B     EXT60                                                            
*                                                                               
EXT52C   MVI   ERROR,INVALID                                                    
         B     EXTERR                                                           
         EJECT                                                                  
* VALIDATE YEAR FIELD                                                           
*                                                                               
EXT60    LA    R2,EXTYEARH                                                      
         GOTO1 VGETFLD,PARAS,(R2)                                               
         CLI   FLDH+5,0            TEST FOR NO INPUT                            
         BE    EXT62               YES                                          
*                                                                               
         MVI   ERROR,INVALID                                                    
         CLI   EXST,0              TEST REGULAR PLAN                            
         BNE   EXTERR              YES-YEAR IS EXTRANEOUS                       
         MVI   ERROR,NOTNUM                                                     
         TM    FLDH+4,X'08'        TEST NUMERIC INPUT                           
         BZ    EXTERR                                                           
*                                                                               
         CHI   R0,80               TREAT YEARS BEFORE 1980 AS Y2K               
         BNL   *+8                                                              
         AHI   R0,100                                                           
         STC   R0,EXST             SET START YEAR                               
         LR    R1,R0                                                            
         CLC   EXST+1(1),EXEND+1   TEST START MONTH L.T. END MONTH              
         BL    *+8                 YES-EXTRACT START/ENDS IN SAME YEAR          
         LA    R1,1(R1)            NO-EXTRACT CROSSES INTO NEXT YEAR            
         STC   R1,EXEND                                                         
         B     EXT65                                                            
*                                                                               
EXT62    MVI   ERROR,MISSING                                                    
         CLI   EXST,0              TEST OPEN ENDED PLAN                         
         BE    EXTERR              YES-YEAR IS REQUIRED                         
         SPACE 2                                                                
* VALIDATE PERIOD FILTER FIELD                                                  
*                                                                               
EXT65    LA    R2,EXTPERH                                                       
         GOTO1 VGETFLD,PARAS,(R2)                                               
         CLI   FLDH+5,0                                                         
         BE    EXT69               NO FILTER SPECIFIED                          
         XC    DMCB+8(4),DMCB+8                                                 
         MVC   DMCB+8(1),EXST+1    FISCAL YEAR START MONTH                      
         MVC   DMCB+9(1),CLTTYPE   FISCAL YEAR TYPE                             
         GOTO1 VMONVAL,DMCB,FLD,EXST                                            
         MVI   ERROR,INVDATE                                                    
         OC    4(4,R1),4(R1)                                                    
         BZ    EXTERR                                                           
         MVC   PERIOD,4(R1)                                                     
         MVC   EXST(4),PERIOD      SET NEW EXTRACT PERIOD                       
*                                                                               
EXT69    B     EXT70                                                            
         EJECT                                                                  
* PREVENT LIVE PLAN EXTRACT RUNNING SOON                                        
*                                                                               
EXT70    CLI   SVTEST,YES          TEST FOR LIVE EXTRACT                        
         BE    EXT72               NO                                           
         OC    OUTCODE,OUTCODE     TEST FOR OUTLINE REQUEST                     
         BNZ   EXT72               YES                                          
         MVI   ERROR,SOONERR       N0-REQUEST IS FOR WHOLE PLAN                 
         LA    R2,CONWHENH                                                      
         TM    WHEN,X'18'          TEST FOR OVERNIGHT REQUEST                   
         BZ    EXTERR              NO-MUST BE SOON                              
         SPACE 1                                                                
* FOR LIVE EXTRACT REQUEST, SET INDICATOR ON PLAN RECORD                        
*                                                                               
EXT72    CLI   OFFLINE,C'Y'        ONLY WHEN ON-LINE                            
         BE    EXT75                                                            
         CLI   SVTEST,C'Y'         AND LIVE EXTRACT REQUEST                     
         BE    EXT75                                                            
*                                                                               
         LA    R6,SVPLANEL                                                      
         USING BUPLND,R6                                                        
         TM    BUPLNIND,BUPLNDAT   TEST IF DATA RECORD IND ON                   
         BO    EXT75               YES-ALREADY EXTRACTED OR INPUT               
*                                                                               
         MVC   FULL,NDHOOK                                                      
         XC    NDHOOK,NDHOOK                                                    
         GOTO1 VNODIO,DMCB,ANODBLK,=C'READ',SVNKEY,0                            
         CLI   NDERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   ELCODE,BUPLNELQ                                                  
         GOTO1 HELLO,DMCB,(C'G',SYSFIL),(ELCODE,NDIOA),0                        
         L     R6,12(R1)                                                        
         CLI   12(R1),0            TEST PLAN DESC EL FOUND                      
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    BUPLNIND,BUPLNDAT   TURN ON DATA RECORD INDICATOR                
         GOTO1 VNODIO,DMCB,ANODBLK,=C'PUT',SVNKEY,0                             
         CLI   NDERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   NDHOOK,FULL         RESTORE A(HOOK) ROUTINE                      
*                                                                               
EXT75    B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
CLRNAME  XC    EXTCLNM,EXTCLNM                                                  
         OI    EXTCLNMH+6,X'80'                                                 
         XC    SVCLTEL,SVCLTEL                                                  
*                                                                               
         XC    EXTPRNM,EXTPRNM                                                  
         OI    EXTPRNMH+6,X'80'                                                 
         XC    SVPRDEL,SVPRDEL                                                  
*                                                                               
         XC    EXTPLNM,EXTPLNM                                                  
         OI    EXTPLNMH+6,X'80'                                                 
         XC    SVPLANEL,SVPLANEL                                                
*                                                                               
         XC    EXTOTNM,EXTOTNM                                                  
         OI    EXTOTNMH+6,X'80'                                                 
         XC    SVOUTEL,SVOUTEL                                                  
*                                                                               
         BR    RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO READ THE PARENTS OF AN OUTLINE AND POST THEIR RULES            
*                                                                               
* AT ENTRY, NODKEY CONTAINS OUTLINE'S KEY AND NODBLK DESCRIBES                  
*           THE OUTLINE                                                         
*                                                                               
RDPAR    NTR1                                                                   
         L     R8,ANODBLK                                                       
         USING NODBLKD,R8                                                       
         ZIC   R2,NDLEV                                                         
         SH    R2,=H'4'            COMPUTE N'PARENTS                            
         BZ    RDPARX              NONE                                         
         LR    R4,R2               SAVE N'PARENTS IN R4                         
*                                                                               
RDPAR1   L     R5,AIO3             BUILD TABLE OF PARENTS' KEYS IN IO3          
         LR    R1,R2                                                            
         BCTR  R1,0                                                             
         MH    R1,=Y(L'NODKEY)     INDEX TO LAST ENTRY POSITION                 
         LA    R5,0(R1,R5)         R5=A(NODAL KEY)                              
         MVC   NODKEYSV,NODKEY     INITIALIZE KEY AREA                          
*                                                                               
RDPAR2   BAS   RE,GETPAR           CONSTRUCT PARENT'S KEY                       
         MVC   0(L'NODKEY,R5),NODKEYSV                                          
         LA    RF,L'NODKEY                                                      
         SR    R5,RF               BACK UP TO NEXT PARENT'S POSITION            
         BCT   R2,RDPAR2                                                        
*                                                                               
RDPAR4   L     R5,AIO3             R5=A(PARENT'S NODAL KEY)                     
*                                                                               
RDPAR5   GOTO1 VNODIO,DMCB,ANODBLK,=C'READ',(R5)                                
         CLI   NDERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R5,L'NODKEY(R5)                                                  
         BCT   R4,RDPAR5                                                        
*                                                                               
RDPARX   B     EXIT                                                             
         SPACE 1                                                                
* SUB-ROUTINE TO CONSTRUCT THE PARENT'S KEY FOR AN OUTLINE                      
* (CALLED FROM RDPAR. AT ENTRY NODKEYSV CONTAINS OUTLINE KEY)                   
*                                                                               
GETPAR   LA    R0,L'NODKEY         R0=COUNTER                                   
         LA    RF,NODKEYSV                                                      
         LA    RF,L'NODKEY-1(RF)   RF=A(END OF NODAL KEY)                       
*                                                                               
GETPAR2  CLC   NDDELIM,0(RF)                                                    
         BE    GETPAR4                                                          
         MVI   0(RF),C' '                                                       
         BCTR  RF,0                                                             
         BCT   R0,GETPAR2                                                       
         DC    H'0'                                                             
*                                                                               
GETPAR4  MVI   0(RF),C' '                                                       
         BR    RE                                                               
         DROP  R8                                                               
         EJECT                                                                  
* NODIO HOOK PROCESSING *                                                       
         SPACE 1                                                                
NDHK     NTR1                                                                   
         L     R8,ANODBLK                                                       
         USING NODBLKD,R8                                                       
*                                                                               
         CLI   NDLEV,0             NEVER PROCESS MASTER RECORD                  
         BE    EXIT                                                             
         CLI   NDMODE,NDPROC       TEST FOR PROCESS NODAL RECORD                
         BNE   EXIT                                                             
*                                                                               
         L     R6,NDIOA                                                         
         USING BURECD,R6                                                        
         LA    R6,BUFRSTEL                                                      
         DROP  R6                                                               
*                                                                               
NDHK2    CLI   0(R6),0                                                          
         BE    NDHKX                                                            
         LA    R0,(NDHKTABX-NDHKTAB)/4                                          
         LA    R1,NDHKTAB                                                       
*                                                                               
NDHK4    CLC   0(1,R6),0(R1)                                                    
         BE    NDHK6                                                            
         LA    R1,4(R1)                                                         
         BCT   R0,NDHK4                                                         
         B     NDHK10                                                           
*                                                                               
NDHK6    L     RF,0(R1)            GET PROC ROUTINE ADDRESSS                    
         A     RF,RELO                                                          
         BASR  RE,RF               SET RE IN CASE RF BAD                        
         DC    H'0'                AMAZING IF WE GET TO THIS                    
*                                                                               
NDHK10   SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
         B     NDHK2                                                            
         SPACE 1                                                                
NDHKTAB  DC    AL1(BUCLTELQ),AL3(NDHKCLT)                                       
         DC    AL1(BUPROELQ),AL3(NDHKPRO)                                       
         DC    AL1(BUPLNELQ),AL3(NDHKPLN)                                       
         DC    AL1(BUOUTELQ),AL3(NDHKOUT)                                       
         DC    AL1(BUFOVELQ),AL3(NDHKFOV)                                       
NDHKTABX EQU   *                                                                
         EJECT                                                                  
* CLIENT ELEMENT PROCESSING                                                     
*                                                                               
NDHKCLT  LA    R1,SVCLTEL                                                       
         GOTO1 VGETVAL                                                          
         B     NDHKMV                                                           
*                                                                               
* PRODUCT ELEMENT PROCESSING                                                    
*                                                                               
NDHKPRO  LA    R1,SVPRDEL                                                       
         GOTO1 VGETVAL                                                          
         B     NDHKMV                                                           
*                                                                               
* PLAN ELEMENT PROCESSING                                                       
*                                                                               
NDHKPLN  LA    R1,SVPLANEL                                                      
         GOTO1 VGETVAL                                                          
         B     NDHKMV                                                           
*                                                                               
* OUTLINE ELEMENT/RECORD PROCESSING                                             
*                                                                               
NDHKOUT  MVI   BYTE,NO             SET LEVEL CHANGE SWITCH                      
         CLC   PREVLEV,NDLEV       TEST FOR CHANGE IN LEVEL NUMBER              
         BE    *+8                                                              
         MVI   BYTE,YES                                                         
         MVC   PREVLEV,NDLEV                                                    
*                                                                               
NDHKOUT2 CLI   RULESW,1            TEST TO PROCESS RULES NOW                    
         BNE   NDHKOUT4            NO                                           
         OI    RULESW,X'80'        SET 'PROCESSED' IND                          
*                                  ALWAYS CALL SETRULES FOR AN OUTLINE          
         L     R8,ANODBLK                                                       
         USING NODBLKD,R8                                                       
         GOTO1 =V(SETRULES),DMCB,(NDLEV,NDIOA),(BYTE,ARULETAB),RR=RELO          
         CLI   4(R1),0             TEST FOR ERRORS                              
         BE    NDHKOUT4                                                         
         SPACE 1                                                                
* PRINT ERROR MESSAGE AND STOP READ *                                           
*                                                                               
NDHKOUT3 MVC   AERRULE,4(R1)       SAVE ERROR RULE                              
         L     RF,NDLEVPTR                                                      
         USING NDLVTABD,RF                                                      
         MVC   WORK(8),NDLVCOD     EXTRACT OUTLINE CODE                         
*                                                                               
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         MVI   FORCEHED,YES                                                     
         L     RE,ARULETAB                                                      
         MVI   SVTERMSW,YES        TERMINATE REPORT                             
         MVC   SVERRMSG,SPACES                                                  
         MVC   SVERRMSG(8),=C'OUTLINE='                                         
         OC    WORK(8),SPACES                                                   
         MVC   SVERRMSG+8(8),WORK                                               
         MVC   SVERRMSG+18(40),0(RE) MESSAGE PASSED BY SETRULES                 
         L     RD,T50240RD         RETURN IMMEDIATELY TO GENCON                 
         B     EXIT                                                             
*                                                                               
NDHKOUT4 LA    R1,SVOUTEL                                                       
         B     NDHKMV                                                           
*                                                                               
* FISCAL YEAR OVERRIDE ELEMENT PROCESSING                                       
*                                                                               
NDHKFOV  ZIC   RE,SVNFOV           GET N'FISCAL YEAR OVERRIDES                  
         LR    RF,RE                                                            
         MH    RE,=Y(SVFOVL)       DISPLACEMENT INTO OVERRIDE LIST              
         LA    RE,SVFOV(RE)        INDEX TO NEXT OVERRIDE ENTRY                 
         USING BUFOVD,R6                                                        
         USING SVFOVD,RE                                                        
         MVC   SVFOVD(4),BUFOVSYS  EXTRACT ELEMENT DATA                         
         LA    RF,1(RF)            INCREMENT OVERRIDE COUNT                     
         STC   RF,SVNFOV                                                        
         CLI   SVNFOV,MAXFOV       TEST EXCEEDED MAXIMUM                        
         BNH   NDHK10              NO                                           
         DC    H'0'                                                             
         DROP  R6,RE                                                            
*                                                                               
NDHKMV   ZIC   RE,1(R6)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8              MOVE ELEM TO SAVE AREA                       
         B     NDHK10                                                           
         MVC   0(0,R1),0(R6) *EXECUTED*                                         
         SPACE 2                                                                
NDHKX    CLI   RULESW,X'81'        TEST RULES PROCESSED                         
         BNE   EXIT                                                             
         NI    RULESW,X'7F'        RESET 'PROCESSED' IND                        
         SPACE 2                                                                
EXIT     XIT1                                                                   
*                                                                               
EXTERR   GOTO1 ERREX                                                            
         EJECT                                                                  
* REPORT PROCESSING *                                                           
         SPACE 1                                                                
*********************************************************************           
* REMEMBER ABOUT PROBLEMS OF HAVING TO READ ID RECORDS FOR SECURITY *           
*********************************************************************           
         SPACE 1                                                                
* FILL IN RULES TABLE ENTRIES WITH AGENCY CODE *                                
*                                                                               
EXT100   DS    0H                                                               
         L     R2,ARULETAB                                                      
         USING QRHDRD,R2                                                        
         LA    R2,QRDATA           POINT TO FIRST ENTRY                         
         USING QRD,R2                                                           
         OC    QRLEN,QRLEN         ANY RULES AT ALL                             
         BNZ   EXT101              YES                                          
*                                                                               
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         MVC   P+10(39),=C'** NOTHING TO EXTRACT - REPORT ENDED **'             
         GOTO1 SPOOL,PARAS,(R8)                                                 
         B     EXT140              EXIT FREEING UP STORAGE                      
*                                                                               
EXT101   OC    QRAGYC,QRAGYC       TEST AGENCY RULE PROVIDED                    
         BNZ   *+10                YES                                          
         MVC   QRAGYC,AGENCY       NO-SO ITS THE USER                           
         MVC   HALF,QRLEN                                                       
         AH    R2,HALF                                                          
         OC    QRLEN,QRLEN         TEST END OF RULES TABLE                      
         BNZ   EXT101              NO-NEXT RULE                                 
*                                                                               
* FILL IN THE RULES TABLE ENTRIES WITH SE NUMBER AND AGENCY/MEDIA               
*                                                                               
EXT102   L     R2,ARULETAB                                                      
         LA    R2,QRDATA-QRHDRD(R2) POINT TO FIRST ENTRY IN TABLE               
*                                                                               
EXT103   CLI   QRSYS,0             TEST IF RULE HAS BEEN PROCESSED              
         BE    EXT104              NO                                           
         MVC   HALF,QRLEN                                                       
         AH    R2,HALF                                                          
         OC    QRLEN,QRLEN         TEST EOT                                     
         BNZ   EXT103              NO                                           
         B     EXT109              YES-ALL SE NUMBERS, AGYMD SET                
*                                                                               
* CONFID WILL RETURN FILES FOR FIRST ID FOR AGENCY IF YOU ONLY PASS             
* AGENCY CODE.  TO EXTRACT ACROSS AGENCIES, HARD CODE IS NEEDED TO              
* INSURE CORRECT SET OF FILE WILL BE OPENED.                                    
*                                                                               
EXT104   XC    CONFTAB,CONFTAB                                                  
         LA    R4,CONFTAB                                                       
         USING CND,R4                                                           
         MVC   CNAGY(2),QRAGYC     FIND FILES FOR RULE'S AGENCY                 
         L     RE,TWAMASTC                                                      
         USING MASTD,RE                                                         
         MVC   CNID,MCORIGID                                                    
         DROP  RE                                                               
*&&US                                                                           
         LA    R0,AGYIDENT         R0=LOOP COUNTER                              
         L     RE,=A(AGYIDTAB)     RE=A(AGENCY ID TABLE)                        
         CLC   QRAGYC,0(RE)        MATCH ON TWO-CHARACTER AGENCY CODE           
         BE    *+16                                                             
         LA    RE,L'AGYIDTAB(RE)                                                
         BCT   R0,*-14                                                          
         B     *+10                NO OVERRIDE NEEDED                           
*                                                                               
         MVC   CNID,2(RE)          OVERRIDE ORIGIN ID FOR CONFID                
*&&                                                                             
         L     RF,TWADCONS                                                      
         L     RF,TCONFID-TWADCOND(RF)                                          
         GOTO1 (RF),DMCB,CONFTAB,(1,FULL)                                       
         OC    FULL,FULL                                                        
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
EXT105   LA    R0,(CFDSPTBX-CFDSPTAB)/L'CFDSPTAB                                
         L     R1,=A(CFDSPTAB)                                                  
         MVC   HALF(1),QROVSYS                                                  
         MVC   HALF+1(1),QRSYSTYP                                               
*                                                                               
EXT106   CLC   HALF,0(R1)          MATCH OVSYS                                  
         BE    EXT107                                                           
         LA    R1,L'CFDSPTAB(R1)                                                
         BCT   R0,EXT106                                                        
         DC    H'0'                                                             
*                                                                               
EXT107   MVC   QRKEYLEN,9(R1)      SET KEY COMPARE LENGTH                       
         SR    RE,RE                                                            
         ICM   RE,3,3(R1)          GET DSP TO CONFID DATA                       
         LA    RE,0(R4,RE)         POINT TO CONFID DATA FOR THIS SYS            
         MVC   QRSYS,0(RE)         SET ACTUAL SYSTEM NUMBER                     
         MVC   QRAGY,1(RE)         AND AGENCY/COMPANY CODE                      
*                                                                               
EXT108   MVC   HALF,QRLEN                                                       
         AH    R2,HALF             NEXT RULE                                    
         OC    QRLEN,QRLEN         TEST MORE DATA                               
         BZ    EXT102              NO-EOT                                       
         CLC   QRAGYC,CNAGY        TEST SAME AGENCY AS IN CONFID TABLE          
         BE    EXT105              YES-SET FILE FOR THIS RULE                   
         B     EXT108              NO-BUMP TO NEXT RULE                         
*                                                                               
EXT109   TM    WHEN,X'20'          TEST 'SOON' PROCESSING                       
         BZ    EXT110                                                           
         SPACE 1                                                                
* DO BUPPER INITIALIZATION CALL *                                               
         SPACE 1                                                                
         MVI   DMCB,BUPINIT                                                     
         GOTO1 VBUPPER                                                          
*                                                                               
         L     RF,TWAMASTC                                                      
         USING MASTD,RF                                                         
         LA    RE,(BUPKEY-BUPBLKD)+BUPBLOCK                                     
         MVC   MCREPPQI,0(RE)      SET USER INFO FOR SPOOF CLOSE                
         DROP  RF                                                               
         B     EXT110                                                           
         EJECT                                                                  
* SEARCH FOR ANY RULES NOT YET PROCESSED *                                      
*                                                                               
         SPACE 1                                                                
EXT110   DS    0H                                                               
         L     R2,ARULETAB                                                      
         USING QRHDRD,R2                                                        
         LA    R2,QRDATA                                                        
         USING QRD,R2                                                           
         XC    ARULDATA,ARULDATA                                                
*                                                                               
EXT112   OC    QRLEN,QRLEN         ANY MORE RULES                               
         BZ    EXT140              NO - DONE                                    
         TM    QRFLAG,X'80'        TEST PREVIOUSLY PROCESSED                    
         BZ    EXT113              NO                                           
         MVC   HALF,QRLEN                                                       
         AH    R2,HALF             NEXT RULE                                    
         B     EXT112                                                           
*                                                                               
EXT113   ST    R2,ARULDATA         SAVE FIRST RULE ADDRESS                      
         BAS   RE,LNKRULE          GO LINK RULES FOR SAME A/M/C/P               
*                                                                               
         LA    R1,CFDSPTAB                                                      
         LA    R0,(CFDSPTBX-CFDSPTAB)/L'CFDSPTAB                                
         MVC   HALF(1),QROVSYS                                                  
         MVC   HALF+1(1),QRSYSTYP                                               
*                                                                               
EXT116   CLC   HALF,0(R1)          FIND ENTRY                                   
         BE    EXT118                                                           
         LA    R1,L'CFDSPTAB(R1)                                                
         BCT   R0,EXT116                                                        
         DC    H'0'                                                             
*                                                                               
EXT118   MVC   ASYSDATA,5(R1)       SAVE SYS FILE LIST ADDRESS                  
         MVC   ASYSDATA(1),QROVSYS  AND OVSYS NUM                               
         MVC   AEXTOVLY(1),2(R1)    SAVE EXT OVERLAY NUM                        
         MVC   AGYC,QRAGYC         SET AGENCY CODE BEING PROCESSED              
         MVC   THISSYS(2),HALF     SAVE OVSYS/SYSTEM TYPE                       
*                                                                               
         CLI   THISSYS,6           TEST FOR ACCOUNTING                          
         BNE   EXT119                                                           
*                                                                               
         L     R6,TWAMASTC         GET MASTC ADDRESS                            
         L     R6,MCUTL-MASTD(R6)  GET UTL ADDRESS                              
         MVC   SVUTLSYS,4(R6)      SAVE ORIGINAL SYSNUM                         
         MVC   4(1,R6),QRSYS       SET PROCESSING SYSNUM                        
*                                                                               
         XC    DMCB(24),DMCB                                                    
         GOTO1 DATAMGR,DMCB,=C'DTFAD ',=C'ACCFIL'                               
         MVI   EMULATE,C'N'                                                     
         L     RE,12(R1)                                                        
         USING ISDTF,RE                                                         
         TM    ISFTYPE,ISFTEMU     TEST FOR EMULATED FILE                       
         BZ    *+8                                                              
         MVI   EMULATE,C'Y'                                                     
         MVC   4(1,R6),SVUTLSYS    RESTORE UTL SYSTEM                           
         DROP  RE                                                               
*                                                                               
         CLI   EMULATE,C'Y'        TEST NEW FILE                                
         BNE   EXT119              NO                                           
         LA    R1,NACFLIST         YES-OPEN NEW FILE LIST                       
         STCM  R1,7,ASYSDATA+1                                                  
*                                                                               
EXT119   L     R1,ASYSDATA         GET FILE LIST ADDRESS                        
         ST    R1,DMCB+8                                                        
         SH    R1,=H'8'            BACK UP TO SYSTEM LITERAL                    
         ST    R1,DMCB+4                                                        
         MVC   DMCB+12(4),AIO1                                                  
*                                                                               
         L     R6,TWAMASTC         GET MASTC ADDRESS                            
         L     R6,MCUTL-MASTD(R6)  GET UTL ADDRESS                              
         MVC   SVUTLSYS,4(R6)      SAVE ORIGINAL SYSNUM                         
         MVC   4(1,R6),QRSYS       SET PROCESSING SYSNUM                        
         GOTO1 DATAMGR,DMCB,=C'OPEN'                                            
         SPACE 1                                                                
* LOAD IN THE PROCESSING OVERLAY *                                              
         SPACE 1                                                                
         MVC   DMCB(4),AEXTOVLY                                                 
         MVC   DMCB+4(3),=X'D90502'                                             
         MVC   DMCB+7(1),AEXTOVLY                                               
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   AEXTOVLY,0(R1)      SET LOAD ADDRESS                             
*                                                                               
* BUILD PERIOD START AND END DATE TABLE THEN CALL OVERLAY                       
*                                                                               
EXT120   GOTO1 =A(BLDPER),DMCB,(RC),RR=RELO                                     
         MVI   FORCEHED,YES        BREAK PAGE FOR NEW CALL                      
         MVI   XTRARULE,C' '       CLEAR EXTRA RULE TEXT AREA                   
         MVC   XTRARULE+1(L'XTRARULE-1),XTRARULE                                
         CLC   QRAGYC,=C'NW'       TEST FOR AYNY                                
         BNE   EXT122                                                           
         CLC   SIGNON,=CL8'AYJW'   TEST ITS JWT USER                            
         BNE   EXT122              NO                                           
*                                                                               
         LA    R0,NCLIENTS                                                      
         LA    RE,CLITAB                                                        
         CLC   QRCLT(L'CLITAB),0(RE)                                            
         BE    EXT122              ITS IN THE OK TABLE                          
         LA    RE,L'CLITAB(RE)                                                  
         BCT   R0,*-14                                                          
*                                                                               
         MVC   P+10(29),=C'SECURITY VIOLATION FOR CLIENT'                       
         MVC   P+40(L'QRCLT),QRCLT                                              
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
EXT121A  GOTO1 VPRTRULE,DMCB,(R2)                                               
         ICM   R2,15,QRNEXT                                                     
         BNZ   EXT121A                                                          
         B     EXT123                                                           
*                                                                               
EXT122   L     RF,AEXTOVLY                                                      
         GOTO1 (RF),DMCB,(RC),(R2)      CALL PROCESSING OVERLAY                 
         EJECT                                                                  
* FLAG ALL OUTLINES FOR THIS AG/MED/CLT/PRD AS PROCESSED *                      
         SPACE 1                                                                
EXT123   L     R2,ARULDATA         POINT TO INITIAL RULE                        
*                                                                               
EXT124   OI    QRFLAG,X'80'                                                     
         ICM   R2,15,QRNEXT        TEST MORE                                    
         BNZ   EXT124                                                           
         SPACE 1                                                                
* LOOK FOR MORE UNPROCESSED RULES FOR THIS SYSTEM *                             
         SPACE 1                                                                
         L     R2,ARULDATA         POINT TO RULE                                
*                                                                               
EXT126   MVC   HALF,QRLEN                                                       
         AH    R2,HALF             POINT TO NEXT RULE                           
         OC    QRLEN,QRLEN         TEST MORE DATA                               
         BZ    EXT130                                                           
         TM    QRFLAG,X'80'        TEST PROCESSED OR NON-EXTRACTABLE            
         BO    EXT126                                                           
         CLC   THISSYS,QROVSYS     TEST SAME OVERLAY SYSTEM                     
         BNE   EXT126                                                           
         CLC   THISSTYP,QRSYSTYP   TEST SAME SYSTEM TYPE                        
         BNE   EXT126                                                           
         CLC   AGYC,QRAGYC         TEST SAME AGENCY                             
         BNE   EXT126                                                           
*                                                                               
         ST    R2,ARULDATA         SET FIRST RULE ADDRESS                       
         BAS   RE,LNKRULE          LINK RULES TOGETHER                          
         B     EXT120              GO BACK TO OVERLAY                           
*                                                                               
EXT130   DS    0H                                                               
         L     R1,ASYSDATA         GET FILE LIST ADDRESS                        
         SH    R1,=H'8'            BACK UP TO SYSTEM LITERAL                    
         ST    R1,DMCB+4                                                        
         GOTO1 DATAMGR,DMCB,=C'DMCLSE'                                          
*                                                                               
         L     R6,TWAMASTC         GET MASTC ADDRESS                            
         L     R6,MCUTL-MASTD(R6)  GET UTL ADDRESS                              
         MVC   4(1,R6),SVUTLSYS    RESTORE ORIGINAL SYSNUM                      
         B     EXT110                                                           
SVUTLSYS DC    X'00'                                                            
         EJECT                                                                  
* SUB-ROUTINE TO LINK TOGETHER RULES FOR SAME SYSTEM/MEDIA, AGENCY,             
* CLIENT AND PRODUCT.  (ACTUAL LINKAGE COMPARE LENGTH IS IN                     
* QRKEYLEN).                                                                    
*                                                                               
* ROUTINE MAKES A DOUBLE PASS OF TABLE.  ON FIRST PASS, RULES                   
* ARE LINKED TOGETHER.  ON THE SECOND PASS, JUMP POINTERS ARE                   
* SET.  WHEN THE OVERLAY IS PROCESSING A RECORD OR PIECE OF DATA,               
* QRNEXT IS USED WHEN RULE DOES NOT APPLY TO DATA AND QRJUMP IS                 
* USED WHEN RULE APPLIES TO DATA.  PRESENCE OF QRJUMP ALLOWS                    
* DUPLICATION OF DATA IN PLAN AND MULTIPLE 'OTHERS' LINES                       
*                                                                               
LNKRULE  NTR1                                                                   
         ST    R2,DUB                                                           
         LR    RE,R2               AND FIRST RULE ADDRESS                       
*                                                                               
LNKRULE2 MVC   HALF,QRLEN                                                       
         AH    R2,HALF                                                          
         OC    QRLEN,QRLEN              TEST MORE ENTRIES                       
         BZ    LNKRULE4            EOL-BEGIN SECOND PASS                        
         CLC   QROVSYS,QROVSYS-QRD(RE)  TEST SAME SYSTEM                        
         BNE   LNKRULE2                                                         
         CLC   QRSYSTYP,QRSYSTYP-QRD(RE) TEST SAME TYPE (TAL VS PROD)           
         BNE   LNKRULE2                                                         
         TM    QRFLAG,X'80'             TEST EXTRACTABLE                        
         BO    LNKRULE2                                                         
* TEST SAME A/M/C/P                                                             
         ZIC   RF,QRKEYLEN         GET KEY COMPARE LEN                          
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   QRKEY-QRD(0,RE),QRKEY-QRD(R2) *EXECUTED*                         
         BNE   LNKRULE2                                                         
         STCM  R2,15,QRNEXT-QRD(RE)  SET AS 'NEXT' RULE IN PREV                 
         LR    RE,R2                 POINT TO NEW PREVIOUS                      
         B     LNKRULE2                                                         
*                                                                               
LNKRULE4 LA    R6,SVPLANEL                                                      
         USING BUPLND,R6                                                        
         TM    BUPLNIND,BUPLNDUP   TEST DUPLICATION OF DATA ALLOWED             
         BZ    LNKRULEX            NO                                           
*                                                                               
LNKRULE5 L     R2,DUB              GET POINTER TO FIRST RULE                    
         LR    RE,R2               RE=A(FIRST RULE)                             
*                                                                               
LNKRULE6 ICM   R2,15,QRNEXT        POINT TO NEXT LINKED RULE                    
         BZ    LNKRULEX                                                         
         TM    QRFLAG,X'40'        TEST FOR LEVEL CHANGE RULE                   
         BZ    LNKRULE6                                                         
*                                                                               
LNKRULE8 STCM  R2,15,QRJUMP-QRD(RE) SET JUMP ADDRESS                            
         ICM   RE,15,QRNEXT-QRD(RE) POINT TO NEXT RULE                          
         CR    RE,R2               TEST IF JUMP POINT REACHED                   
         BL    LNKRULE8            NO-KEEP SETTING JUMP POINTERS                
         ST    R2,DUB              YES-RESTART LOOK AHEAD FOR JUMP PT.          
         B     LNKRULE5                                                         
*                                                                               
LNKRULEX B     EXIT                                                             
         EJECT                                                                  
* TABLE OF RESTRICTED CLIENT CODES ON AYNY FILE FOR AYJW (JWT) USERS            
*                                                                               
CLITAB   DS    0CL3                                                             
         DC    C'DB '                                                           
         DC    C'DA '                                                           
         DC    C'DBC'                                                           
NCLIENTS EQU   (*-CLITAB)/L'CLITAB                                              
*                                                                               
LENMEM   DC    F'1331200'                                                       
         EJECT                                                                  
* HEADLINE HOOK ROUTINE                                                         
*                                                                               
HOOK     NTR1  BASE=T50240RB                                                    
         L     RA,T50240RA         SPOOL CAN BE CALLED FROM                     
         L     R8,ASPOOLD          OVERLAYS                                     
         USING SPOOLD,R8                                                        
*                                                                               
         MVC   H4+10(3),CLTCODE                                                 
         OC    H4+10(3),SPACES                                                  
         MVC   H4+19(L'CLTNAM),CLTNAM                                           
*                                                                               
         MVC   H5+10(3),PRDCODE                                                 
         OC    H5+10(3),SPACES                                                  
         MVC   H5+19(L'PRDNAM),PRDNAM                                           
*                                                                               
         MVC   H6+10(3),PLANCODE                                                
         OC    H6+10(3),SPACES                                                  
         MVC   H6+19(L'PLANNAM),PLANNAM                                         
*                                                                               
HOOK1    MVC   H7+10(3),=C'ALL'                                                 
         OC    OUTCODE,OUTCODE     TEST IF OUTLINE REQUEST                      
         BZ    HOOK2               NO                                           
         MVC   H7+10(8),OUTCODE                                                 
         OC    H7+10(8),SPACES                                                  
         MVC   H7+19(L'OUTNAME),OUTNAME                                         
*                                                                               
HOOK2    MVC   H6+74(7),=C'EXTRACT'                                             
         LA    R4,H6+82                                                         
         CLI   SVTEST,YES                                                       
         BNE   *+14                                                             
         MVC   0(4,R4),=C'TEST'                                                 
         LA    R4,5(R4)                                                         
*                                                                               
         MVC   0(3,R4),=C'FOR'                                                  
         LA    R4,4(R4)            POSITION FOR DATE                            
         GOTO1 DATCON,DMCB,(3,BTODAY),(8,(R4))                                  
*                                                                               
         LA    R4,9(R4)                                                         
         LA    R6,SVPLANEL                                                      
         USING BUPLND,R6                                                        
         TM    BUPLNIND,BUPLNDUP   TEST IF DATA DUPLICATION ACTIVE              
         BZ    *+10                NO                                           
         MVC   0(21,R4),=C'WITH DATA DUPLICATION'                               
*&&US                                                                           
         CLI   SVTERMSW,YES                                                     
         BE    HOOK4                                                            
         L     R2,ARULDATA                                                      
         CLI   QROVSYS,4           TEST FOR PRINT EXTRACT                       
         BNE   HOOK4               NO                                           
*                                                                               
         TM    BUPLNIND+1,BUPLNSHR TEST FOR DRD SHARE APPLICATION               
         BZ    *+10                                                             
         MVC   0(19,R4),=C'APPLYING DRD SHARES'                                 
*&&                                                                             
*                                                                               
HOOK4    CLI   SVTERMSW,YES        TEST IF REPORT TERMINATED                    
         BE    HOOKX                                                            
         L     R2,ARULDATA         R2=FIRST RULE FOR EXTRACT                    
         MVC   DUB(1),QROVSYS      SYSTEM OVERLAY NUMBER                        
         MVC   DUB+1(1),QRSYSTYP   SYSTEM CHARACTER CODE                        
         MVC   DUB+2(1),QRMED      MEDIA CODE                                   
         LA    R0,SYSENTS          R0=N'TABLE ENTRIES                           
         L     R4,=A(SYSTAB)                                                    
         A     R4,RELO             R3=A(SYSTEM/MEDIA TABLE)                     
         USING SYSTABD,R4                                                       
*                                                                               
HOOK5    ZIC   R1,SYSTCOML         GET COMPARE LENGTH                           
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SYSTOV(0),DUB       TEST SAME OVERLAY NUMBER/SYSTEM ETC.         
         BE    HOOK6                                                            
         LA    R4,SYSTABL(R4)                                                   
         BCT   R0,HOOK5                                                         
         B     HOOKX               CANNOT FIND SYSTEM/MEDIA                     
*                                                                               
HOOK6    MVC   WORK,SPACES                                                      
         MVC   WORK(L'SYSTSNAM),SYSTSNAM SYSTEM NAME                            
         MVC   WORK+L'SYSTSNAM+1(L'SYSTMNAM),SYSTMNAM                           
         TM    SYSTIND,SYSTPMED    TEST PRINTING MEDIA SEPARATELY               
         BZ    HOOK8                                                            
         LA    R1,WORK+L'SYSTSNAM+1                                             
         MVC   0(6,R1),=C'MEDIA='                                               
         MVC   6(1,R1),QRMED       SHOW MEDIA FILTER                            
         CLI   QRMED,0             TEST FOR ALL MEDIA                           
         BNE   *+10                                                             
         MVC   6(3,R1),=C'ALL'                                                  
*                                                                               
HOOK8    LA    R0,L'SYSTSNAM+L'SYSTMNAM+1                                       
         GOTO1 SQUASHER,DMCB,WORK,(R0)                                          
         LR    R1,R0                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   H7+74(0),WORK                                                    
*                                                                               
HOOKX    B     EXIT                                                             
         EJECT                                                                  
* COMMON SUB-ROUTINE TO PRINT DATA HEADLINES                                    
*                                                                               
DATAHD   NTR1  BASE=T50240RB                                                    
         L     RA,T50240RA                                                      
         GOTO1 =A(DHEAD),DMCB,(RC)                                              
         B     EXIT                                                             
         EJECT                                                                  
* PROVIDE COMMON INTERFACE TO BUPPER *                                          
         SPACE 1                                                                
CALLBUP  NTR1  BASE=T50240RB                                                    
         L     RA,T50240RA                                                      
*                                                                               
         L     R3,ATWA                                                          
         L     RE,TWAMASTC                                                      
         L     RE,MCUTL-MASTD(RE)                                               
         MVC   SVBUPUTL,4(RE)      SAVE CURRENT UTL SYSNUM                      
         MVC   4(1,RE),SVUTLSYS    SET MPL SYSNUM IN UTL                        
*                                                                               
         LA    RE,BUPBLOCK         SET PARAM BLOCK ADDRESS                      
         STCM  RE,7,DMCB+1                                                      
*                                                                               
         CLI   DMCB,BUPADD                                                      
         BE    CALLB0                                                           
         CLI   DMCB,BUPPUT                                                      
         BE    CALLB0                                                           
         CLI   DMCB,BUPFINAL       TEST FOR LAST FOR RULE                       
         BE    CALLB30                                                          
         SPACE 1                                                                
* ACTION NOT ADD/PUT                                                            
         SPACE 1                                                                
         CLI   SVTEST,C'Y'                                                      
         BE    CALLBUPX                                                         
*                                                                               
         GOTO1 =V(BUPPER),DMCB,RR=RELO                                          
         CLI   0(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         B     CALLBUPX                                                         
*                                                                               
         SPACE 1                                                                
* FORMAT BUFFALO DATA TO FILE RECORD FORMAT *                                   
         SPACE 1                                                                
CALLB0   XC    ELEM,ELEM                                                        
         LA    R6,ELEM+4           LENGTH PRECEDES RECORD STUPID                
         USING BURECD,R6                                                        
*                                                                               
         L     R8,ANODBLK                                                       
         USING NODBLKD,R8                                                       
*                                                                               
         MVC   BUKEY,NDKEY                                                      
*                                                                               
         L     R2,BUFFRULE                                                      
         USING QRD,R2                                                           
*                                                                               
         MVC   BUKNODE,QRNODE                                                   
         MVC   BUKCODE,QRCODE                                                   
         MVI   BUVSUB,BUVSUBQ                                                   
*                                                                               
         LA    R1,SVDTYPES                                                      
         USING SVDTD,R1                                                         
CALLB2   CLC   SVDTEX,BUFFTYPE                                                  
         BE    CALLB4                                                           
         LA    R1,SVDTL(R1)                                                     
         B     CALLB2                                                           
*                                                                               
CALLB4   MVC   BUVDTYP,SVDTCOD                                                  
         MVC   BUVPER,BUFFPER                                                   
         LA    RE,(BUFRSTEL-BUKEY)+BUDALNQ                                      
         STCM  RE,3,BURLEN         SET REC LENGTH                               
         LA    RE,5(RE)            ADJUST FOR LEN FIELD + X'00'                 
         SLL   RE,16                                                            
         STCM  RE,15,ELEM                                                       
*                                                                               
         LA    R6,BUFRSTEL                                                      
         USING BUDAD,R6                                                         
         MVI   BUDAEL,BUDAELQ                                                   
         MVI   BUDALEN,BUDALNQ                                                  
         MVI   BUDAPREC,X'82'      INDICATE PENNIES                             
         ZAP   BUDATA,BUFFGRS                                                   
*                                                                               
CALLB10  DS    0H                                                               
         CLI   SVTEST,C'Y'         IS THIS A TEST RUN                           
         BE    CALLB20             YES - DO NOT WRITE RECORDS                   
*                                                                               
         GOTO1 =V(BUPPER),DMCB,RR=RELO                                          
         CLI   0(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* ACCUMULATE DATA ON PRINT LINES FOR EACH OUTLINE/DATA TYPE - PRINT             
* FOR EACH NEW OUTLINE/DATA TYPE                                                
*                                                                               
CALLB20  L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         LA    R4,P                R4=A(PRINT LINES)                            
         USING DHD,R4                                                           
         LA    R6,ELEM+4           R6=A(DATA RECORD)                            
         USING BURECD,R6                                                        
*                                                                               
CALLB22  CLC   LASTVALS,BUFFREC    TEST NEW RULE/DATA TYPE                      
         BE    CALLB24             NO                                           
         OC    LASTVALS,LASTVALS   TEST FIRST TIME FOR RULE                     
         BZ    *+8                                                              
         BAS   RE,TOTPRT           PRINT DETAIL AND TOTAL LINES                 
         MVC   LASTVALS,BUFFREC    UPDATE LAST RULE/DATA TYPE                   
         ZAP   THISSUM,=P'0'       CLEAR TOTAL BUCKET                           
*                                                                               
CALLB24  AP    THISSUM,BUFFGRS     UPDATE BUCKET                                
         MVC   DHCODE,BUKCODE                                                   
         OC    DHCODE,SPACES                                                    
         MVC   DHDTYP(L'BUVDTYP),BUVDTYP                                        
         LA    R1,SVEXTDTS         R1=A(EXTRACT PERIOD ENTRY)                   
         USING SVEXD,R1                                                         
         LA    R0,6                R0=COUNTER                                   
         LA    R5,DHPER1           R5=A(PRINT POSITION)                         
         MVI   BYTE,NO             SET SECOND LINE SWITCH                       
*                                                                               
CALLB26  OC    SVEXPER,SVEXPER     TEST EOT                                     
         BNZ   *+6                                                              
         DC    H'0'                PERIOD ON RECORD DEFECTIVE                   
         CLC   BUVPER,SVEXPER      MATCH ON PERIOD                              
         BE    CALLB28                                                          
         LA    R1,SVEXL(R1)        NEXT PERIOD                                  
         LA    R5,DHPER2-DHPER1(R5) NEXT PRINT POSITION                         
         BCT   R0,CALLB26                                                       
*                                                                               
CALLB27  CLI   BYTE,YES            TEST SECOND LINE PROCESSED                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         LA    R0,7                                                             
         LA    R5,DHPER7                                                        
         MVI   BYTE,YES                                                         
         B     CALLB26                                                          
*                                                                               
CALLB28  EDIT  (P8,BUFFGRS),(13,(R5)),2,MINUS=YES                               
         B     CALLBUPX                                                         
*                                                                               
* BUPFINAL CALL TO FLUSH ANY PENDING PRINT LINES FOR OUTLINE                    
*                                                                               
CALLB30  L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         LA    R4,P                                                             
         USING DHD,R4                                                           
         CLC   P,SPACES            TEST ANY PENDING PRINT LINES                 
         BE    *+8                                                              
         BAS   RE,TOTPRT           FORMAT TOTAL AND PRINT DETAIL LINES          
*                                                                               
         XC    LASTVALS,LASTVALS   CLEAR LAST VALUES                            
         ZAP   THISSUM,=P'0'                                                    
         GOTO1 SPOOL,DMCB,(R8)     SKIP A LINE AFTER DATA                       
*                                                                               
CALLBUPX L     R3,ATWA                                                          
         L     RE,TWAMASTC                                                      
         L     RE,MCUTL-MASTD(RE)                                               
         MVC   4(1,RE),SVBUPUTL    RESTORE UTL VALUE                            
         B     EXIT                                                             
SVBUPUTL DC    X'00'                                                            
         SPACE 1                                                                
* SUB-ROUTINE TO FORMAT TOTAL LINE FOR OUTLINE/DATA TYPE AND TO                 
* PRINT DETAIL LINE(S) AND TOTAL LINE                                           
*                                                                               
TOTPRT   ST    RE,SAVERE                                                        
         LA    R1,DHPER7           R1=A(1ST PERIOD ON 2ND PRINT LINE)           
         LA    R5,DHPER8           R5=A(2ND PERIOD ON 2ND PRINT LINE)           
         CLC   P2,SPACES           TEST ANYTHING ON SECOND LINE                 
         BE    *+12                NO-PUT TOTAL THERE                           
         LA    R1,L'P(R1)          YES-BUMP POINTERS AHEAD TO 3RD LINE          
         LA    R5,L'P(R5)                                                       
         MVC   3(8,R1),=C'** TOTAL'                                             
         MVI   12(R1),C'='                                                      
         EDIT  (P6,THISSUM),(13,(R5)),2,MINUS=YES                               
         MVC   14(2,R5),=C'**'                                                  
         GOTO1 SPOOL,PARAS,(R8)                                                 
         L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
         DROP  R1,R4                                                            
         EJECT                                                                  
* ROUTINE TO PRINT RULE TABLE DATA                                              
*                                                                               
* AT ENTRY, P1= 0 TO PRINT ALL LINKED RULES STARTING AT ARULDATA                
*              OR A(SPECIFIC RULE TO PRINT)                                     
* ROUTINE USES IO3 TO BUFFER PRINT LINES                                        
*                                                                               
PRTRULE  NTR1  BASE=T50240RB                                                    
         L     RA,T50240RA                                                      
*                                                                               
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         L     R3,ATWA                                                          
         USING T502FFD,R3                                                       
*                                                                               
         L     R2,0(R1)                                                         
         ST    R2,ARULFILT                                                      
         LTR   R2,R2               TEST FOR RULE FILTER                         
         BNZ   *+8                                                              
         L     R2,ARULDATA         NO-PRINT ENTIRE CHAIN                        
         USING QRD,R2                                                           
*                                                                               
         ZIC   R1,LINE                                                          
         LA    R1,3(R1)            MINIMUM OF 3 LINES FOR AN OUTLINE            
         CLI   SVTRACE,0                                                        
         BE    *+8                                                              
         LA    R1,1(R1)            ONE MORE WHEN TRACING                        
         CLM   R1,1,MAXLINES       TEST ENOUGH ROOM ON PAGE                     
         BL    *+8                                                              
         MVI   FORCEHED,YES        FORCE PAGE BREAK                             
         MVI   P+1,DASH                                                         
         MVC   P+2(119),P+1        PRINT A LINE OF DASHES                       
         GOTO1 SPOOL,PARAS,(R8)                                                 
*                                                                               
PRTRUL1  CLI   SVTRACE,0           TEST FOR TRACING                             
         BE    PRTRUL2             NO-SO SKIP INTERNAL ITEMS                    
         LA    R4,P+1                                                           
*                                                                               
         MVC   0(4,R4),=C'NOD='                                                 
         LA    R4,4(R4)                                                         
         LA    R1,QRNODE                                                        
         LA    R0,4                                                             
         BAS   RE,PRTHEX                                                        
         LA    R4,9(R4)                                                         
*                                                                               
         MVC   0(4,R4),=C'LEN='                                                 
         LA    R4,4(R4)                                                         
         LH    R0,QRLEN                                                         
         BAS   RE,EDIT                                                          
*                                                                               
         MVC   0(4,R4),=C'LVL='                                                 
         LA    R4,4(R4)                                                         
         ZIC   R0,QRLVL                                                         
         BAS   RE,EDIT                                                          
*                                                                               
         MVC   0(4,R4),=C'OVS='                                                 
         LA    R4,4(R4)                                                         
         ZIC   R0,QROVSYS                                                       
         BAS   RE,EDIT                                                          
*                                                                               
         MVC   0(4,R4),=C'SYS='                                                 
         LA    R4,4(R4)                                                         
         ZIC   R0,QRSYS                                                         
         BAS   RE,EDIT                                                          
*                                                                               
         MVC   0(4,R4),=C'AGY='                                                 
         LA    R4,4(R4)                                                         
         LA    R1,QRAGY                                                         
         LA    R0,1                                                             
         BAS   RE,PRTHEX                                                        
         LA    R4,3(R4)                                                         
*                                                                               
PRTRUL2  LA    R4,P+1                                                           
         CLI   SVTRACE,0           TEST IF TRACING                              
         BE    *+8                                                              
         LA    R4,P2+1             YES-CONTINUE ON SECOND LINE                  
*                                                                               
         MVC   0(8,R4),=C'OUTLINE='                                             
         MVC   8(8,R4),QRCODE                                                   
         LA    R4,17(R4)           BUMP 1 PAST OUTPUT                           
*                                                                               
         OC    QRAGYC,QRAGYC       TEST IF AGENCY SET                           
         BZ    PRTRUL2A            NO                                           
         CLC   AGENCY,QRAGYC       TEST IF AGENCY RULE                          
         BE    PRTRUL2A            NO-AGENCY=USER AGENCY                        
         MVC   0(7,R4),=C'AGENCY='                                              
         MVC   7(2,R4),QRAGYC                                                   
         LA    R4,10(R4)                                                        
*                                                                               
PRTRUL2A MVC   0(4,R4),=C'CLT='                                                 
         MVC   4(5,R4),QRCLT                                                    
         LA    R0,9                                                             
         BAS   RE,FINDLEN                                                       
         LA    R4,1(R1,R4)                                                      
*                                                                               
PRTRUL3  CLI   QRPGR,0             TEST FOR PRODUCT GROUP                       
         BE    PRTRUL4             NO-THEN ITS A PRODUCT RULE                   
         MVC   0(4,R4),=C'PGR='                                                 
         MVC   4(1,R4),QRPGR       FIRST CHARACTER OF PRODUCT GROUP             
         MVC   5(3,R4),QRPRD       REST OF PRODUCT GROUP IN PRODUCT             
         LA    R4,9(R4)                                                         
         B     PRTRUL6                                                          
*                                                                               
PRTRUL4  OC    QRPRD,QRPRD         TEST FOR ANY PRODUCT                         
         BZ    PRTRUL6                                                          
         MVC   0(4,R4),=C'PRD='                                                 
         MVC   4(3,R4),QRPRD                                                    
         LA    R4,8(R4)                                                         
*                                                                               
PRTRUL6  OC    QRFLT,QRFLT         TEST FOR ESTIMATE FILTERS                    
         BZ    PRTRUL10                                                         
         MVC   0(3,R4),=C'EF='                                                  
         LA    R4,4(R4)                                                         
         LA    R1,QRFLT                                                         
         LA    R0,3                                                             
*                                                                               
PRTRUL8  TM    0(R1),X'40'         TEST FOR NEGATIVE FILTER                     
         BO    *+12                NO                                           
         MVI   0(R4),C'-'                                                       
         LA    R4,1(R4)            INCREMENT OUTPUT POINTER                     
         MVC   0(1,R4),0(R1)                                                    
         OI    0(R4),C' '          RESTORE FILTER VALUE                         
         LA    R4,1(R4)                                                         
         LA    R1,1(R1)                                                         
         BCT   R0,PRTRUL8                                                       
*                                                                               
PRTRUL10 GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         TM    WHEN,X'20'          TEST SOON PROCESSING                         
         BZ    PRTRUL12                                                         
         CLI   SVTEST,YES          EXTRACT OR TEST                              
         BE    PRTRUL12            TEST-DO NOT HAVE WORK FILE                   
*                                                                               
         LA    RF,BUPBLOCK                                                      
         USING BUPBLKD,RF                                                       
*                                                                               
         LA    R4,P+1                                                           
         MVC   0(7,R4),=C'WKFILE='                                              
         MVC   7(4,R4),BUWKDSYS    SYS/PGM                                      
         UNPK  DUB,BUWKPDAY(2)                                                  
         MVC   11(2,R4),DUB+5                                                   
         MVC   13(1,R4),BUWKCLS                                                 
         LA    R4,14(R4)                                                        
         SR    R0,R0                                                            
         ICM   R0,3,BUWKSEQN                                                    
         BAS   RE,CVD                                                           
         MVI   0(R4),C','                                                       
         MVC   1(4,R4),WORK+6                                                   
         DROP  RF                                                               
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
PRTRUL12 L     R7,=A(PRTTAB)                                                    
         A     R7,RELO                                                          
         USING PRTTABD,R7                                                       
*                                                                               
PRTRUL20 SR    R5,R5                                                            
         ICM   R5,3,PRTDSP         GET DSPL TO THIS LIST IN RULE                
         AR    R5,R2               POINT TO LIST DSPL IN RULE                   
         OC    0(2,R5),0(R5)       TEST LIST DSPL PRESENT                       
         BNZ   PRTRUL30            YES - GO PRINT IT                            
*                                                                               
PRTRUL22 LA    R7,PRTNEXT                                                       
         CLI   0(R7),X'FF'         TEST FOR EOT                                 
         BNE   PRTRUL20            NO                                           
         CLC   XTRARULE,SPACES     TEST FOR USER SUPPLIED TEXT                  
         BE    PRTRUL23            NO                                           
         MVC   P1+10(L'XTRARULE),XTRARULE                                       
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   XTRARULE,SPACES     CLEAR USER TEXT FIELD                        
*                                                                               
PRTRUL23 GOTO1 SPOOL,DMCB,(R8)     SKIP A LINE                                  
         OC    ARULFILT,ARULFILT   TEST FOR RULE FILTER                         
         BNZ   EXIT                YES-RULE HAS BEEN PRINTED NOW                
         ICM   R2,15,QRNEXT        TEST MORE RULES                              
         BNZ   PRTRUL2                                                          
         B     EXIT                                                             
*                                                                               
PRTRUL30 MVC   P1+1(4),PRTDESC     MOVE IN DESCRIPTION                          
         L     R4,AIO3             POINT TO PRINT BUILD AREA                    
         LA    R0,8                                                             
         LR    R1,R4                                                            
         XC    0(250,R1),0(R1)     AND CLEAR                                    
         LA    R1,250(R1)                                                       
         BCT   R0,*-10                                                          
*                                                                               
         LH    R5,0(R5)            GET DSPL TO LIST                             
         AR    R5,R2               AND POINT TO LIST                            
         ZIC   R6,0(R5)            GET NUMBER OF ENTRIES                        
         LA    R5,1(R5)            POINT TO FIRST ENTRY                         
         SR    RF,RF                                                            
         ICM   RF,7,PRTRTN                                                      
         BASR  RE,RF               * SET RE FOR HELP IN DUMPS *                 
         DC    H'0'                SUBR SHOULD BRANCH TO PRTRUL32               
         SPACE 1                                                                
* ON  RETURN R4 POINTS 1 BEYOND DATA *                                          
         SPACE 1                                                                
PRTRUL32 L     R1,AIO3             POINT TO PRINT DATA                          
         LA    R5,P1+10                                                         
*                                                                               
PRTRUL34 LA    RE,120(R1)          MAX MOVE IS 120 CHARS                        
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8                                                           
*                                                                               
         SR    RE,R1               GIVES LEN-1 TO MOVE                          
         BM    PRTRUL36            GET OUT IF NEGATIVE                          
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),0(R1)       MOVE DATA TO PRINT LINE                      
*                                                                               
         LA    R1,2(RE,R1)                                                      
         LA    R5,132(R5)                                                       
         CR    R1,R4               TEST DONE                                    
         BL    PRTRUL34                                                         
*                                                                               
PRTRUL36 GOTO1 SPOOL,DMCB,(R8)                                                  
         B     PRTRUL22                                                         
*                                                                               
       ++INCLUDE BUFIL40PCO                                                     
         EJECT                                                                  
PRTJOB   DS    0H                                                               
         TM    0(R5),X'40'         TEST FOR POSITIVE RULE                       
         BO    PRTJOB2             YES                                          
         MVI   0(R4),STAR          NO-'*' FOR NEGATIVE RULE                     
         LA    R4,1(R4)                                                         
*                                                                               
PRTJOB2  MVC   0(LRUJOB,R4),0(R5)                                               
         OI    0(R4),X'40'         MAKE SURE RULE IS PRINTABLE                  
         LA    R4,LRUJOB+1(R4)                                                  
         LA    R5,LRUJOB(R5)                                                    
         BCT   R6,PRTJOB2                                                       
         B     PRTRUL32                                                         
         SPACE 2                                                                
PRTWCD   DS    0H                                                               
         TM    0(R5),X'40'         TEST FOR POSITIVE RULE                       
         BO    PRTWCD2                                                          
         MVI   0(R4),STAR                                                       
         LA    R4,1(R4)                                                         
*                                                                               
PRTWCD2  MVC   0(2,R4),0(R5)                                                    
         OI    0(R4),X'40'                                                      
         LA    R4,3(R4)                                                         
         LA    R5,LRUWCD(R5)                                                    
         BCT   R6,PRTWCD2                                                       
         B     PRTRUL32                                                         
         SPACE 2                                                                
PRTAF    TM    0(R5),X'40'         TEST FOR NEGATIVE FILTERS                    
         BO    PRTAF2                                                           
         MVI   0(R4),STAR                                                       
         LA    R4,1(R4)                                                         
*                                                                               
PRTAF2   MVC   0(1,R4),0(R5)                                                    
         OI    0(R4),X'40'                                                      
         LA    R4,2(R4)                                                         
         LA    R5,1(R5)                                                         
         BCT   R6,PRTAF2                                                        
         B     PRTRUL32                                                         
         SPACE 2                                                                
PRTMED   TM    0(R5),X'40'         TEST FOR NEGATIVE FILTERS                    
         BO    PRTMED2                                                          
         MVI   0(R4),STAR                                                       
         LA    R4,1(R4)                                                         
*                                                                               
PRTMED2  MVC   0(1,R4),0(R5)                                                    
         OI    0(R4),X'40'                                                      
         LA    R4,2(R4)                                                         
         LA    R5,1(R5)                                                         
         BCT   R6,PRTMED2                                                       
         B     PRTRUL32                                                         
         EJECT                                                                  
CVD      CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(10),DUB                                                     
         BR    RE                                                               
*                                                                               
EDIT     DS    0H                                                               
         ST    RE,FULL                                                          
         EDIT  (R0),(10,(R4)),ALIGN=LEFT                                        
         AR    R4,R0                                                            
         LA    R4,1(R4)                                                         
         L     RE,FULL                                                          
         BR    RE                                                               
*                                                                               
* FINDLEN - AT ENTRY, R4 =A(OUTPUT), R0=MAX OUTPUT LEN                          
*           ON EXIT, R1=L'SIGNIFICANT OUTPUT                                    
*                                                                               
FINDLEN  LR    R1,R4                                                            
         AR    R1,R0                                                            
         BCTR  R1,0                R1=A(LAST OUTPUT BYTE)                       
         CLI   0(R1),C' '                                                       
         BH    FINDLEN2                                                         
         BCTR  R1,0                BACK UP POINTER                              
         BCT   R0,*-10                                                          
         DC    H'0'                                                             
*                                                                               
FINDLEN2 SR    R1,R4               COMPUTE LENGTH                               
         LA    R1,1(R1)                                                         
         BR    RE                                                               
*                                                                               
PRTHEX   NTR1                                                                   
         ST    R1,DMCB                                                          
         ST    R4,DMCB+4                                                        
         ST    R0,DMCB+8                                                        
         LA    RE,=C'TOG'                                                       
         ST    RE,DMCB+12                                                       
         L     RF,TWADCONS                                                      
         L     RF,THEXOUT-TWADCOND(RF)                                          
         GOTO1 (RF),DMCB                                                        
         B     EXIT                                                             
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
*  ENTRIES ARE                                                                  
*               OVSYS NUMBER                                                    
*               SYSTEM TYPE                                                     
*               SYSTEM EXTRACT PHASE NUMBER                                     
*               DISPLACEMENT IN CONFID TABLE                                    
*               ADDRESS OF SYSTEM FILE LIST                                     
*               LENGTH FOR EQUAL KEY COMPARES                                   
*********************************************************************           
         SPACE 1                                                                
CFDSPTAB DS    0XL10                                                            
*&&US                                                                           
         DC    X'02',C'S',X'42',AL2(CNSSE-CND)                                  
         DC    AL4(SPFLIST),AL1(QRPRDX-QRKEY)                                   
*                                                                               
         DC    X'03',C'N',X'43',AL2(CNNSE-CND)                                  
         DC    AL4(NTFLIST),AL1(QRCLTX-QRKEY)                                   
*                                                                               
         DC    X'04',C'P',X'44',AL2(CNPSE-CND)                                  
         DC    AL4(PRFLIST),AL1(QRPRDX-QRKEY)                                   
*&&                                                                             
*&&UK                                                                           
         DC    X'04',C'M',X'44',AL2(CNMSE-CND)                                  
         DC    AL4(MEFLIST),AL1(QRPRDX-QRKEY)                                   
*&&                                                                             
*                                                                               
         DC    X'06',C'P',X'46',AL2(CNASE-CND)                                  
         DC    AL4(ACFLIST),AL1(QRPRDX-QRKEY)                                   
*                                                                               
         DC    X'06',C'I',X'45',AL2(CNASE-CND)                                  
         DC    AL4(ACFLIST),AL1(QRPRDX-QRKEY)                                   
*                                                                               
CFDSPTBX EQU   *                                                                
         SPACE 2                                                                
*&&US                                                                           
         DC    CL8'SPOT    '                                                    
SPFLIST  DS    0C                                                               
         DC    CL8'NSPTFILE'                                                    
         DC    CL8'NSPTDIR'                                                     
         DC    CL8'NSTAFILE'                                                    
         DC    CL8'NCTFILE'                                                     
         DC    C'X'                                                             
         SPACE 1                                                                
         DC    CL8'SPOT    '       *** NOTE NET OPENED AS SPOT                  
NTFLIST  DS    0C                                                               
         DC    CL8'NSPTFILE'                                                    
         DC    CL8'NSPTDIR'                                                     
         DC    CL8'NSTAFILE'                                                    
         DC    CL8'NCTFILE'                                                     
         DC    CL8'NUNTDIR'                                                     
         DC    CL8'NUNTFIL'                                                     
         DC    C'X'                                                             
         SPACE 1                                                                
         DC    C'PRINT   '                                                      
PRFLIST  DS    0C                                                               
         DC    CL8'NPUBDIR'                                                     
         DC    CL8'NPUBFILE'                                                    
         DC    CL8'NPRTDIR'                                                     
         DC    CL8'NPRTFILE'                                                    
         DC    CL8'NCTFILE'                                                     
         DC    C'X'                                                             
*&&                                                                             
         SPACE 1                                                                
         DC    CL8'ACCOUNT'                                                     
ACFLIST  DS    0C                                                               
         DC    CL8'NACCFIL'                                                     
         DC    CL8'NCTFILE'                                                     
         DC    C'X'                                                             
         SPACE 1                                                                
         DC    CL8'ACCOUNT'                                                     
NACFLIST DS    0C                                                               
         DC    CL8'NACCDIR'                                                     
         DC    CL8'NACCMST'                                                     
         DC    CL8'NACCARC'                                                     
         DC    CL8'NCTFILE'                                                     
         DC    C'X'                                                             
         SPACE 1                                                                
         DC    CL8'MEDIA   '                                                    
MEFLIST  DS    0C                                                               
         DC    CL8'NCTFILE'                                                     
*&&UK                                                                           
         DC    CL8'NMEDDIR'                                                     
         DC    CL8'NMEDFIL'                                                     
*&&                                                                             
         DC    C'X'                                                             
*&&US                                                                           
         SPACE 2                                                                
* TABLE OF AGENCY IDS AND THEIR CORRESPONDING ID NUMBERS FOR CONFID             
*                                                                               
         DS    0H                                                               
AGYIDTAB DS    0CL4                                                             
         DC    C'DF',H'11'                                                      
         DC    C'SF',H'12'                                                      
         DC    C'DT',H'109'                                                     
         DC    C'DW',H'1467'                                                    
         DC    C'YN',H'892'                                                     
         DC    C'AR',H'1471'                                                    
         DC    C'BP',H'1023'                                                    
         DC    C'SC',H'29'                                                      
         DC    C'SG',H'2858'                                                    
         DC    C'BD',H'34'                                                      
         DC    C'BN',H'2507'                                                    
         DC    C'WW',H'1555'                                                    
         DC    C'SY',H'3571'                                                    
         DC    C'MW',H'406'                                                     
         DC    C'TH',H'3841'                                                    
         DC    C'SJ',H'0017'                                                    
         DC    C'HD',H'0283'                                                    
         DC    C'JW',H'22'                                                      
         DC    C'NW',H'87'                                                      
AGYIDENT EQU   (*-AGYIDTAB)/L'AGYIDTAB                                          
*&&                                                                             
         EJECT                                                                  
* DATA HEADLINE ROUTINE, AT ENTRY, P1=RC                                        
*                                                                               
DHEAD    NMOD1 0,**DHD**                                                        
         L     RC,0(R1)                                                         
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         LA    R2,P                                                             
         USING DHD,R2                                                           
*                                                                               
DHEAD2   MVC   DHCODE(7),=C'OUTLINE='                                           
         MVC   DHCODEU,DASHES                                                   
         MVC   DHDTYP,=C'DATA TYPE'                                             
         MVC   DHDTYPU,DASHES                                                   
*                                                                               
         LA    R0,6                R0=COUNTER FOR FIRST 6 PERIODS               
         LA    R4,DHPER1           R4=A(PERIOD POSITION)                        
         LA    R5,SVEXTDTS         R5=A(PERIOD EXTRACT DATES ENTRY)             
         USING SVEXD,R5                                                         
         MVI   BYTE,NO             SET SECOND LINE SWITCH                       
*                                                                               
DHEAD3   OC    SVEXPER,SVEXPER     TEST FOR EOT                                 
         BZ    DHEAD4              YES                                          
         GOTO1 VPEROUT,DMCB,SVEXPER,3(R4)                                       
         LA    R4,DHPER2-DHPER1(R4)                                             
         LA    R5,SVEXL(R5)                                                     
         BCT   R0,DHEAD3                                                        
*                                                                               
         CLI   BYTE,YES            TEST SECOND LINE PROCESSED                   
         BE    DHEAD4              YES                                          
         MVI   BYTE,YES            NO-SET UP TO PROCESS IT                      
         LA    R0,7                                                             
         LA    R4,DHPER7           POINT TO SECOND LINE                         
         B     DHEAD3                                                           
*                                                                               
DHEAD4   ZIC   R1,LINE                                                          
         LA    R1,5(R1)            TEST HEADLINES + 2 MORE FIT ON PAGE          
         CLM   R1,1,MAXLINES                                                    
         BL    *+8                                                              
         MVI   FORCEHED,YES        NO-FORCE A PAGE BREAK                        
         GOTO1 SPOOL,PARAS,(R8)                                                 
         GOTO1 (RF),(R1),(R8)      SKIP A LINE                                  
*                                                                               
DHEADX   XMOD1 1                                                                
         DROP  R2,R5                                                            
         SPACE 2                                                                
DASHES   DC    10C'-'                                                           
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
* BUILD TABLE OF PERIOD START/END DATES AND PERIOD NUMBERS *                    
* AT ENTRY, P1=A(GEND)                                                          
         SPACE 2                                                                
BLDPER   CSECT                                                                  
         NMOD1 0,**BLDPER                                                       
         L     RC,0(R1)            RE-ESTABLISH GEND ADDRESSABILITY             
*                                                                               
         CLI   CLTTYPE,10          TEST FOR 444 FISCAL YEAR                     
         BNE   *+12                NO                                           
         BAS   RE,BLDTAB           BUILD TABLE OF EXTRACT DATES                 
         B     BLDPER10                                                         
*                                                                               
         GOTO1 DATCON,DMCB,(3,EXST),WORK                                        
         GOTO1 DATCON,DMCB,(3,EXEND),WORK+6                                     
         MVC   WORK+4(2),=C'15'    FORCE ARBITRARY DAYS                         
         MVC   WORK+10(2),=C'15'                                                
*                                  GO BACK 26 MONTHS                            
         GOTO1 ADDAY,DMCB,(C'M',WORK),WORK,-26                                  
*              GO FORWARD 2 MONTHS                                              
         GOTO1 ADDAY,DMCB,(C'M',WORK+6),WORK+6,2                                
*              NOW ADD 1 YEAR                                                   
         GOTO1 ADDAY,DMCB,(C'Y',WORK+6),WORK+6,1                                
*                                                                               
         LA    R6,SVCLTEL                                                       
         USING BUCLTD,R6                                                        
         MVC   BYTE,BUCLTMON       INIT MONTH TYPE FROM CLT REC                 
         BAS   RE,LKOV             LOOK FOR FISCAL YEAR OVERRIDES               
         MVC   THISFIS,BYTE        SAVE THIS FISCAL YEAR TYPE                   
*                                                                               
         XC    WORK+12(16),WORK+12    SIMULATE SYSPROF AREA                     
         LA    R1,WORK+12                                                       
         MVC   2(1,R1),BYTE                                                     
         MVC   6(2,R1),BUCLTST                                                  
         ZIC   R0,BUCLTDAY         R0=DAY BITS                                  
         LA    RE,X'40'            START AT MONDAY                              
         LA    RF,1                DAY NUMBER 1                                 
         CR    R0,RE               FIND MATCH ON DAY BITS                       
         BE    BLDPER2             YES                                          
         SRL   RE,1                NEXT BIT POSITION                            
         LA    RF,1(RF)            NEXT DAY NUMBER                              
         B     *-14                                                             
*                                                                               
BLDPER2  STC   RF,8(R1)            SET DAY NUMBER IN SYSPROF                    
         L     RF,TWADCONS                                                      
         L     RF,TMOBILE-TWADCOND(RF)                                          
         ZIC   R0,BYTE             GET VALUE PUT IN SIMULATED PROFILE           
         GOTO1 (RF),DMCB,(100,WORK),((R0),AIO1),0,WORK+12                       
         DROP  R6                                                               
         SPACE 1                                                                
* NOW WORK OUT PERIOD NUMBERS - FIND PLACES WHERE YEAR CHANGES                  
         SPACE 1                                                                
         L     R5,AIO2                                                          
         XC    0(256,R5),0(R5)                                                  
         L     R4,AIO1                                                          
*                                                                               
BLDPER3  DS    0H                                                               
         BAS   RE,CKNEWYR          TEST START OF NEW YEAR                       
         BE    BLDPER4             YES                                          
         LA    R4,4(R4)                                                         
         B     BLDPER3                                                          
*                                                                               
BLDPER4 DS     0H                  SET INITIAL YEAR                             
         ZIC   R3,2(R4)                                                         
         SRL   R3,1                                                             
*                                                                               
BLDPER6  DS    0H                                                               
         LA    R1,1                FOR PERIOD NUMBERS WITHIN YEAR               
*                                                                               
BLDPER8  DS    0H                                                               
         STC   R3,0(R5)            YEAR                                         
         STC   R1,1(R5)            MONTH                                        
         MVC   2(4,R5),0(R4)       START-END OF PERIOD                          
         LA    R5,6(R5)                                                         
         LA    R4,4(R4)                                                         
         CLI   0(R4),X'FF'         TEST E-O-L                                   
         BE    BLDPER10                                                         
         LA    R1,1(R1)                                                         
         BAS   RE,CKNEWYR          TEST NEW YEAR                                
         BNE   BLDPER8             NO - CONTINUE                                
         LA    R3,1(R3)            BUMP YEAR                                    
         B     BLDPER6                                                          
         EJECT                                                                  
* NOW BUILD 12 BYTE ENTRIES IN EXTDATES                                         
*          PERIOD NUMBER (2)                                                    
*          PERIOD START  (2)                                                    
*          PERIOD END    (2)                                                    
*          PERIOD START  (3)                                                    
*          PERIOD END    (3)                                                    
         SPACE 1                                                                
BLDPER10 L     R5,AIO2                                                          
         XC    SVEXTDTS,SVEXTDTS                                                
         LA    R4,SVEXTDTS                                                      
         USING SVEXD,R4                                                         
*                                                                               
BLDPER12 CLC   0(2,R5),EXST        MATCH PERIOD START                           
         BE    BLDPER14                                                         
         BL    *+6                                                              
         DC    H'0'                                                             
         LA    R5,6(R5)                                                         
         B     BLDPER12                                                         
*                                                                               
BLDPER14 MVC   SVEXPER(6),0(R5)                                                 
         GOTO1 DATCON,DMCB,(2,SVEXSTC),(3,SVEXSTB)                              
         GOTO1 DATCON,DMCB,(2,SVEXENDC),(3,SVEXENDB)                            
*                                                                               
         LA    R4,SVEXL(R4)                                                     
         LA    R5,6(R5)                                                         
         CLC   0(2,R5),EXEND                                                    
         BNH   BLDPER14                                                         
*                                                                               
         SH    R4,=Y(SVEXL)                      BACK UP TO LAST DATE           
         MVC   SVEXTNPE,SVEXPER                  SAVE END PERIOD                
         MVC   SVEXTNDP,SVEXENDC                 SAVE END PACKED                
         MVC   SVEXTNDB,SVEXENDB                 SAVE END BINARY                
         GOTO1 DATCON,DMCB,(2,SVEXENDC),SVEXTEND SAVE END YYMMDD                
*                                                                               
         LA    R4,SVEXTDTS                                                      
         MVC   SVEXTSPE,SVEXPER                  SAVE START PERIOD              
         MVC   SVEXTSTP,SVEXSTC                  SAVE START PACKED              
         MVC   SVEXTSTB,SVEXSTB                  SAVE START BINARY              
         GOTO1 DATCON,DMCB,(2,SVEXSTC),SVEXTST   SAVE START YYMMDD              
*                                                                               
BLDPERX  XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
* SUB-ROUTINE TO BUILD A TABLE OF PERIOD ENTRIES FOR 444 EXTRACT                
*                                                                               
* TABLE ENTRIES = PERIOD(2),COMPRESSED PERIOD START/END (4)                     
*                                                                               
BLDTAB   NTR1                                                                   
         MVC   FULL(1),EXST        YEAR FROM EXTRACT                            
         MVC   FULL+1(2),CLTSTART  MONTH DAY=FISCAL START                       
         GOTO1 DATCON,DMCB,(3,FULL),DUB                                         
         GOTO1 GETDAY,DMCB,DUB,FULL                                             
         ZIC   R0,0(R1)            DAY OF FISCAL START/YEAR                     
*                                                                               
         LA    R2,1                R1=DAY NUMBER                                
         LA    RE,X'40'            DAY BIT MASK                                 
BLDTAB2  EX    RE,TSTDAY                                                        
         BO    BLDTAB4                                                          
         SRL   RE,1                                                             
         LA    R2,1(R2)            INCREMENT DAY NUMBER                         
         B     BLDTAB2                                                          
*                                                                               
TSTDAY   TM    CLTDAY,0                                                         
*                                                                               
BLDTAB4  SR    R2,R0               ADJUST TO ACTUAL FISCAL START                
         GOTO1 ADDAY,DMCB,DUB,DUB,(R2)                                          
*                                                                               
         ZIC   R0,EXST+1           GET START PERIOD IN FISCAL YEAR              
         BCTR  R0,0                R0=N'PERIODS TO START                        
         MH    R0,=H'28'           CONVERT TO DAYS                              
         GOTO1 ADDAY,DMCB,DUB,DUB,(R0)                                          
         MVC   WORK(6),DUB         SET START DATE                               
*                                                                               
BLDTAB6  ZIC   R2,EXEND+1          GET END MONTH                                
         ZIC   RF,EXST+1           AND START MONTH                              
         CR    R2,RF                                                            
         BNL   *+8                                                              
         LA    R2,13(R2)                                                        
         LA    R2,1(R2)                                                         
         SR    R2,RF               R2=N'PERIODS IN EXTRACT                      
         LA    R2,1(R2)            ADD ONE MORE TO MAKE BLDPER10 WORK           
*                                                                               
         L     R5,AIO2                                                          
         XC    0(256,R5),0(R5)     CLEAR TABLE AREA                             
         MVC   HALF,EXST           INITIALIZE PERIOD                            
*                                                                               
BLDTAB8  GOTO1 ADDAY,DMCB,WORK,WORK+6,27                                        
         MVC   0(2,R5),HALF        SET PERIOD IN ENTRY                          
         GOTO1 DATCON,DMCB,WORK,(2,2(R5))                                       
         GOTO1 DATCON,DMCB,WORK+6,(2,4(R5))                                     
         LA    R5,6(R5)            NEXT ENTRY POSITION                          
         GOTO1 ADDAY,DMCB,WORK+6,WORK,1                                         
         ZIC   RE,HALF             RE=YEAR                                      
         ZIC   RF,HALF+1           RF=MONTH                                     
         LA    RF,1(RF)            NEXT MONTH                                   
         CH    RF,=H'13'           TEST IF PAST LAST PERIOD IN YEAR             
         BNH   *+12                                                             
         LA    RF,1                YES-RESET MONTH TO ONE                       
         LA    RE,1(RE)            GO TO NEXT YEAR                              
         STC   RE,HALF             SET NEW YEAR/MONTH                           
         STC   RF,HALF+1                                                        
         BCT   R2,BLDTAB8                                                       
*                                                                               
BLDTABX  XIT1                                                                   
         EJECT                                                                  
*  FIND START OF NEW YEAR                                                       
*     1) A PERIOD THAT SPANS YEAR CHANGE                                        
*        AND BEGINS NO FURTHER AWAY                                             
*        FROM 12/31 THAN IT ENDS                                                
*     2) A PERIOD THAT STARTS BEFORE 1/14                                       
         SPACE 1                                                                
CKNEWYR  DS    0H                                                               
         MVC   DUB(4),0(R4)                                                     
         NI    DUB,X'01'           STRIP YEAR                                   
         CLC   DUB(2),NEWYRLO                                                   
         BL    CKNYYES                                                          
*                                                                               
         CLC   DUB(2),PDDEC                                                     
         BNH   CKNYNO                                                           
*                                                                               
         NI    DUB+2,X'01'                                                      
         CLC   DUB+2(2),PDDEC                                                   
         BH    CKNYNO                                                           
*                                                                               
         NI    DUB+1,X'1F'         ISOLATE DAY                                  
         ZIC   RF,DUB+1                                                         
         LA    R0,30                                                            
         SR    R0,RF                                                            
         BNP   CKNYYES             STARTS ON 30TH OR 31ST                       
         STC   R0,DUB+4                                                         
*                                                                               
         NI    DUB+3,X'1F'         ISOLATE DAY                                  
         CLC   DUB+4(1),DUB+3                                                   
         BNH   CKNYYES                                                          
*                                                                               
CKNYNO   DS    0H                                                               
         LTR   RE,RE                                                            
         BR    RE                                                               
CKNYYES  DS    0H                                                               
         SR    R0,R0                                                            
         BR    RE                                                               
*                                                                               
*                                                                               
NEWYRLO  DC    X'002E'             JAN14                                        
PDDEC    DC    X'0180'             DEC00                                        
         EJECT                                                                  
* SUB-ROUTINE TO LOOK FOR FISCAL MONTH TYPE OVERRIDES BY SYSTEM/MEDIA           
*                                                                               
* MONTH TYPE VALUE IN BYTE WILL BE REPLACED IF OVERRIDE LOCATED                 
*                                                                               
LKOV     ST    RE,SAVERE                                                        
         SR    R0,R0                                                            
         ICM   R0,1,SVNFOV         R0=N'FISCAL YEAR OVERRIDES                   
         BZ    LKOVX               NONE                                         
*                                                                               
         L     R2,ARULDATA                                                      
         USING QRD,R2                                                           
         XC    FULL,FULL                                                        
         LA    R1,FULL                                                          
         USING SVFOVD,R1                                                        
         MVC   SVFOVSYS,QROVSYS    BUILD SEARCH ENTRY                           
         MVC   SVFOVCOD,QRSYSTYP                                                
         MVC   SVFOVMED,QRMED                                                   
*                                                                               
         LA    R1,SVFOV            R1=A(FISCAL YEAR OVERRIDE TABLE)             
*                                                                               
LKOV2    CLC   SVFOVSYS(2),FULL    TEST MATCH ON OVSYS AND SYS CODE             
         BNE   LKOV4               NO                                           
         CLI   SVFOVMED,0          TEST IF ENTRY IS ALL MEDIA OVERRIDE          
         BE    LKOV3               YES                                          
         CLC   SVFOVMED,FULL+2     TEST FOR ADDITIONAL MATCH ON MEDIA           
         BE    LKOV3               YES                                          
         B     LKOV4                                                            
*                                                                               
LKOV3    MVC   BYTE,SVFOVMON                                                    
*                                                                               
LKOV4    LA    R1,SVFOVL(R1)                                                    
         BCT   R0,LKOV2                                                         
*                                                                               
LKOVX    L     RE,SAVERE                                                        
         BR    RE                                                               
         DROP  R1,R2                                                            
         SPACE 2                                                                
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* TABLE OF RULE TYPES AND THEIR PRINT ROUTINES                                  
* (COVERED BY PRTTABD)                                                          
*                                                                               
PRTTAB   DS    0XL(PRTTABL)                                                     
       ++INCLUDE BUFIL40PTB                                                     
         DC    X'FF'                                                            
         SPACE 2                                                                
* TABLE OF SYSTEM/MEDIA FOR PRINTING IN HOOK ROUTINE                            
* (COVERED BY SYSTABD)                                                          
*                                                                               
SYSTAB   DS    0CL(SYSTABL)                                                     
*                                                                               
*&&US                                                                           
         DC    X'02',C'S',C'T',AL1(3),X'00'                                     
         DC    CL10'SPOT',CL10'TELEVISION'                                      
*                                                                               
         DC    X'02',C'S',C'R',AL1(3),X'00'                                     
         DC    CL10'SPOT',CL10'RADIO'                                           
*                                                                               
         DC    X'02',C'S',C'X',AL1(3),X'00'                                     
         DC    CL10'SPOT',CL10'NET RADIO'                                       
*                                                                               
         DC    X'02',C'S',C'N',AL1(3),X'00'                                     
         DC    CL10'SPOT',CL10'NETWORK'                                         
*                                                                               
         DC    X'02',C'S',C'C',AL1(3),X'00'                                     
         DC    CL10'SPOT',CL10'COMBINED'                                        
*                                                                               
         DC    X'03',C'N',C'N',AL1(3),X'00'                                     
         DC    CL10'NETWORK',CL10'TELEVISION'                                   
*                                                                               
         DC    X'04',C'P',C'N',AL1(3),X'00'                                     
         DC    CL10'PRINT',CL10'NEWSPAPERS'                                     
*                                                                               
         DC    X'04',C'P',C'M',AL1(3),X'00'                                     
         DC    CL10'PRINT',CL10'MAGAZINES'                                      
*                                                                               
         DC    X'04',C'P',C'O',AL1(3),X'00'                                     
         DC    CL10'PRINT',CL10'OUTDOOR'                                        
*                                                                               
         DC    X'04',C'P',C'S',AL1(3),X'00'                                     
         DC    CL10'PRINT',CL10'SUPPLEMENT'                                     
*                                                                               
         DC    X'04',C'P',C'T',AL1(3),X'00'                                     
         DC    CL10'PRINT',CL10'TRADE'                                          
*                                                                               
         DC    X'04',C'P',C'I',AL1(3),X'00'                                     
         DC    CL10'PRINT',CL10'INTERACTIV'                                     
*                                                                               
         DC    X'06',C'P',C' ',AL1(2),AL1(SYSTPMED)                             
         DC    CL10'PRODUCTION',CL10' '                                         
*                                                                               
         DC    X'06',C'I',C' ',AL1(2),AL1(0)                                    
         DC    CL10'INTERAGY',CL10' '                                           
*                                                                               
*&&                                                                             
*&&UK                                                                           
*                                                                               
         DC    X'04',C'M',C' ',AL1(2),AL1(SYSTPMED)                             
         DC    CL10'MEDLINE',CL10' '                                            
*                                                                               
         DC    X'06',C'P',C' ',AL1(2),AL1(SYSTPMED)                             
         DC    CL10'PRODUCTION',CL10' '                                         
*                                                                               
*&&                                                                             
SYSENTS  EQU   (*-SYSTAB)/SYSTABL                                               
         EJECT                                                                  
* REPORT HEADLINE SPECS                                                         
*                                                                               
HEDSPECS DS    0D                                                               
*                                                                               
*&&US*&& SSPEC H1,2,C'ABC - ACCOUNT BUDGET SYSTEM'                              
*&&UK*&& SSPEC H1,2,C'ABS - ACCOUNT BUDGET SYSTEM'                              
         SSPEC H2,2,C'---------------------------'                              
         SSPEC H1,46,C'DATA EXTRACT REPORT'                                     
         SSPEC H2,46,C'-------------------'                                     
         SSPEC H1,75,AGYNAME                                                    
         SSPEC H2,75,AGYADD                                                     
         SSPEC H4,2,C'CLIENT'                                                   
         SSPEC H5,2,C'PRODUCT'                                                  
         SSPEC H6,2,C'PLAN'                                                     
         SSPEC H7,2,C'OUTLINE'                                                  
         SSPEC H4,75,REPORT                                                     
         SSPEC H4,87,RUN                                                        
         SSPEC H5,75,REQUESTOR                                                  
         SSPEC H5,93,PAGE                                                       
*                                                                               
         DC    X'00'                                                            
         EJECT                                                                  
* DSECT TO COVER RULE PRINTING TABLE                                            
*                                                                               
PRTTABD  DSECT                                                                  
*                                                                               
PRTDSP   DS    AL2                 DISPLACEMENT IN RULE ENTRY                   
PRTRTN   DS    AL3                 PRINT ROUTINE ADDRESS                        
PRTDESC  DS    CL4                 DESCRIPTION                                  
PRTNEXT  EQU   *                                                                
PRTTABL  EQU   *-PRTTABD           TABLE ENTRY LENGTH                           
         SPACE 2                                                                
* DSECT TO COVER SYSTEM/MEDIA TABLE                                             
*                                                                               
SYSTABD  DSECT                                                                  
SYSTOV   DS    X                   OVERLAY SYSTEM NUMBER                        
SYSTSYS  DS    C                   SYSTEM CHARACTER                             
SYSTMED  DS    C                   MEDIA                                        
SYSTCOML DS    X                   COMPARE LENGTH                               
SYSTIND  DS    X                   INDICATORS                                   
SYSTPMED EQU   X'80'               X'80'=PRINT MEDIA CODE                       
SYSTSNAM DS    CL10                SYSTEM NAME                                  
SYSTMNAM DS    CL10                MEDIA NAME                                   
SYSTABL  EQU   *-SYSTABD           TABLE ENTRY LENGTH                           
         SPACE 2                                                                
* DSECT TO COVER DATA HEADLINES                                                 
*                                                                               
DHD      DSECT                                                                  
         DS    C                                                                
DHCODE   DS    CL8                                                              
         DS    CL2                 SPARE                                        
DHDTYP   DS    CL9                                                              
         DS    CL4                                                              
DHPER1   DS    CL13                FIRST PERIOD                                 
         DS    C                                                                
DHPER2   DS    CL13                SECOND PERIOD                                
         DS    C                                                                
DHPER3   DS    CL13                                                             
         DS    C                                                                
DHPER4   DS    CL13                                                             
         DS    C                                                                
DHPER5   DS    CL13                                                             
         DS    C                                                                
DHPER6   DS    CL13                                                             
         DS    C                                                                
         ORG   DHPER1+L'P                                                       
DHPER7   DS    CL13                                                             
         DS    C                                                                
DHPER8   DS    CL13                                                             
         DS    C                                                                
DHPER9   DS    CL13                                                             
         DS    C                                                                
DHPER10  DS    CL13                                                             
         DS    C                                                                
DHPER11  DS    CL13                                                             
         DS    C                                                                
DHPER12  DS    CL13                                                             
         DS    C                                                                
DHPER13  DS    CL13                13TH PERIOD IF 444 FISCAL YEAR               
         ORG   DHCODE+L'P                                                       
DHCODEU  DS    CL7                 UNDERSCORE CODE                              
         ORG   DHDTYP+L'P                                                       
DHDTYPU  DS    CL9                 UNDERSCORE DATA TYPE                         
         EJECT                                                                  
       ++INCLUDE BUFILWORKD                                                     
         EJECT                                                                  
       ++INCLUDE BUEXTWORKD                                                     
         EJECT                                                                  
       ++INCLUDE BUPPERD                                                        
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDCNTRL2                                                       
       ++INCLUDE DDREPMASTD                                                     
       ++INCLUDE DMDTFIS                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'094BUFIL40   05/01/02'                                      
         END                                                                    
