*          DATA SET NEWRI42    AT LEVEL 061 AS OF 05/01/02                      
*PHASE T32042A,+0                                                               
*INCLUDE COVAIL                                                                 
         TITLE 'T32042 - BRMECOST UPDATE/REPORT'                                
         PRINT NOGEN                                                            
************************************************************                    
* EQUALIZED COST UPDATE/REPORT                             *                    
*                                                          *                    
* THIS PROGRAM CALCULATES THE ASSIGNED COST FOR UNITS      *                    
*                                                          *                    
*                                                          *                    
* GLOBALS: RA - A(TWA)                                     *                    
*          RB - BASE REG                                   *                    
*          RC - GEND                                       *                    
*          R9 - NETWORK SYSTEM DSECT                       *                    
*          R8 - A(DSECT FOR SPOOL PRINTING)                *                    
*          R7 - WORKING STORAGE/ANETWS1                    *                    
*          R6 - BASE REG FOR NDDEMBLK,DEDBLOCK/ANETWS4     *                    
*                                                          *                    
************************************************************                    
T32042   CSECT                                                                  
         NMOD1 0,**NEBC**,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         LA    RA,4095(RB)                                                      
         LA    RA,1(RA)                                                         
         USING T32042,RB,RA         RA = 2ND BASE REG                           
         LA    R1,HDHOOK                                                        
         ST    R1,HEADHOOK                                                      
         LA    R1,MYSPECS                                                       
         ST    R1,SPECS                                                         
         L     R7,ANETWS1     R7 - ANETWS1/ANETWS2 =WORKING STORAGE             
         USING WORKD,R7                                                         
**       L     R6,ANETWS4     R6 - ANETWS4+1000=NDDEMBLK/DEDBLOCK               
**       A     R6,=F'1000'         (ANETWS4=NETFILTERS)                         
**       ST    R6,NBADEM           SET IN NEWRI41                               
                                                                                
         L     R1,=A(MYNTBUF)      NETWORK BUFFER                               
         ST    R1,NBANBUFF                                                      
         LR    RE,R1                                                            
         SR    RF,RF                                                            
         A     RF,=F'4000'                                                      
         XCEF                                                                   
                                                                                
         L     R6,NBADEM                                                        
         USING NDDEMBLK,R6                                                      
                                                                                
         CLI   MODE,PRINTREP                                                    
         BNE   EXIT                                                             
         B     INIT                                                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
* INITIALIZATION                                                                
         SPACE                                                                  
INIT     DS    0H                                                               
         SPACE                                                                  
         LA    RE,WRKSTRT                                                       
         LA    RF,WRKEND-WRKSTRT                                                
         XCEF                                                                   
         L     R2,ATWA                                                          
         USING T320FFD,R2                                                       
         MVI   UPDATFLG,C'N'                                                    
         CLI   SPLTST,C'Y'                     TEST RUN OPTION                  
         BE    *+8                                                              
         MVI   UPDATFLG,C'Y'                                                    
         DROP  R2                                                               
         ZAP   PKGCSTSV,=P'0'                                                   
         ZAP   RECSFSRT,=P'0'                  (NOT PRINTED/FOR DEBUG)          
         ZAP   PAKWRK,=P'0'                                                     
         ZAP   GRPTOTS,=P'0'                                                    
         ZAP   CPP,=P'0'                                                        
         ZAP   ACTTOTS,=P'0'                                                    
         ZAP   ASGNTOTS,=P'0'                                                   
         ZAP   TEMPASGN,=P'0'                                                   
         ZAP   PENNIES,=P'0'                                                    
         ZAP   INTGTOT,=P'0'                                                    
         ZAP   TEMPGRP,=P'0'                                                    
         ZAP   NPROGS,=P'0'                                                     
         ZAP   ASIGTOT,=P'0'                                                    
         LA    RE,PKGLIST                                                       
         L     RF,=F'3000'                                                      
         XCEF                                                                   
         SPACE                                                                  
         MVC   DBFILE,=C'NTI'    SET UP DEDBLOCK/NO DEMO SCREEN FIELD           
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELMED,C'N'                                                    
         MVI   DBSELSRC,C'N'                                                    
         SPACE                                                                  
         EJECT                                                                  
*                                                                               
         MVI   NBSELUOP,C'A'                                                    
         MVI   NBESTOPT,C'A'                                                    
         CLI   PFBOPT,C'Y'                                                      
         BNE   *+8                                                              
         MVI   NBESTOPT,C'M'       YES/GIVE ME PFBS                             
         CLI   DEMOPT,C'A'                                                      
         BNE   PAS04                                                            
         MVI   NBACTOPT,C'Y'                                                    
         MVI   NBESTOPT,0                                                       
PAS04    MVI   NBDATA,C'U'         SELECT UNIT RECORDS                          
         MVI   NBSEQ,C'Q'          TRUE PROGRAM                                 
         CLI   PKGOPT,C'Y'         IF PKG OPTION                                
         BNE   PAS05                                                            
         MVI   NBDATA,C'B'         GET ALL RECORDS                              
         MVI   NBRESUME,NBPROCPK                                                
         SPACE                                                                  
PAS05    NETGO NSNETIO,DMCB,NETBLOCK            GET  RECORD                     
         SPACE                                                                  
         CLI   NBMODE,NBREQLST                                                  
         BNE   PAS06                                                            
         CLI   FRST,0              IF NO RECORDS                                
         BE    NORECX              EXIT                                         
         BAS   RE,CALCGRP                                                       
         B     FRM20                                                            
PAS06    CLI   NBMODE,NBPROCPK                                                  
         BNE   *+8                                                              
         BAS   RE,GETPKG                                                        
         CLI   NBMODE,NBPROCUN                                                  
         BNE   PAS05                                                            
         CLI   UNASIGND,C'Y'       IF UNASSIGNED ONLY                           
         BNE   PAS08                                                            
         OC    NBASSIGN,NBASSIGN   SKIP UNITS WITH ASSIGNED DOLLARS             
         BNZ   PAS05                                                            
PAS08    B     PASUNT                                                           
         SPACE                                                                  
NORECX   DS    0H                          NO UNITS FOUND/EXIT                  
         MVC   P+1(14),=C'NO UNITS FOUND'                                       
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   HALF,C'U'           UNLOCK UPDATIVE SOON                         
         BAS   RE,LOCKEM                                                        
         B     EXIT                                                             
         EJECT                                                                  
         SPACE                                                                  
PASUNT   DS    0H                                                               
         L     R3,ANETWS3                                                       
         USING SORTRECD,R3                                                      
         XC    0(SRTRECLN,R3),0(R3)                                             
         MVC   SRTNET,NBACTNET                                                  
         MVC   SRTDTE,NBACTDAT                                                  
         MVC   SRTDSUB,NBACTSUB                                                 
         MVC   SRTDAY,NBDAYNAM                                                  
         GOTO1 UNTIME,DMCB,NBTIME,SRTTIME                                       
         MVC   SRTPRGCD,NBACTPRG                                                
         MVC   SRTPRGNM,NBPROGNM                                                
         CLI   CPMS,C'Y'                                                        
         BNE   PAS10                                                            
         MVC   SRTGRP,NDESTDEM+4        IMPS                                    
         CLI   DEMOPT,C'A'                                                      
         BNE   PAS11                                                            
         MVC   SRTGRP,NDACTDEM+4                                                
         B     PAS11                                                            
PAS10    MVC   SRTGRP+2(2),NDESTDEM+2        GRP (1DEC)                         
         CLI   DEMOPT,C'A'                                                      
         BNE   *+10                                                             
         MVC   SRTGRP+2(2),NDACTDEM+2                                           
PAS11    ICM   R1,15,NBACTUAL           ACTUAL DOLS (CENTS)                     
         CLI   SPCLOPT,C'Y'             INCLUDE SPECIAL CHARGES                 
         BNE   PAS11C                                                           
         ICM   RE,15,NBSPCHRG                                                   
         AR    R1,RE                                                            
PAS11C   STCM  R1,15,SRTACTDL                                                   
         CLI   ASGNDOVR,C'Y'            ASSIGNED COST OVERRIDES                 
         BE    PAS12                    Y=REALLOCATE OVERRIDES                  
         TM    NBUNST3,X'80'              IS IT OVERRIDE                        
         BNO   PAS12                                                            
         MVI   SRTUNTK,C'*'               THEN MARK AS SUCH                     
         MVC   SRTUNTK+1(4),NBASSIGN      AND STORE OVERRIDE ASSGN              
         ICM   R1,15,NBASSIGN                                                   
         CVD   R1,DUB                                                           
         AP    ASIGTOT,DUB                                                      
         B     *+10                                                             
PAS12    MVC   SRTUNTK,NBKEY            UNT REC KEY                             
         MVC   SRTINTEG,NBINTEG                                                 
         MVC   SRTLEN,NBLEN                                                     
         B     FRM12                                                            
         DROP  R3                                                               
         SPACE                                                                  
         EJECT                                                                  
*                                                                               
FRM12    AP    RECSFSRT,=P'1'                                                   
         CLI   FRST,C'N'                                                        
         BE    FRM13                                                            
*                                                                               
         MVI   FRST,C'N'                   FIRST TIME                           
         AP    NPROGS,=P'1'                                                     
*                                                                               
         GOTO1 =V(COVAIL),DMCB,C'GET',480000,480000                             
         L     R4,4(R1)                                                         
         L     R2,8(R1)                                                         
         LTR   R4,R4                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    R4,ASVAREA                                                       
         C     R2,=F'480000'                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R5,ASVAREA                                                       
         USING SORTRECD,R5                                                      
         MVC   SORTREC(SRTRECLN),0(R3)                                          
         MP    PKGCSTSV,=P'100'   PKG COST IN DOLS/WE NEED CENTS                
         LA    R1,NDDEMBLK         GET DEMONAME                                 
         ST    R1,ANTDMBLK                                                      
         LA    R1,DBLOCK                                                        
         ST    R1,ADEDBLK                                                       
         NETGO NVDEMCON,DMCB,(0,ANTDMBLK),ADEDBLK,(7,DEMONAME)                  
         B     FRM15                                                            
         SPACE                                                                  
FRM13    DS    0H                                                               
         CLC   SRTPRGCD,11(R3)     IS IT SAME PROGRAM                           
         BE    *+8                                                              
         BAS   RE,CALCGRP          NO/GET AVERAGE GRP AND PLUG IN               
         AP    NPROGS,=P'1'                                                     
         LA    R5,SRTRECLN(R5)                                                  
         MVC   SORTREC(SRTRECLN),0(R3)                                          
         SPACE                                                                  
FRM15    DS    0H                                                               
         BAS   RE,ADDGRPS                 ADD TO GRP TOTS                       
         CLI   SRTUNTK,C'*'        IF ASSIGNED COST OVERRIDE                    
         BE    PAS05               LEAVE OUT OF TOTALS                          
         BAS   RE,ADDOLS                  ADD TO DOLLAR TOTS                    
         B     PAS05                                                            
         EJECT                                                                  
*                                                                               
*  CALCULATE GRP FOR UNITS BY GETTING AVG GRP FOR PROGRAM                       
*  THEN MULTIPLYING AVEGRP BY LENGTH OF UNIT BASED ON 30                        
*  30 SECOND UNIT = 1, 15 SECOND = 1/2 AND SO ON.                               
*  R5 = ADDR OF LAST PROG REC IN SAVE AREA                                      
*                                                                               
*                                                                               
CALCGRP  NTR1                                                                   
         XC    DUB,DUB                                                          
         MVC   DUB+4(4),NPROGS                                                  
         CVB   R2,DUB              R2=NO OF PROGRAMS                            
         ST    R2,BNPROGS                                                       
         LTR   R2,R2                                                            
         BZ    CLCX                                                             
*                                                                               
         ZAP   PAKWRK,TEMPGRP        GET ROUNDED AVERAGE GRP                    
         MP    PAKWRK,=P'10'                                                    
         DP    PAKWRK,NPROGS                                                    
         AP    PAKWRK(12),=P'5'                                                 
         DP    PAKWRK(12),=P'10'                                                
         MVC   DUB,PAKWRK+2                                                     
         CVB   R4,DUB              R4 = AVERAGE GRP                             
*                                                                               
*                                  BACK UP TO PROG START IN SAVEAREA            
         C     R2,=F'1'                                                         
         BE    CLC5                                                             
         BCTR  R2,0                                                             
         LA    R1,SRTRECLN                                                      
         SR    R0,R0                                                            
         M     R0,=F'-1'                                                        
         AR    R5,R1                                                            
         BCT   R2,*-2                                                           
*                                                                               
CLC5     DS    0H                  CALCULATE AND SEED GRP                       
         L     R2,BNPROGS                                                       
CLC10    CLI   EQIVAL,0            ARE WE EQUIVALENCING                         
         BNE   CLC12                                                            
         LR    R1,R4               NO/SET AVERAGE VALUE TO REC                  
         B     CLC15                                                            
*                                                                               
CLC12    XC    FULL,FULL                                                        
         MVC   FULL+3(1),EQIVAL                                                 
         ZIC   R1,SRTLEN                                                        
         MH    R1,=H'100'                                                       
         SR    R0,R0                                                            
         D     R0,FULL                                                          
         SR    R0,R0                                                            
         MR    R0,R4                                                            
         SR    R0,R0                                                            
         D     R0,=F'100'                                                       
CLC15    CLI   SRTUNTK,C'*'        IS IT ASSIGN COST OVERRIDE                   
         BE    CLC17               YES/SKIP                                     
         STCM  R1,15,SRTAVGRP      NO/SEED INTO REC                             
         CVD   R1,DUB                                                           
         AP    GRPTOTS,DUB         ADD TO GRP TOTALS                            
CLC17    LA    R5,SRTRECLN(R5)                                                  
         BCT   R2,CLC10                                                         
         ZAP   NPROGS,=P'0'                                                     
         ZAP   TEMPGRP,=P'0'                                                    
CLCX     XIT1                                                                   
         EJECT                                                                  
         SPACE                                                                  
FRM20    MVI   FORCEHED,C'Y'                                                    
         CLI   PKGOPT,C'Y'         IF USING PKG COST                            
         BNE   *+8                                                              
         BAS   RE,SETPCST          SET PKG COST INTO ACTTOTS                    
         LA    R5,SRTRECLN(R5)     BUMP TO END OF SAVE AREA                     
         MVI   0(R5),X'FF'                                                      
         BAS   RE,GETCPP                                                        
         CLI   DUB,C'X'            CHK ERROR                                    
         BE    FRM40                                                            
         L     R5,ASVAREA          RESET TO START OF SAVE AREA                  
FRM25    CLI   0(R5),X'FF'                                                      
         BE    FRM30                                                            
         BAS   RE,GETASGND                                                      
         BAS   RE,UPDTREC                                                       
         LA    R5,SRTRECLN(R5)                                                  
         B     FRM25                                                            
         SPACE                                                                  
FRM30    DS    0H                                                               
         BAS   RE,TOTTRTN                                                       
FRM40    L     R2,ASVAREA                                                       
         GOTO1 =V(COVAIL),DMCB,C'FREE',(R2),480000                              
FRMX     DS    0H                                                               
         MVI   HALF,C'U'           UNLOCK UPDATIVE SOON                         
         BAS   RE,LOCKEM                                                        
         B     EXIT                                                             
         EJECT                                                                  
****************************************************************                
* DO NOT USE R3,R5 IN CALCS BELOW/ USING STAT POINT TO RECORDS *                
*    USED IN EARLIER RTNS AS WELL AS IN CALCS                  *                
****************************************************************                
         SPACE                                                                  
ADDGRPS  NTR1                                                                   
         ICM   R1,15,SRTGRP                                                     
         CVD   R1,DUB                                                           
         AP    TEMPGRP,DUB                                                      
         B     EXIT                                                             
         SPACE                                                                  
ADDOLS   NTR1                                                                   
         ICM   R1,15,SRTACTDL                                                   
         CVD   R1,DUB                                                           
         AP    ACTTOTS,DUB                                                      
         B     EXIT                                                             
         SPACE                                                                  
GETCPP   NTR1                   ACTTOTS(2DEC)/GRP(1DEC) = CPP(2DEC)             
*                                                                               
*                                      MULT ACTOT BY 10 TO IGNORE               
         ZAP   PAKWRK,ACTTOTS          DEC OF GRP SO AFTER DIV                  
         MP    PAKWRK,=P'10'                                                    
*                                                                               
         ZAP   FULL,=P'0'           CHECK IF THERE ARE GRPS                     
         CP    FULL,GRPTOTS                                                     
         BNE   CPP5                                                             
         MVC   P(25),=C'*** NO GRPS FOR UNITS ***'                              
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   DUB,C'X'            SET ERROR                                    
         B     EXIT                                                             
*                                                                               
CPP5     DP    PAKWRK,GRPTOTS                                                   
         CLI   CPMS,C'Y'           IF BASED ON IMPRESSIONS                      
         BNE   CPP7                                                             
         AP    PAKWRK(12),=P'5'                                                 
         DP    PAKWRK(12),=P'10'                                                
         ZAP   CPP,PAKWRK(10)                                                   
         B     EXIT                                                             
*CPP7     MVC   WORK(12),PAKWRK                                                 
*         ZAP   PAKWRK,WORK(12)                                                 
*         DP    PAKWRK,=P'100'      PUT DOLS IN PAKWRK/PENYS IN +14             
*        ZAP   CPP,PAKWRK(14)      SET ONLY DOLS IN CPP                         
*        MP    CPP,=P'100'         BUT WILL EDIT FOR PENYS                      
CPP7     ZAP   CPP,PAKWRK(12)      SET DOLS +PENNIES PER BRISTOL                
         B     EXIT                                                             
         SPACE 2                                                                
GETASGND NTR1                    UNIT GRP X ACTTOTS/GRPTOTS=ASGN COST           
*                                  (1DEC)   (2DEC)  (1DE)    (2DEC)             
         CLI   SRTUNTK,C'*'        IS IT OVERRIDE                               
         BE    EXIT                IF YES/EXIT                                  
*                                                                               
         ZAP   PAKWRK,ACTTOTS             DIV ACTOTS BY 10 TO GET               
         DP    PAKWRK,=P'10'                   2 DEC IN PKWRK                   
         MVC   WORK(14),PAKWRK                                                  
         ZAP   PAKWRK,WORK(14)                                                  
*                                                                               
         ICM   R1,15,SRTAVGRP     UNIT GRP(SRTGRP) X ACTOTS(IN PAKWRK)          
         CVD   R1,DUB                                                           
         MP    PAKWRK,DUB+4(4)                                                  
*                                                                               
         MP    PAKWRK,=P'10'     TO BALANCE DIFF IN DEC IN DIVISION             
         DP    PAKWRK,GRPTOTS                                                   
         CLI   ROUNDFLG,C'Y'       CHK ROUNDING OPTION      BRISTOL             
         BNE   GAS10                                        BRISTOL             
         MVC   WORK(12),PAKWRK     GIVE EM PENNIES TOO                          
         ZAP   PAKWRK,WORK(12)                                                  
         B     GASX10                                                           
*                                                                               
GAS10    MVC   WORK(12),PAKWRK                                                  
         ZAP   PAKWRK,WORK(12)    SAVE PNYS/SET ASGN COST WITHOUT PENY          
         SPACE                                                                  
         DP    PAKWRK,=P'100'  (PUTS DOLS IN PAKWRK/PENYS IN PAKWRK+14)         
         ZAP   TEMPASGN,PAKWRK(14)   SET DOLS(NO PENNIES) TO TEMPASGN           
         MP    TEMPASGN,=P'100'      BUT WE EDIT FOR PENYS                      
         CLI   ROUNDFLG,C'Y'       CHK ROUNDING OPTION                          
         BE    GASX                IF YES/SKIP PENNY COUNT                      
*                                                                               
         AP    PENNIES,PAKWRK+14(2)  AND KEEP COUNT OF PENNIES                  
         CP    PENNIES,=P'99'                                                   
         BNH   GASX                                                             
         AP    TEMPASGN,=P'100'    ADD DOLLAR TO ASGNCOST                       
         SP    PENNIES,=P'100'     SUBRACT DOLLAR FROM PENNIES                  
GASX     DS    0H                                                               
         CLI   ROUNDFLG,C'Y'       CHK ROUNDING OPTION                          
         BNE   GASXXX                                                           
         ZAP   PAKWRK,TEMPASGN                                                  
GASX10   DP    PAKWRK,ROUND        DIVIDE BY ROUND TO GET REMAINDER             
         MVC   WORK(13),PAKWRK     SET QUOTIENT INTO WORK                       
         ZAP   DUB,ROUND                                                        
         DP    DUB,=P'2'                                                        
         ZAP   DUB,DUB(7)                                                       
         CP    DUB+5(3),PAKWRK+13(3)   PAKWRK+13=REMAINDER                      
         BH    GASXX                                                            
         AP    WORK(13),=P'1'                                                   
GASXX    ZAP   PAKWRK,WORK(13)                                                  
         MP    PAKWRK,ROUND                                                     
         ZAP   TEMPASGN,PAKWRK                                                  
GASXXX   AP    ASGNTOTS,TEMPASGN                                                
         B     EXIT                                                             
         SPACE 2                                                                
TOTTRTN  NTR1                  TOTALS                                           
         LA    R2,P                                                             
         USING PLINED,R2                                                        
         MVC   PUTIME(13),=C'*** TOTAL ***'                                     
         CLI   CPMS,C'Y'                                                        
         BNE   TOT4                                                             
         EDIT  GRPTOTS,(7,PUPRGGRP)                                             
         B     TOT5                                                             
TOT4     EDIT  GRPTOTS,(7,PUPRGGRP),1                                           
TOT5     EDIT  ACTTOTS,(13,PUACTDOL),2     ACTUALS DOLLARS                      
         EDIT  ASGNTOTS,(13,PUASGNDL),2    ASSIGND DOLS BY SEED PROG            
         CP    ASIGTOT,=PL8'0'             PREVIOUSLY ASSIGNED DOLS             
         BE    TOT5D                       NO/SKIP                              
         LA    R4,PUASGNDL+132             YES/PRINT                            
         EDIT  ASIGTOT,(13,0(R4)),2,FLOAT=*  ASSIGND NOT OVERRIDEN              
         AP    ASGNTOTS,ASIGTOT                                                 
         LA    R4,PUASGNDL+264                                                  
         EDIT  ASGNTOTS,(13,0(R4)),2                                            
TOT5D    GOTO1 SPOOL,DMCB,(R8)                                                  
         ZAP   CPP,=P'0'                                                        
         ZAP   ASGNTOTS,=P'0'                                                   
         ZAP   GRPTOTS,=P'0'                                                    
         ZAP   PAKWRK,=P'0'                                                     
         ZAP   PENNIES,=P'0'                                                    
         ZAP   INTGTOT,=P'0'                                                    
         CLI   PKGOPT,C'Y'                                                      
         BNE   TOTX                                                             
         L     R1,PKGS                                                          
         LA    R1,6(R1)                                                         
         STC   R1,ALLOWLIN                                                      
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   P+1(8),=C'PACKAGES'                                              
         MVC   P2+1(8),=C'--------'                                             
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R4,PKGLIST                                                       
         USING PKGLISTD,R4                                                      
TOT10    MVC   P+1(4),PKGNET                                                    
         MVI   P+6,0                                                            
         EDIT  (B1,PKGNUM),(3,P+7)                                              
         MVC   P+12(16),PKGNAME                                                 
         ICM   R1,15,PKGCOST                                                    
         SR    R0,R0                                                            
         M     R0,=F'100'                                                       
         EDIT  (R1),(13,P+30),2                                                 
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R4,PKGLEN(R4)                                                    
         CLI   0(R4),0                                                          
         BNE   TOT10                                                            
         MVC   P+30(13),=C'-------------'                                       
         EDIT  PKGCSTSV,(13,P2+30),2                                            
         ZAP   ACTTOTS,=P'0'                                                    
         GOTO1 SPOOL,DMCB,(R8)                                                  
TOTX     B     EXIT                                                             
         EJECT                                                                  
         SPACE                                                                  
********************************************                                    
*  WRITE UNIT REC TO P LINE                *                                    
*  SEED UNIT REC WITH ASSIGNED DOLLARS     *                                    
********************************************                                    
UPDTREC  NTR1                                                                   
* WRITE TO PRINT LINE *                                                         
         LA    R2,P                                                             
         USING PLINED,R2                                                        
         MVC   PUNET,SRTNET                                                     
         GOTO1 DATCON,DMCB,(2,SRTDTE),(4,PUDTE)        DATE                     
         MVI   PUDTE+5,C'-'                                                     
         LA    R4,PUDTE+6                                                       
         ZIC   R3,SRTDSUB                                                       
         EDIT  (R3),(2,0(R4)),ALIGN=LEFT               -SUBLINE                 
         MVC   PUDAY,SRTDAY                                                     
         MVC   PUTIME,SRTTIME                                                   
         MVC   PUPRGCD,SRTPRGCD                                                 
         MVC   PUPRGNM,SRTPRGNM                                                 
         EDIT  (B1,SRTLEN),(3,PULEN)                                            
         CLI   CPMS,C'Y'                                                        
         BNE   UPDT4                                                            
         EDIT  SRTAVGRP,(7,PUPRGGRP)                                            
         EDIT  SRTGRP,(7,PUGRP)                                                 
         B     UPDT4A                                                           
UPDT4    EDIT  (B4,SRTAVGRP),(7,PUPRGGRP),1                                     
         EDIT  (B4,SRTGRP),(7,PUGRP),1                                          
UPDT4A   CLI   PKGOPT,C'Y'                   IF PKGOPT SET ACTTOTS              
         BE    UPDT6                                                            
UPDT5    DS    0H                                                               
         ICM   R1,15,SRTACTDL                                                   
         EDIT  (R1),(13,PUACTDOL),2                                             
UPDT6    DS    0H                                                               
         CLI   SRTUNTK,C'*'        IF OVERRIDE/SKIP CPP                         
         BE    UPDT6A                                                           
         EDIT  CPP,(13,PUCPP),2                                                 
UPDT6A   LR    R1,R5               R5 POINTS TO CURRENT REC                     
         LA    R1,SRTRECLN(R1)     ARE THERE ANY MORE RECS                      
         CLI   0(R1),X'FF'                                                      
         BNE   UPDT10                                                           
         CP    ACTTOTS,ASGNTOTS    LAST REC IN THIS GROUP                       
         BE    UPDT10                                                           
         CLI   ROUNDFLG,C'Y'        IS IT ROUNDING                              
         BE    UPDT10                                                           
         MVC   WORK(8),ACTTOTS     GET DIFF BETWEEN ACTTOTS                     
         SP    WORK(8),ASGNTOTS        AND ASGNTOTS                             
         AP    TEMPASGN,WORK(8)        AND ADD TO LAST ASGN COST                
         AP    ASGNTOTS,WORK(8)                                                 
UPDT10   CLI   SRTUNTK,C'*'        IF ASSIGNED COST OVERRRIDE                   
         BNE   UPDT10D                                                          
         MVC   FULL,SRTUNTK+1    (HAS ASSIGNED COST OVER RIDE)                  
         L     R3,FULL                                                          
         EDIT  (R3),(13,PUASGNDL),2,FLOAT=*                                     
         B     UPDT20                                                           
UPDT10D  EDIT  TEMPASGN,(13,PUASGNDL),2                                         
         SPACE                                                                  
UPDT20   GOTO1 SPOOL,DMCB,(R8)                        PRINT THE LINE            
         SPACE                                                                  
* UPDATE REC *                                                                  
         MVI   NBNOWRIT,C'N'                                                    
         MVI   NBUPUNIT,C'N'                                                    
         CLI   SRTUNTK,C'*'         IS IT OVERRIDDEN                            
         BE    UPDTX                                                            
         CLI   UPDATFLG,C'Y'       TEST RUN OPTION                              
         BNE   UPDTX                                                            
         SPACE                                                                  
         MVI   NBUPUNIT,C'Y'       SET NETIO PUTREC SWITCH                      
         MVI   NBNOWRIT,C'Y'       SET NETIO WRITE SWITCH                       
         MVI   NBFUNCT,NBFGET      SET NETIO HAVE KEY/GET REC SWITCH            
         MVC   NBKEY,SRTUNTK       MOVE IN KEY FROM SORTREC                     
         LA    R2,NETIOHK          SET NETIO HOOK FOR PUTREC                    
         ST    R2,NBHOOK                                                        
         NETGO NSNETIO,DMCB,NETBLOCK                                            
         B     UPDTX                                                            
         SPACE                                                                  
NETIOHK  NTR1                                                                   
         L     R2,NBAIO                                                         
         USING NUMAINEL,R2                                                      
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   DUB,TEMPASGN                                                     
         CVB   R1,DUB                                                           
         STCM  R1,15,NUASSIGN                                                   
         OI    NUUNITST,X'08'      ASSIGNED COST INPUT                          
         L     R2,NBAIO                                                         
         MVI   ELCODE,X'02'        IF ASS COST WAS OVERRRIDEN                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING NUSDREL,R2                                                       
         NI    NUSDST3,X'7F'       TURN OFF ASSIGNED COST OVERRIDE              
         B     UPDTX                                                            
         SPACE 2                                                                
UPDTX    DS 0H                                                                  
         B     EXIT                (XIT1)                                       
         EJECT                                                                  
***************************************                                         
* ROUTINE GETS PACKAGE REC AND SETS                                             
* PACKAGE COST INTO ACTTOTS                                                     
*                                                                               
* ONLY CALLED IF PKGOPT=Y                                                       
*                                                                               
****************************************                                        
         SPACE                                                                  
GETPKG   NTR1                                                                   
         L     R1,NBPAKCST                                                      
         CVD   R1,DUB                                                           
         AP    PKGCSTSV,DUB                                                     
*                                                                               
         LA    R4,PKGLIST                                                       
         USING PKGLISTD,R4                                                      
         CLI   0(R4),0                                                          
         BE    GP10                                                             
         LA    R4,PKGLEN(R4)                                                    
         B     *-12                                                             
GP10     MVC   PKGNET,NBACTNET                                                  
         MVC   PKGNUM,NBACTPAK                                                  
         MVC   PKGNAME,NBPAKNAM                                                 
         MVC   PKGCOST,NBPAKCST                                                 
         L     R1,PKGS                                                          
         LA    R1,1(R1)                                                         
         ST    R1,PKGS                                                          
GETPX    B     EXIT                                                             
         DROP  R4                                                               
*                                                                               
SETPCST  NTR1                                                                   
         ZAP   ACTTOTS,=P'0'                                                    
         AP    ACTTOTS,PKGCSTSV                                                 
         CLI   PKGOPT,C'Y'                                                      
         BNE   SETPX                                                            
         SP    ACTTOTS,ASIGTOT                                                  
SETPX    B     EXIT                                                             
         EJECT                                                                  
*                                                                               
         GETEL (R2),NBDTADSP,ELCODE                                             
         SPACE 2                                                                
*                                                                               
         SPACE                                                                  
HDHOOK   NTR1                                                                   
         L     R4,ATWA                                                          
         USING T320FFD,R4                                                       
         MVC   H3+10(3),SPLCLI                                                  
         MVC   H3+16(20),SPLCLIN                                                
         CLC   =C'ALL',SPLPRO                                                   
         BNE   HD2                                                              
         MVC   H4+10(3),=C'ALL'                                                 
         B     HD2B                                                             
HD2      MVC   H4+10(3),SPLPRO                                                  
         MVC   H4+16(20),SPLPRON                                                
HD2B     LA    R3,H5+10                                                         
         MVC   0(6,R3),SPLEST                                                   
         LA    R3,H6+10                                                         
         MVC   0(3,R3),=C'ALL'                                                  
         CLC   =C'ALL',SPLPAK                                                   
         BE    HD3                                                              
         CLC   =C'   ',SPLPAK                                                   
         BNL   HD3                                                              
         MVC   0(8,R3),SPLPAK                                                   
         DROP  R4                                                               
HD3      CLI   UPDATFLG,C'Y'                                                    
         BE    *+10                                                             
         MVC   H4+49(30),=C'* TEST RUN - FILE NOT MARKED *'                     
         CLI   ASGNDOVR,C'Y'                                                    
         BE    *+10                                                             
         MVC   H5+50(27),=C'(*=ASSIGNED COST OVERRIDES)'                        
         LA    R2,H10                                                           
         USING PLINED,R2                                                        
         MVC   PUNET(4),=C'NTWK'                                                
         MVC   PUDTE(4),=C'UNIT'                                                
         MVC   PUDTE+132(4),=C'DATE'                                            
         MVC   PUDAY(3),=C'DAY'                                                 
         MVC   PUTIME(4),=C'TIME'                                               
         MVC   PUPRGCD(7),=C'PROGRAM'                                           
         MVC   PUPRGCD+133(4),=C'CODE'                                          
         MVC   PUPRGNM(12),=C'PROGRAM NAME'                                     
         MVC   PUPRGGRP+132,=C'PROGRM'                                          
         MVC   PUGRP+132,=C'ACTUAL'                                             
         MVC   PUPRGGRP+5(7),DEMONAME                                           
         MVC   PULEN,=C'LEN'                                                    
         MVC   PUACTDOL+3(6),=C'ACTUAL'                                         
         MVC   PUACTDOL+136(4),=C'COST'                                         
         MVC   PUCPP+3(8),=C'COST PER'                                          
         MVC   PUCPP+137(5),=C'POINT'                                           
         CLI   CPMS,C'Y'                                                        
         BNE   *+10                                                             
         MVC   PUCPP+135(8),=C'THOUSAND'                                        
         MVC   PUASGNDL+3(8),=C'ASSIGNED'                                       
         MVC   PUASGNDL+137(4),=C'COST'                                         
         SPACE                                                                  
*  SET UP BOXES PARAMETERS *                                                    
         L     R1,ABOX                                                          
         USING BOXD,R1                                                          
         MVC   BOXCOLS,SPACES                                                   
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0           (ONLY FOR SPOOF)                              
         SPACE                                                                  
         LA    R5,BOXCOLS                                                       
         MVI   0(R5),C'L'                                                       
         LA    R5,6(R5)            UNIT NETWORK                                 
         MVI   0(R5),C'C'                                                       
         LA    R5,10(R5)           UNIT DATE                                    
         MVI   0(R5),C'C'                                                       
         LA    R5,6(R5)            DAY                                          
         MVI   0(R5),C'C'                                                       
         LA    R5,13(R5)           TIME                                         
         MVI   0(R5),C'C'                                                       
         LA    R5,8(R5)            PRGRM CODE                                   
         MVI   0(R5),C'C'                                                       
         LA    R5,18(R5)           PRGRM NAME                                   
         MVI   0(R5),C'C'                                                       
         LA    R5,5(R5)            LEN                                          
         MVI   0(R5),C'C'                                                       
         LA    R5,8(R5)            PROGGRP                                      
         MVI   0(R5),C'C'                                                       
         LA    R5,8(R5)            GRP                                          
         MVI   0(R5),C'C'                                                       
         LA    R5,15(R5)           ACTUAL COST                                  
         MVI   0(R5),C'C'                                                       
         LA    R5,15(R5)           CPP                                          
         MVI   0(R5),C'C'                                                       
         LA    R5,15(R5)           ASSIGNED COST                                
         MVI   0(R5),C'R'                                                       
         SPACE                                                                  
         LA    R5,BOXROWS                                                       
         LA    R5,8(R5)                                                         
         MVI   0(R5),C'T'                                                       
         LA    R5,3(R5)                                                         
         MVI   0(R5),C'M'                                                       
         LA    R5,46(R5)                                                        
         MVI   0(R5),C'B'                                                       
         SPACE                                                                  
HDX      B     EXIT                 (XIT1)                                      
         EJECT                                                                  
*                                                                               
MYSPECS  DS    0F                                                               
         SSPEC H1,1,C'MEDIA     NETWORK T.V.'                                   
         SSPEC H1,55,C'EQUALIZED COST REPORT'                                   
         SSPEC H1,96,AGYNAME                                                    
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,55,C'---------------------'                                   
         SSPEC H3,52,PERIOD                                                     
         SSPEC H2,96,AGYADD                                                     
         SSPEC H3,1,C'CLIENT'                                                   
         SSPEC H4,1,C'PRODUCT'                                                  
         SSPEC H5,1,C'ESTIMATE'                                                 
         SSPEC H6,1,C'PACKAGE'                                                  
         SSPEC H4,96,RUN                                                        
         SSPEC H5,109,PAGE                                                      
         DC    X'00'                                                            
         EJECT                                                                  
* LOCKER FOR UPDATIVE SOON                                                      
         SPACE                                                                  
LOCKEM   NTR1                                                                   
         L     R1,ATWA                                                          
         USING T320FFD,R1                                                       
         CLC   =C'SOON',CONWHEN    IS IT SOON                                   
         BNE   LOCKX               NO/FORGET IT                                 
         CLI   UPDATFLG,C'Y'       WRITING TO FILE?                             
         BNE   LOCKX                                                            
         DROP  R1                                                               
* - GET SE NUMBER                                                               
LOCK0    GOTO1 GETFACT,DMCB,(2,0)                                               
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         CLI   FAOVSYS,3           MUST BE NET                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   BYTE,FASYS          GET SE NUMBER                                
         DROP  R1                                                               
* - LOCK / UNLOCK                                                               
         LA    R3,MYKEY                                                         
         USING LKKEYD,R3                                                        
         XC    MYKEY(L'LOCKEY),MYKEY                                            
         MVC   LOCKSE,BYTE             SET SE NUMBER                            
         MVC   LOCKAGY,NBSELAGY        AGENCY                                   
         MVC   LOCKRTY,=C'UN'          UNIT RECORDS                             
         MVC   LOCKKEY(3),NBSELCLI     3 BYTE CLIENT CODE                       
         XC    DMCB(4),DMCB                                                     
         MVC   DMCB+4(3),=X'D9000A'                                             
         MVI   DMCB+7,X'7E'                                                     
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     R2,DMCB                                                          
         XC    DMCB(24),DMCB                                                    
         L     R6,ACOMFACS                                                      
         LTR   R6,R6                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTO1 (R2),(R1),(HALF,MYKEY),(R6)                                      
         CLI   DMCB+4,0            ANY ERRORS                                   
         BE    LOCKX                                                            
         DC    H'0'                                                             
*                                                                               
         DROP  R3                                                               
LOCKX    XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
PKGLIST  DS    CL3000              AT 25CL PER PKG = 120 PKGS                   
*                                                                               
MYNTBUF  DS    CL4000              5 X 799 STATIONS                             
         EJECT                                                                  
*                                                                               
*                                                                               
*** MY WORKING STORAGE USING ANETWS1 AND 2 ******                               
*                                                                               
WORKD    DSECT                                                                  
CLISTSV  DS    CL880            *  FROM EDIT MODULE                             
CLTNMSV  DS    CL20             *  FROM EDIT MODULE                             
ROUNDFLG DS    CL1              *  FROM EDIT MODULE                             
ROUND    DS    PL3              *  FROM EDIT MODULE                             
PKGOPT   DS    CL1              *  FROM EDIT MODULE                             
DEMOPT   DS    CL1              *  FROM EDIT MODULE                             
ASGNDOVR DS    CL1                                                              
RELO     DS    F                *  FROM EDIT MODULE                             
EQIVAL   DS    CL1              *  FROM EDIT MODULE                             
CPMS     DS    CL1              *  FROM EDIT MODULE                             
UNASIGND DS    CL1              *  FROM EDIT MODULE                             
PFBOPT   DS    CL1              *  FROM EDIT MODULE                             
SPCLOPT  DS    CL1              *  FROM EDIT MODULE                             
*                                                                               
WRKSTRT  EQU   *                                                                
ASVAREA  DS    F                                                                
PKGS     DS    F                                                                
ANTDMBLK DS    A                                                                
ADEDBLK  DS    A                                                                
GRPTOTS  DS    PL4                 TOTAL FOR GRP                                
PAKWRK   DS    PL16                PACKED WORK AREA                             
CPP      DS    PL8                 COST PER POINT                               
ACTTOTS  DS    PL8                 TOTAL FOR ACTUAL DOLLARS                     
ASGNTOTS DS    PL8                 TOTAL FOR ASSIGNED DOLLARS                   
TEMPASGN DS    PL8                 ASSIGNED COST                                
PENNIES  DS    PL8                 PENNIES                                      
INTGTOT  DS    PL8                 INTEGRATION TOTAL (BBDO FEATURE)             
ASIGTOT  DS    PL8                 PREVIOUSLY ASSIGNED DOLLARS                  
TEMPGRP  DS    PL4                                                              
NPROGS   DS    PL4                                                              
BNPROGS  DS    F                                                                
*                                                                               
RECSFSRT DS    CL8                                                              
FRST     DS    CL1                                                              
UPDATFLG DS    CL1                                                              
DEMONAME DS    CL15                                                             
*                                                                               
PKGCSTSV DS    PL8                                                              
PKGINTSV DS    F                                                                
*                                                                               
MYKEY    DS    CL40                                                             
*                                                                               
WRKEND   EQU   *                                                                
*                                                                               
PKGLISTD DSECT                                                                  
PKGNET   DS    CL4                                                              
PKGNUM   DS    CL1                                                              
PKGNAME  DS    CL16                                                             
PKGCOST  DS    CL4                                                              
PKGLEN   EQU   *-PKGNET                                                         
         EJECT                                                                  
         SPACE                                                                  
*                                                                               
SORTRECD DSECT                                                                  
SORTREC  DS    0CL1         *****   NO LONGER SORTED  *****                     
         DS    CL1                                                              
SRTCLT   DS    CL3                 CLIENT CODE (PRINTABLE)                      
SRTEST   DS    CL1                 ESTIMATE  (NBACTEST)                         
SRTNET   DS    CL4                 NETWORK   (NBACTNET)                         
SRTPKG   DS    CL1                 PACKAGE   (NBPACK)                           
SRTPRD   DS    CL1                 PRODUCT   (NBPRD)                            
SRTPRGCD DS    CL6                 PROGRAM CODE   (NBACTPRG)                    
SRTDTE   DS    CL2                 DATE(NBACTDAT)                               
SRTDSUB  DS    CL1                 -SUBLINE(NBACTSUB)                           
SRTDAY   DS    CL3                 DAY  (NBDAYNAM)                              
SRTTIME  DS    CL11                START-END TIME (NBTIME)                      
SRTPRGNM DS    CL16                PROGRAM NAME (NBPRGNAM)                      
SRTGRP   DS    CL4                 GRP (1DEC)                                   
SRTAVGRP DS    CL4                 CALCULATED GRP BASED ON PROG AV              
SRTACTDL DS    CL4                 ACTUAL DOLLARS                               
SRTPRDCD DS    CL3                 PRINTABLE PRODUCT CODE                       
SRTUNTK  DS    CL32                UNIT REC KEY (NBKEY)                         
SRTINTEG DS    CL4                 INTEGRATION                                  
SRTLEN   DS    CL1                                                              
SRTRECLN EQU   *-SORTREC                                                        
SRTRECND EQU   *                                                                
         EJECT                                                                  
*                                                                               
PLINED   DSECT            *** DSECT FOR PRINT LINE ***                          
         DS    CL2                                                              
PUNET    DS    CL4                 UNIT NETWORK                                 
         DS    CL2                                                              
PUDTE    DS    CL8                 UNIT DATE MMMDD-NN                           
         DS    CL2                                                              
PUDAY    DS    CL3                 UNIT DAY ALPHA                               
         DS    CL2                                                              
PUTIME   DS    CL11                UNIT TIME                                    
         DS    CL2                                                              
PUPRGCD  DS    CL6                 UNIT PROGRAM CODE                            
         DS    CL2                                                              
PUPRGNM  DS    CL16                UNIT PROGRAM NAME                            
         DS    CL2                                                              
PULEN    DS    CL3                                                              
         DS    CL2                                                              
PUPRGGRP DS    CL6                 CALCULATED GRP BASED ON PROG AV              
         DS    CL2                                                              
PUGRP    DS    CL6                 GRP                                          
         DS    CL2                                                              
PUACTDOL DS    CL13                ACTUAL COST                                  
         DS    CL2                                                              
PUCPP    DS    CL13                COST PER POINT                               
         DS    CL2                                                              
PUASGNDL DS    CL13                ASSIGNED COST                                
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE NETINCLS                                                       
         EJECT                                                                  
       ++INCLUDE NEWRIFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEWRIEBD                                                       
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
       ++INCLUDE DDBIGBOX                                                       
         EJECT                                                                  
NDBLK    DSECT                                                                  
       ++INCLUDE NETDEMOD                                                       
       ++INCLUDE DEDBLOCK                                                       
         EJECT                                                                  
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE NEGENPACK                                                      
       ++INCLUDE NEGENUNIT                                                      
*                                                                               
       ++INCLUDE FALOCKETD                                                      
       ++INCLUDE FASYSFAC                                                       
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FASSB                                                          
       ++INCLUDE DDMASTC                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'061NEWRI42   05/01/02'                                      
         END                                                                    
