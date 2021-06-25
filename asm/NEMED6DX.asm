*          DATA SET NEMED6DX   AT LEVEL 002 AS OF 05/01/02                      
*          DATA SET NEMED6D    AT LEVEL 004 AS OF 10/20/00                      
*          DATA SET NEMED6D    AT LEVEL 011 AS OF 06/07/99                      
*PHASE T31E6DA,+0                                                               
         TITLE 'T31E6D - EQUALIZED COST UPDATE/REPORT'                          
         PRINT NOGEN                                                            
************************************************************                    
* EQUALIZED COST UPDATE/REPORT                             *                    
*                                                          *                    
* THIS PROGRAM CALCULATES THE ASSIGNED COST FOR UNITS      *                    
*                                                          *                    
*   INTEGFLG ='B'  -  BBDO FEATURE. CPP IS CALCULATED ON   *                    
*                 ACTUAL DOLLARS BUT REPORT SHOWS ACTUAL   *                    
*                 DOLLARS PLUS INTEGRATION IN ACT DOLS     *                    
*                 COLUMN.                                  *                    
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
T31E6D   CSECT                                                                  
         NMOD1 0,**NE6D**,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         LA    RA,4095(RB)                                                      
         LA    RA,1(RA)                                                         
         USING T31E6D,RB,RA         RA = 2ND BASE REG                           
         LA    R1,HDHOOK                                                        
         ST    R1,HEADHOOK                                                      
         L     R7,ANETWS1     R7 - ANETWS1 AND ANETWS2 =WORKING STORAGE         
         USING WORKD,R7                                                         
         L     R6,ANETWS4     R6 - ANETWS4+1000=NDDEMBLK/DEDBLOCK               
**->     A     R6,=F'2000'                                                      
         USING NDDEMBLK,R6                                                      
*                                                                               
         LA    RE,MYSAVE                                                        
         LA    RF,MYSVEND-MYSAVE                                                
         XCEF                                                                   
*                                                                               
         SPACE 3                                                                
         B     INIT                                                             
EXIT     XIT1                                                                   
         EJECT                                                                  
* INITIALIZATION                                                                
         SPACE                                                                  
INIT     DS    0H                                                               
         SPACE                                                                  
         L     R2,ATWA                                                          
         USING T31EFFD,R2                                                       
         MVI   UPDATFLG,C'N'                                                    
         CLI   SPLTST,C'Y'                     TEST RUN OPTION                  
         BE    *+8                                                              
         MVI   UPDATFLG,C'Y'                                                    
         DROP  R2                                                               
         L     R1,=A(STALIST)      FOR NTI LINKED STATIONS                      
         ST    R1,NBCNVNTI                                                      
         MVI   FRST,0                                                           
         ZAP   PKGCSTSV,=P'0'                                                   
         XC    PKGINTSV,PKGINTSV                                                
         ZAP   RECSFSRT,=P'0'                                                   
         ZAP   PAKWRK,=P'0'                                                     
         ZAP   GRPTOTS,=P'0'                                                    
         ZAP   CPP,=P'0'                                                        
         ZAP   ACTTOTS,=P'0'                                                    
         ZAP   ASGNTOTS,=P'0'                                                   
         ZAP   TEMPASGN,=P'0'                                                   
         ZAP   PENNIES,=P'0'                                                    
         ZAP   INTGTOT,=P'0'                                                    
         SPACE                                                                  
         MVC   DBFILE,=C'NTI'    SET UP DEDBLOCK/NO DEMO SCREEN FIELD           
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELMED,C'N'                                                    
         MVI   DBSELSRC,C'N'                                                    
         SPACE                                                                  
         EJECT                                                                  
*                                                                               
         CLI   EQIVAL,X'FF'        EQUIVALENCING RATINGS OPTION                 
         BE    *+10                                                             
         MVC   NBUSER2(1),EQIVAL                                                
         OI    NBINDS,X'80'        EQUIVALENCE OVERRIDES                        
*        NETGO NVDEMOPT,DMCB       RETURN EST DEMOS FOR MAKE GOOD               
*                                  BUT NOT FOR PFB                              
*                                                                               
         CLI   CABOPT,C'Y'         CABLE?                                       
         BNE   *+12                                                             
         MVI   NBHUNOPT,C'Y'       SET FOR CABLE                                
         MVI   NBPREOPT,C'Y'       SET FOR CABLE                                
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
         MVI   NBRESUME,NBPROCPK   START AT PKGS                                
         MVI   NBDATA,C'B'         GET ALL RECORDS                              
         SPACE                                                                  
PAS05    NETGO NSNETIO,DMCB,NETBLOCK            GET  RECORD                     
         SPACE                                                                  
         CLI   NBMODE,NBREQLST                                                  
         BE    FRM20                                                            
         CLI   NBMODE,NBPROCPK                                                  
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
PAS11    MVC   SRTACTDL,NBACTUAL        ACTUAL DOLS (CENTS)                     
         CLI   ASGNDOVR,C'Y'            ASSIGNED COST OVERRIDES                 
         BE    PAS12                    Y=REALLOCATE OVERRIDES                  
         TM    NBUNST3,X'80'              IS IT OVERRIDE                        
         BNO   PAS12                                                            
         MVI   SRTUNTK,C'*'               THEN MARK AS SUCH                     
         MVC   SRTUNTK+1(4),NBASSIGN      AND STORE OVERRIDE ASSGN              
         B     *+10                                                             
PAS12    MVC   SRTUNTK,NBKEY            UNT REC KEY                             
         MVC   SRTINTEG,NBINTEG                                                 
         MVC   SRTLEN,NBLEN                                                     
         SPACE                                                                  
         CLI   INTEGFLG,C'N'       INTEG + ACTUAL OPTION                        
         BE    PAS20                                                            
         MVC   FULL,NBACTUAL       YES/CARRY INTEG+ACT IN SRTACTDL              
         L     R1,FULL                                                          
         MVC   FULL,NBINTEG                                                     
         A     R1,FULL                                                          
         STCM  R1,15,SRTACTDL                                                   
         CLI   INTEGFLG,C'S'       INTEG + ACTUAL + SPECIAL                     
         BNE   PAS20                                                            
         MVC   FULL,NBSPCHRG                                                    
         A     R1,FULL                                                          
         STCM  R1,15,SRTACTDL                                                   
         SPACE                                                                  
PAS20    B     FRM12                                                            
         SPACE                                                                  
         EJECT                                                                  
*                                                                               
FRM12    AP    RECSFSRT,=P'1'                                                   
         CLI   FRST,C'N'                           IS IT FIRST TIME             
         BE    FRM13                                                            
*                                                                               
         MVI   FRST,C'N'                            YES/GET STORAGE             
****     GOTO1 =V(COVAIL),DMCB,C'GET',4000000,4000000                           
****     L     R4,4(R1)                                                         
****     L     R2,8(R1)                                                         
****     LTR   R4,R4                                                            
****     BNZ   *+6                                                              
****     DC    H'0'                                                             
****     ST    R4,ASVAREA                                                       
****     C     R2,=F'4000000'                                                   
****     BE    *+6                                                              
****     DC    H'0'                                                             
*        L     R0,GTSTORL                                                       
*        ST    R0,MINSTOR                                                       
*        ST    R0,MAXSTOR                                                       
*        LA    R4,MINSTOR                                                       
*        LA    R5,ASVAREA                                                       
*        GETMAIN VC,LA=(R4),A=(R5)                                              
*                                                                               
         CLI   SOONRUN,C'Y'                                                     
         BNE   FRM12B                                                           
         GETMAIN EU,LV=3000000,A=ANSWADD         SOON TAKES LESS                
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVC   ASVAREA,ANSWADD                                                  
         B     FRM12D                                                           
*                                                                               
FRM12B   DS    0H                                                               
         GETMAIN EU,LV=4000000,A=ANSWADD                                        
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVC   ASVAREA,ANSWADD                                                  
         B     FRM12D                                                           
*                                                                               
FRM12D   DS    0H                                                               
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
         LA    R5,SRTRECLN(R5)                                                  
         MVC   SORTREC(SRTRECLN),0(R3)                                          
         SPACE                                                                  
FRM15    DS    0H                                                               
         CLI   SRTUNTK,C'*'        ..IF ASSIGNED COST OVERRIDE                  
         BNE   FRM16                                                            
         ICM   R1,15,SRTACTDL                                                   
         LTR   R1,R1                                                            
         BP    PAS05               ..POSITIVE DOLLARS DON'T INCLUDE             
         BAS   RE,ADDOLS           ..NEGATIVE INCLUDE                           
         B     PAS05                                                            
FRM16    BAS   RE,ADDGRPS                 ADD TO GRP TOTS                       
         BAS   RE,ADDOLS                  ADD TO DOLLAR TOTS                    
         B     PAS05                                                            
         EJECT                                                                  
         SPACE                                                                  
FRM20    CLI   FRST,0              ARE THERE ANY RECORDS                        
         BE    FRMXX               NO                                           
         MVI   FORCEHED,C'Y'                                                    
         CLI   PKGOPT,C'Y'         IF USING PKG COST                            
         BNE   *+8                                                              
         BAS   RE,SETPCST          SET PKG COST INTO ACTTOTS                    
         LA    R5,SRTRECLN(R5)     BUMP TO END OF SAVE AREA                     
         MVI   0(R5),X'FF'                                                      
         BAS   RE,GETCPP                                                        
         L     R5,ASVAREA          RESET TO START OF SAVE AREA                  
         CLI   DUB,C'X'            CHK ERROR                                    
         BE    FRM30                                                            
         L     R1,FACTOR           CONVERT FACTOR TO PACKED DEC                 
         CVD   R1,DUB                                                           
         MVC   FACTOR,DUB+4                                                     
FRM25    CLI   0(R5),X'FF'                                                      
         BE    FRM30                                                            
         BAS   RE,GETASGND                                                      
         BAS   RE,UPDTREC                                                       
         MVI   0(R5),X'FF'                                                      
         LA    R5,SRTRECLN(R5)                                                  
         B     FRM25                                                            
         SPACE                                                                  
FRM30    DS    0H                                                               
         BAS   RE,SUBTRTN                         DO SUB TOTALS                 
         L     R2,ASVAREA                                                       
******   GOTO1 =V(COVAIL),DMCB,C'FREE',(R2),4000000  PXZ                        
         CLI   SOONRUN,C'Y'                                                     
         BNE   FRM40                                                            
         L     R1,ASVAREA                                                       
         L     R2,ANSWLEN                                                       
         FREEMAIN EU,LV=3000000,A=ANSWADD       SOON TAKES LESS                 
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         B     FRMX                                                             
*                                                                               
FRM40    L     R1,ASVAREA                                                       
         L     R2,ANSWLEN                                                       
         FREEMAIN EU,LV=4000000,A=ANSWADD       NOT SOON TAKES MORE             
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
FRMX     DS    0H                                                               
         B     EXIT                                                             
*                                                                               
FRMXX    MVC   P(14),=C'NO UNITS FOUND'                                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXIT                                                             
         EJECT                                                                  
****************************************************************                
* DO NOT USE R3,R5 IN CALCS BELOW/ THESE REGS POINT TO RECORDS *                
*    USED IN EARLIER RTNS AS WELL AS IN CALCS                  *                
****************************************************************                
         SPACE                                                                  
ADDGRPS  NTR1                                                                   
         ICM   R1,15,SRTGRP                                                     
         CVD   R1,DUB                                                           
         AP    GRPTOTS,DUB                                                      
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
         CLI   CABOPT,C'Y'         ,,IF CABLE OPTION                            
         BNE   CPP4                                                             
         MP    PAKWRK,=P'10'       ,,DO IT AGAIN FOR GRP IS 2 DEC               
CPP4     ZAP   FULL,=P'0'           CHECK IF THERE ARE GRPS                     
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
CPP7     MVC   WORK(12),PAKWRK                                                  
         ZAP   PAKWRK,WORK(12)                                                  
         DP    PAKWRK,=P'100'      PUT DOLS IN PAKWRK/PENYS IN +14              
         ZAP   CPP,PAKWRK(14)      SET ONLY DOLS IN CPP                         
         MP    CPP,=P'100'         BUT WILL EDIT FOR PENYS                      
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
GETASGND NTR1                    UNIT GRP X ACTTOTS/GRPTOTS=ASGN COST           
*                                  (1DEC)   (2DEC)  (1DE)    (2DEC)             
         CLI   SRTUNTK,C'*'        IS IT OVERRIDE                               
         BE    EXIT                IF YES/EXIT                                  
*                                                                               
         ZAP   PAKWRK,ACTTOTS      ,,DIV ACTOTS BY 10 TO GET                    
         CLI   CABOPT,C'Y'         ,,,IF IT'S CABLE                             
         BE    GAS3                                                             
         DP    PAKWRK,=P'10'       ,,2 DEC IN PKWRK                             
         B     GAS4                                                             
GAS3     DP    PAKWRK,=P'100'      ,,,CABLE HAS GRP TO 2 DEC                    
         B     GAS4                                                             
*                                                                               
GAS4     MVC   WORK(14),PAKWRK                                                  
         ZAP   PAKWRK,WORK(14)                                                  
*                                                                               
         ICM   R1,15,SRTGRP     UNIT GRP(SRTGRP) X ACTOTS(IN PAKWRK)            
         CVD   R1,DUB                                                           
         MP    PAKWRK,DUB+4(4)                                                  
*                                                                               
         MP    PAKWRK,=P'10'     TO BALANCE DIFF IN DEC IN DIVISION             
         CLI   CABOPT,C'Y'         ..IF ITS CABLE                               
         BNE   GAS4B                                                            
         MP    PAKWRK,=P'10'       ..AGAIN FOR GRP IS 2 DEC                     
GAS4B    DP    PAKWRK,GRPTOTS                                                   
*                                                                               
         MVC   WORK(12),PAKWRK                                                  
         ZAP   PAKWRK,WORK(12)    SAVE PNYS/SET ASGN COST WITHOUT PENY          
         SPACE                                                                  
         CLI   FCTRFLG,C'Y'        CHK FACTOR PERCENT (NN.NN)                   
         BNE   GAS5                                                             
         MP    PAKWRK,FACTOR                                                    
         DP    PAKWRK,=P'100000'      DIVIDE BY 100000 FOR NN.NNN PCNT          
         MVC   WORK(12),PAKWRK                                                  
         ZAP   PAKWRK,WORK(12)         ZAP IN FACTORED ASGNCOST                 
         SPACE                                                                  
GAS5     DP    PAKWRK,=P'100'  (PUTS DOLS IN PAKWRK/PENYS IN PAKWRK+14)         
         ZAP   TEMPASGN,PAKWRK(14)   SET DOLS(NO PENNIES) TO TEMPASGN           
         MP    TEMPASGN,=P'100'      BUT WE EDIT FOR PENYS                      
         CLI   ROUNDFLG,C'Y'       CHK ROUNDING OPTION                          
         BE    GASX                IF YES/SKIP PENNY COUNT                      
*                                                                               
         AP    PENNIES,PAKWRK+14(2)  AND KEEP COUNT OF PENNIES                  
         CP    PENNIES,=P'99'                                                   
         BNH   GASX                                                             
         AP    TEMPASGN,=P'100'    ADD DOLLAR TO ASGNCOST                       
         SP    PENNIES,=P'100'     SUBTRACT DOLLAR FROM PENNIES                 
GASX     DS    0H                                                               
         CLI   ROUNDFLG,C'Y'       CHK ROUNDING OPTION                          
         BNE   GASXXX                                                           
         ZAP   PAKWRK,TEMPASGN                                                  
         DP    PAKWRK,ROUND        DIVIDE BY ROUND TO GET REMAINDER             
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
         EJECT                                                                  
*                                                                               
SUBTRTN  NTR1                     TOTALS                                        
         LA    R2,P                                                             
         USING PLINED,R2                                                        
         MVC   PUTIME(13),=C'*** TOTAL ***'                                     
         CLI   CPMS,C'Y'                                                        
         BNE   SUBT4                                                            
         EDIT  GRPTOTS,(7,PUGRP)                                                
         B     SUBT4A                                                           
*                                                                               
SUBT4    CLI   CABOPT,C'Y'         CABLE?                                       
         BE    SUBTCAB                                                          
         EDIT  GRPTOTS,(7,PUGRP),1                                              
         B     SUBT4A                                                           
SUBTCAB  EDIT  GRPTOTS,(7,PUGRP),2                                              
*                                                                               
SUBT4A   EDIT  ACTTOTS,(13,PUACTDOL),2                                          
         CLI   INTEGFLG,C'B'       **BBDO FEATURE ADDS INTG TO ASGNTOT          
         BNE   SUBT5                                                            
         AP    ASGNTOTS,INTGTOT                                                 
SUBT5    EDIT  ASGNTOTS,(13,PUASGNDL),2                                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
         ZAP   ACTTOTS,=P'0'                                                    
         ZAP   CPP,=P'0'                                                        
         ZAP   ASGNTOTS,=P'0'                                                   
         ZAP   GRPTOTS,=P'0'                                                    
         ZAP   PAKWRK,=P'0'                                                     
         ZAP   PENNIES,=P'0'                                                    
         ZAP   INTGTOT,=P'0'                                                    
         MVI   FRST,0                                                           
         B     EXIT                                                             
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
         EDIT  SRTGRP,(7,PUGRP)                                                 
         B     UPDT4C                                                           
UPDT4    CLI   CABOPT,C'Y'                   CABLE ?                            
         BE    UPDT4B                                                           
         EDIT  (B4,SRTGRP),(7,PUGRP),1       NOT CABLE                          
         B     UPDT4C                                                           
UPDT4B   EDIT  (B4,SRTGRP),(7,PUGRP),2       CABLE                              
*                                                                               
UPDT4C   CLI   PKGOPT,C'Y'                   IF PKGOPT SET ACTTOTS              
         BE    UPDT6                                                            
         ICM   R1,15,SRTACTDL                                                   
         EDIT  (R1),(13,PUACTDOL),2,MINUS=YES                                   
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
         CLI   FCTRFLG,C'Y'        IS IT FACTOR                                 
         BE    UPDT10                                                           
         CLI   ROUNDFLG,C'Y'        IS IT ROUNDING                              
         BE    UPDT10                                                           
         MVC   WORK(8),ACTTOTS     GET DIFF BETWEEN ACTTOTS                     
         SP    WORK(8),ASGNTOTS        AND ASGNTOTS                             
         AP    TEMPASGN,WORK(8)        AND ADD TO LAST ASGN COST                
         AP    ASGNTOTS,WORK(8)                                                 
UPDT10   DS    0H                                                               
         CLI   INTEGFLG,C'B'       ***BBDO FEATURE                              
         BNE   UPDT10A                                                          
         MVC   FULL,SRTINTEG                                                    
         L     R1,FULL                                                          
         CVD   R1,DUB                                                           
         AP    TEMPASGN,DUB                                                     
         AP    INTGTOT,DUB                                                      
UPDT10A  CLI   SRTUNTK,C'*'        IF ASSIGNED COST OVERRRIDE                   
         BNE   UPDT10D                                                          
         MVC   FULL,SRTUNTK+1    (HAS ASSIGNED COST OVER RIDE)                  
         L     R3,FULL                                                          
         EDIT  (R3),(13,PUASGNDL),2,FLOAT=*,MINUS=YES                           
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
         XC    NBNTISTA,NBNTISTA                                                
         XC    NBSTATYP,NBSTATYP                                                
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
         USING NUSDRD,R2                                                        
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BNE   UPDTX                                                            
         NI    NUSDST3,X'FF'-X'80'  TURN OFF OVERRIDE                           
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
         CLI   INTEGFLG,C'Y'                                                    
         BNE   GETPX                                                            
         L     R1,PKGINTSV                                                      
         A     R1,NBPAKINT                                                      
         ST    R1,PKGINTSV                                                      
GETPX    B     EXIT                                                             
*                                                                               
SETPCST  NTR1                                                                   
         ZAP   ACTTOTS,=P'0'                                                    
         CLI   INTEGFLG,C'Y'                                                    
         BNE   PSETX                                                            
         L     R1,PKGINTSV      ADD PACKAGE INT COST                            
         CVD   R1,DUB           TO ACTTOTS                                      
         XC    WORK(8),WORK                                                     
         MVC   WORK+8(8),DUB                                                    
         MP    WORK(16),RECSFSRT  MULT INT COST PER UNIT X NUM OF UNITS         
         AP    ACTTOTS,WORK(16)                                                 
         ZAP   RECSFSRT,=P'0'                                                   
PSETX    AP    ACTTOTS,PKGCSTSV                                                 
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
         GETEL (R2),NBDTADSP,ELCODE                                             
         SPACE 2                                                                
*                                                                               
         SPACE                                                                  
HDHOOK   NTR1                                                                   
         L     R4,ATWA                                                          
         USING T31EFFD,R4                                                       
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
         MVC   PUGRP+1(3),=C'GRP'                                               
         CLI   CPMS,C'Y'                                                        
         BNE   *+10                                                             
         MVC   PUGRP+1(4),=C'IMPS'                                              
         MVC   PULEN,=C'LEN'                                                    
         MVC   PUGRP+132(7),DEMONAME                                            
         MVC   PUACTDOL+3(6),=C'ACTUAL'                                         
         MVC   PUACTDOL+136(4),=C'COST'                                         
         CLI   INTEGFLG,C'Y'                                                    
         BNE   HDHK10                                                           
         MVC   PUACTDOL(13),=C'ACTUAL + INTG'                                   
         MVI   PUACTDOL+136,C' '         TO BLANK 1ST C OF 1ST COST             
         MVC   PUACTDOL+137(4),=C'COST'                                         
HDHK10   MVC   PUCPP+3(8),=C'COST PER'                                          
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
*                                                                               
*GTSTORL  DC    A(4000000)                                                      
ANSWADD  DC    F'0'                                                             
ANSWLEN  DC    F'0'                                                             
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         SPACE                                                                  
STALIST  DS    CL2000              FOR NBCNVNTI                                 
*                                                                               
TRAPERR  EQU   ERREX                                                            
         EJECT                                                                  
*                                                                               
*** MY WORKING STORAGE USING ANETWS1 AND 2 ******                               
*                                                                               
WORKD    DSECT                                                                  
CLISTSV  DS    CL880            *  FROM EDIT MODULE                             
CLTNMSV  DS    CL20             *  FROM EDIT MODULE                             
INTEGFLG DS    CL1              *  FROM EDIT MODULE                             
FCTRFLG  DS    CL1              *  FROM EDIT MODULE                             
ROUNDFLG DS    CL1              *  FROM EDIT MODULE                             
ROUND    DS    PL3              *  FROM EDIT MODULE                             
PKGOPT   DS    CL1              *  FROM EDIT MODULE                             
DEMOPT   DS    CL1              *  FROM EDIT MODULE                             
ASGNDOVR DS    CL1                                                              
FACTOR   DS    F                *  FROM EDIT MODULE                             
RELO     DS    F                *  FROM EDIT MODULE                             
EQIVAL   DS    CL1              *  FROM EDIT MODULE                             
CPMS     DS    CL1              *  FROM EDIT MODULE                             
UNASIGND DS    CL1              *  FROM EDIT MODULE                             
PFBOPT   DS    CL1              *  FROM EDIT MODULE                             
SOONRUN  DS    CL1              *  FROM EDIT MODULE                             
CABOPT   DS    CL1              *  FROM EDIT MODULE                             
*                                                                               
MYSAVE   EQU   *                                                                
ASVAREA  DS    F                                                                
DEFRECS  DS    F                                                                
ANTDMBLK DS    A                                                                
ADEDBLK  DS    A                                                                
ADFN     DS    F                ADDRESS OF DEFINESV AS BUMPED IN DFNR           
GRPTOTS  DS    PL4                 TOTAL FOR GRP                                
PAKWRK   DS    PL16                PACKED WORK AREA                             
CPP      DS    PL8                 COST PER POINT                               
ACTTOTS  DS    PL8                 TOTAL FOR ACTUAL DOLLARS                     
ASGNTOTS DS    PL8                 TOTAL FOR ASSIGNED DOLLARS                   
TEMPASGN DS    PL8                 ASSIGNED COST                                
PENNIES  DS    PL8                 PENNIES                                      
INTGTOT  DS    PL8                 INTEGRATION TOTAL (BBDO FEATURE)             
*                                                                               
RECSFSRT DS    CL8                    "                                         
FRST     DS    CL1                                                              
SEEDFLG  DS    CL1                 SEED FLAG                                    
PRDBKFLG DS    CL1                 PROD BREAK FLAG                              
UPDATFLG DS    CL1                 UPDATE UNT REC FLAG                          
BOXSET   DS    CL1                                                              
EBCDAT   DS    CL6                 EBSDIC DATE                                  
DEMONAME DS    CL15                                                             
*                                                                               
PKGCSTSV DS    PL8                                                              
PKGINTSV DS    F                                                                
MINSTOR  DS    F                                                                
MAXSTOR  DS    F                                                                
MYSVEND  EQU   *                                                                
         EJECT                                                                  
         SPACE                                                                  
*                                                                               
SORTRECD DSECT                                                                  
SORTREC  DS    0CL131       *****   SORTRECORD  *****                           
         DS    CL1                                                              
SRTCLT   DS    CL3                 CLIENT CODE (PRINTABLE)                      
SRTKLENE EQU   *-SORTREC                                                        
SRTEST   DS    CL1                 ESTIMATE  (NBACTEST)                         
SRTNET   DS    CL4                 NETWORK   (NBACTNET)                         
SRTPKG   DS    CL1                 PACKAGE   (NBPACK)                           
SRTKLEN2 EQU   *-SORTREC                                                        
SRTPRD   DS    CL1                 PRODUCT   (NBPRD)                            
SRTPRGCD DS    CL6                 PROGRAM CODE   (NBACTPRG)                    
SRTDTE   DS    CL2                 DATE(NBACTDAT)                               
SRTDSUB  DS    CL1                 -SUBLINE(NBACTSUB)                           
SRTDAY   DS    CL3                 DAY  (NBDAYNAM)                              
SRTTIME  DS    CL11                START-END TIME (NBTIME)                      
SRTPRGNM DS    CL16                PROGRAM NAME (NBPRGNAM)                      
SRTGRP   DS    CL4                 GRP (1DEC)                                   
SRTACTDL DS    CL4                 ACTUAL DOLLARS                               
SRTPRDCD DS    CL3                 PRINTABLE PRODUCT CODE                       
SRTPRDNM DS    CL20                PRODUCT NAME                                 
SRTCLTNM DS    CL20                CLIENT NAME                                  
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
       ++INCLUDE NEMEDFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEMEDEDD                                                       
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
       ++INCLUDE DDBIGBOX                                                       
         EJECT                                                                  
NDBLK    DSECT                                                                  
       ++INCLUDE NETDEMOT                                                       
       ++INCLUDE DEDBLOCK                                                       
         EJECT                                                                  
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE NEGENPACK                                                      
       ++INCLUDE NEGENUNIT                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002NEMED6DX  05/01/02'                                      
         END                                                                    
