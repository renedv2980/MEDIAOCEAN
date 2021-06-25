*          DATA SET SPNWS37    AT LEVEL 219 AS OF 06/19/12                      
*PHASE T20737A,*                                                                
T20737   TITLE 'BWS37 - BUYERS WORK SHEET - TRANSFER TO BUY PROGRAM'            
T20737   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 EXTRAWKL,T20737**,RA,RR=R8                                       
         USING TWAD,R5             R5=A(TWA)                                    
         USING SAVAREA,R6          R6=A(SAVE AREA)                              
         USING WORKD,R7            R7=A(GLOBAL W/S)                             
         LR    R1,RC                                                            
         L     RC,APALOCAL                                                      
         USING LOCALD,RC           RC=A(LOCAL W/S)                              
         ST    R1,AEXTRAWK         SAVE A(EXTRA WORKING STORAGE)                
         ST    R8,APRELO           R8=RELOCATION FACTOR                         
         ST    RB,APBASE1                                                       
         ST    RA,APBASE2                                                       
*                                                                               
         L     R1,ATIA                                                          
         LA    R1,2048(R1)                                                      
         ST    R1,LARECTAB         SET A(RECORD TABLE)                          
*                                                                               
         LA    R0,AXTRAN           SET EXTENTION ROUTINES                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         L     R1,=A(EXTRA)                                                     
         AR    R1,R8                                                            
         ST    R1,AXTRA(RE)                                                     
         STC   RF,AXTRA(RE)                                                     
         LA    RF,1(RF)                                                         
         LA    RE,4(RE)                                                         
         BCT   R0,*-16                                                          
*                                                                               
         L     R1,=A(BUYDESC)      SET A(BUY DESCRIPTION ADD ROUTINE)           
         AR    R1,R8                                                            
         ST    R1,ABUYDESC                                                      
         L     R1,=A(DEMADD)       SET A(DEMO ELEMENT ADD ROUTINE)              
         AR    R1,R8                                                            
         ST    R1,ADEMADD                                                       
*                                                                               
         MVC   APPARM,COMPARM                                                   
         LA    R3,APRECKEY                                                      
         USING BWDRECD,R3                                                       
*                                                                               
         CLI   APMODE,APMVALP                                                   
         BE    VALPAR                                                           
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE SELECT PARAMETERS                                          *         
* OUTPUT : FVMSGNO NE FVOK IF KEY IS INVALID                          *         
*          BWS RECORDS TRANSFERRED TO BUY PROGRAM                     *         
***********************************************************************         
         SPACE 1                                                                
*ALPAR   TM    CUSTAT,CUSDDS       TEST DDS TERMINAL                            
*        BO    VALP1                                                            
***********************************************************************         
*        NEW NWS RECALL OPTION      10/02/02   MHC                    *         
***********************************************************************         
VALPAR   DS    0H                                                               
****  RE-EXTRACT CAMPAIGN INFORMATION TO MAKE SURE THINGS ARE GOOD              
         MVC   APFULL(L'BCAM),BCAM                                              
         XC    BCAM,BCAM                                                        
         GOTO1 AGETCAM,APFULL      GET CAMPAIGN INFO                            
         BNE   VALPX                - SOMETHING'S WRONG, GET OUT!               
         XC    APFULL,APFULL                                                    
****  RE-EXTRACT CAMPAIGN INFORMATION TO MAKE SURE THINGS ARE GOOD              
         TM    BWSKEYH+4,X'20'     BOTH KEY FIELDS VALIDATED?                   
         BZ    *+12                                                             
         TM    BWSKY2H+4,X'20'                                                  
         BNZ   *+8                 YES                                          
         MVI   SVNWSOMF,0          NO                                           
*                                                                               
         TM    SVNWSOMF,NWSOMCON   PRESS ENTER TO CONTINUE?                     
         BZ    VALP1                                                            
         MVI   SVNWSOMF,0                                                       
         ICM   RD,15,TWAB4GNL                                                   
         B     EXIT                                                             
*                                                                               
VALP1    GOTO1 AVALPARM,BWSKY2H    VALIDATE SELECT PARAMETERS                   
         BNE   VALPX                                                            
         OI    BWSKY2H+4,X'20'     VALIDATED?                                   
         TM    CLTIND2,CLTFROZN    CLIENT IS FROZEN?                            
         BNZ   ECLT                                                             
         TM    TWAFLAG,TWANODET    TEST FOR NO DETAIL RECORDS                   
         BNZ   VALPX                                                            
         TM    ESTCNTRL,X'08'      TEST ESTIMATE LOCKED                         
         BO    EEST                                                             
         OC    ESTPW,ESTPW                                                      
         BZ    VALP1A05                                                         
         GOTO1 =A(GETPW),RR=APRELO                                              
         BE    VALP1A00                                                         
         TM    ESTIND,ESTICS2                                                   
         BZ    PWLCKED                                                          
         B     C2LCKED                                                          
*                                                                               
VALP1A00 TM    ESTIND,ESTICS2      COST2 INSTEAD OF PW?                         
         BZ    VALP1A05            NO, REGULAR PW                               
         L     RE,AIOAREA1                                                      
         USING PWRECD,RE                                                        
         LA    RE,PWEL                                                          
         XR    R0,R0                                                            
VALP1A01 CLI   0(RE),0             IF NO MARKET LEVEL COS2                      
         BE    VALP1A05            THEN DEFAULT IS ESTIMATE'S COS2              
         CLI   0(RE),C2STCODQ      COS2 ELEMENT (X'04')?                        
         BE    VALP1A02                                                         
         IC    R0,1(RE)                                                         
         AR    RE,R0                                                            
         B     VALP1A01                                                         
*                                                                               
         USING C2STEL,RE                                                        
VALP1A02 MVC   ESTPW,C2STFCTR+1    USE CURRENT CS2 FACTOR                       
         DROP  RE                                                               
*                                                                               
VALP1A05 MVI   LIND,0              INITIALIZE                                   
         OI    LIND,LFSTREC                                                     
         MVI   LIND2,0                                                          
         ZAP   LBYLNLO,=P'0'                                                    
         ZAP   LBYLNHI,=P'0'                                                    
         ZAP   LTOTBYLN,=P'0'                                                   
         L     RE,AEXTRAWK                                                      
         LA    RF,EXTRAWKL                                                      
         XCEFL ,                                                                
         XC    LSTASAV,LSTASAV                                                  
         XC    LSTALIST,LSTALIST                                                
         GOTO1 VDATCON,APPARM,(3,CMPSTDT),(2,LCMPST) START DATE                 
*                                                                               
         GOTO1 =A(GETFLTRC),RR=APRELO                                           
*********                                                                       
         MVI   SVESTFL1,0                                                       
*********                                                                       
         LA    R2,IOKEY                                                         
         USING ESTHDRD,R2                                                       
         XC    EKEY,EKEY           SEE IF POOL ESTIMATE IS OPEN                 
         MVC   EKEYAM,BAGYMD                                                    
         MVC   EKEYCLT,BCLT                                                     
         MVC   EKEYPRD,=C'POL'                                                  
         MVC   EKEYEST,BEST                                                     
*                                                                               
         GOTO1 AIO,DIRHI+IO1                                                    
         BNE   *+14                                                             
         CLC   EKEY,IOKEYSAV                                                    
         BE    VALP1B              YES                                          
********                                                                        
* CANADIAN AGENCIES REQUIRE POL ESTIMATE TO EXIST BEFORE TRANSFERRING           
* FOR MEDIAS T AND N                                                            
********                                                                        
*****    CLI   APROF7,C'C'         CANADIAN AGENCY?                             
*****    BNE   VALP1A10                                                         
         TM    APROFBTS,A00CANAD   CANADIAN AGENCY?                             
         BZ    VALP1A10                                                         
*                                                                               
         CLI   QMED,C'T'           MEDIA T OR N?                                
         BE    ENOPOLES                                                         
         CLI   QMED,C'N'                                                        
         BE    ENOPOLES            ON POL ESTIMATE OPEN YET                     
********                                                                        
VALP1A10 MVC   FVMSGNO,=AL2(FVFOK)                                              
         TM    CLTIND2,CLTPONLY    POL ONLY?                                    
         BNZ   VALP2               TREAT AS BRAND-ESTIMATE FOR OLD ONES         
         CLI   CLTPROF,C'0'        NO-IF CLIENT DEFINED FOR BRAND POL,          
         BH    VALP2                  GET OUT NOW                               
*                                                                               
         CLI   BPRD,FF             CHECK PRODUCT NOT POOL                       
         BNE   VALP2                                                            
         DC    H'0'                                                             
*                                                                               
VALP1B   DS    0H                                                               
         GOTO1 AIO,FILGET1         YES - GET POOL ESTIMATE HDR                  
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIOAREA1                                                      
         MVC   SVESTFL1,EFLAG1     COPY POL ESTIMATE FLAG 1                     
         TM    SVESTFL1,EF1SDE     SUPERDESK AUTHORIZATION OPEN?                
         BNO   *+12                                                             
         L     RF,AEXTRAWK                                                      
         USING EXTRAWKD,RF                                                      
         OI    XTRAFLG1,XSDE       YES                                          
         DROP  RF                                                               
         TM    ECNTRL,X'08'        IS POL ESTIMATE LOCKED?                      
         BO    EEST                YES, TRANSFER NOT ALLOWED                    
*                                                                               
         MVC   LPOLDEMS,EDEMLST    SAVE POOL DEMOS                              
*                                                                               
         OC    ECOST2,ECOST2       USE ESTIMATE'S COST2 ?                       
         BZ    VALP1B00            NO                                           
         XC    SVCOST2,SVCOST2                                                  
         TM    ESTIND,ESTICS2                                                   
         BNZ   *+14                                                             
         MVC   SVCOST2,ECOST2      POL ESTIMATE OVERRIDE                        
         BNZ   VALP1B00                                                         
         MVC   SVCOST2+1(3),ESTPW  COST2 RECORD CURRENT FACTOR                  
*                                                                               
VALP1B00 BRAS  RE,COS2CHK                                                       
         BNE   VALP1B05                                                         
         OI    LIND2,LCOS2                                                      
*                                                                               
VALP1B05 OC    SVESLN,SVESLN       GOT ESTIMATE BUY ONLY THIS SLN?              
         BNZ   *+10                YES                                          
         MVC   SVESLN,ESLN         NO, USE POL ESTIMATE OVERRIDE                
*                                                                               
*****    XC    SVELOCK(SVELKNDT+L'SVELKNDT-SVELOCK),SVELOCK                     
         OC    SVCLOCK,SVCLOCK                                                  
         BNZ   VALP1B10                                                         
         OC    ELOCKYM,ELOCKYM     USE ESTIMATE'S LOCK MONTH                    
         BZ    VALP1B10            NO                                           
         MVC   SVELOCK,ELOCKYM                                                  
         MVC   APDUB(2),SVELOCK                                                 
         NI    APDUB+1,X'3F'       DROP PRIOR/SUBSEQ FLAGS                      
         MVI   APDUB+2,1                                                        
         GOTO1 VDATCON,APPARM,(3,APDUB),APWORK                                  
         GOTO1 VGTBROAD,APPARM,(1,APWORK),APWORK+6,VGETDAY,VADDAY               
         GOTO1 VDATCON,APPARM,APWORK+6,(2,APFULL)                               
         GOTO1 (RF),(R1),APWORK+12,(2,APFULL+2)                                 
         MVC   SVELKSDT(L'APFULL),APFULL                                        
         TM    SVELOCK+1,X'80'     TEST MONTH AND PRIOR                         
         BZ    *+10                                                             
         XC    SVELKSDT,SVELKSDT                                                
         TM    SVELOCK+1,X'40'     TEST MONTH AND SUBSEQUENT                    
         BZ    *+12                                                             
         LHI   R0,-1                                                            
         STCM  R0,3,SVELKNDT                                                    
*                                                                               
VALP1B10 OI    LIND,LPOL           INDICATE POOL BUY                            
         CLI   BPRD,FF             TEST PRD=POL                                 
         BNE   VALP2                                                            
         CLI   INOPRD,0            YES-TEST PRODUCT ALLOCATED                   
         BNE   VALP1C                                                           
         CLI   CMPPRD1,0           NO-ERROR IF NO CAMPAIGN DEFINED              
         BNE   VALP2                  PIGGYBACKS EITHER,                        
         CLI   CLTPROF,C'0'           AND BRAND POL CLIENT                      
         BH    VALP97                                                           
         B     VALP2                                                            
*                                                                               
VALP1C   XC    APHALF,APHALF       BRAND PRODUCT'S ESLN                         
*                                                                               
         LA    R2,IOKEY            PRODUCT IS ALLOCATED-                        
         XC    EKEY,EKEY           CHECK ESTIMATE IS OPEN FOR PRD               
         MVC   EKEYAM,BAGYMD                                                    
         MVC   EKEYCLT,BCLT                                                     
         MVC   EKEYPRD,POLPRD1                                                  
         MVC   EKEYEST,BEST                                                     
         GOTO1 AIO,DIRRD+IO2                                                    
         BNE   EENO                                                             
*                                                                               
         GOTO1 AIO,FILGET2         YES - GET BRAND ESTIMATE HDR                 
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIOAREA2                                                      
         TM    EFLAG1,EF1SDE     SUPERDESK AUTHORIZATION OPEN?                  
         BNO   *+12                                                             
         L     RF,AEXTRAWK                                                      
         USING EXTRAWKD,RF                                                      
         OI    XTRAFLG1,XSDE       YES                                          
         DROP  RF                                                               
         OC    ESLN,ESLN           GOT ESTIMATE BUY ONLY THIS SLN?              
         BZ    VALP1D              NO                                           
         MVC   APHALF(L'ESLN),ESLN                                              
*                                                                               
VALP1D   CLI   INOPRD+1,0          TEST 2ND ALLOCATED PRODUCT                   
         BE    VALP1E                                                           
         LA    R2,IOKEY                                                         
         XC    EKEY,EKEY           YES-CHECK ESTIMATE IS OPEN FOR PRD           
         MVC   EKEYAM,BAGYMD                                                    
         MVC   EKEYCLT,BCLT                                                     
         MVC   EKEYPRD,POLPRD2                                                  
         MVC   EKEYEST,BEST                                                     
         GOTO1 AIO,DIRRD+IO2                                                    
         BNE   EENO                                                             
*                                                                               
         GOTO1 AIO,FILGET2         YES - GET BRAND ESTIMATE HDR                 
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIOAREA2                                                      
         TM    EFLAG1,EF1SDE     SUPERDESK AUTHORIZATION OPEN?                  
         BO    *+12                                                             
         L     RF,AEXTRAWK                                                      
         USING EXTRAWKD,RF                                                      
         NI    XTRAFLG1,X'FF'-X'20'    NO, PIGGYBACK NOT OPEN                   
         DROP  RF                                                               
         OC    ESLN,ESLN           GOT ESTIMATE BUY ONLY THIS SLN?              
         BZ    VALP1E              NO                                           
         MVC   APHALF+1(L'ESLN),ESLN                                            
*                                                                               
VALP1E   OC    APHALF,APHALF       ANY BRAND ESLN?                              
         BZ    VALP2                                                            
         CLC   APHALF(1),APHALF+1  NON-ZERO, ARE THEY THE SAME?                 
         BE    VALP1G                                                           
         CLI   APHALF+1,0                    NO, ONLY PRD1 ESLN?                
         BE    VALP1G                            YES                            
         CLI   APHALF,0                          ONLY PRD2 ESLN?                
         BE    VALP1F                            YES                            
         MVC   FVMSGNO,=AL2(FVESLNER)                                           
         B     VALPOPER                                                         
*                                                                               
VALP1F   MVC   SVESLN,APHALF+1                                                  
         B     VALP2                                                            
*                                                                               
VALP1G   MVC   SVESLN,APHALF                                                    
*                                                                               
VALP2    XC    LDTINDS,LDTINDS                                                  
         OC    INFDATES,INFDATES   TEST DATE FILTERS                            
         BNZ   *+16                                                             
         TM    CLTIND,CLTITRDT     NO-TEST PROFILE REQUIRES THEM                
         BO    VALP99                                                           
         B     VALP2A                                                           
         GOTO1 AVALDT              YES-VALIDATE DATES                           
         BNE   VALPX                                                            
*                                                                               
VALP2A   GOTO1 VDATCON,APPARM,ESTST,(2,LESTST)  ESTIMATE START                  
         GOTO1 (RF),(R1),ESTND,(2,LESTND)       AND END DATES                   
*                                                                               
         L     RE,LARECTAB         CLEAR THE RECORD TABLE                       
         ST    RE,LANXTREC                                                      
         LHI   RF,14*1024          14K NOW, WAS 8K AND 6K B4 THAT               
         LA    R1,0(RE,RF)         WE'VE COME A LONG WAY                        
         ST    R1,LARECTBX                                                      
         XCEFL                                                                  
*                                                                               
         SR    R8,R8               R8=RECORD COUNT                              
*                                                                               
         LA    R4,NBRKSTA-NBRKEY   SET EXECUTE LENGTH FOR KEY COMPARE           
         OC    QSTA,QSTA                                                        
         BZ    *+8                                                              
         LA    R4,NBRKKBUY-NBRKEY                                               
         BCTR  R4,0                                                             
         STC   R4,LKEYCOMP                                                      
*                                                                               
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         USING NBRKEY,R2                                                        
         MVI   NBRKTYP,NBRKTYPQ                                                 
         MVI   NBRKSTY,NBRKSTYQ                                                 
         MVC   NBRKAGMD,BAGYMD                                                  
         OC    NBRKAGMD,BBYRMASK                                                
         MVC   NBRKBYR,BBYR                                                     
         MVC   NBRKSEQ,BCMSEQ                                                   
         OC    QSTA,QSTA           FOR CABLE SYSTEM  9999/  QSTA IS NOT         
         BZ    *+10                    SET BECAUSE IT IS MISSING THE            
         MVC   NBRKSTA,BSTA            NETWORK                                  
         DROP  R2                                                               
*                                                                               
         XC    APRECKEY,APRECKEY   RE-ADJUST APRECKEY                           
         MVC   APRECKEY,IOKEY                                                   
*                                                                               
VALP3    LA    R1,DIRHI+IO2        READ THE NWS BUY REVISION RECORDS            
         B     VALP4+4                                                          
*                                                                               
VALP4    LA    R1,DIRSQ+IO2                                                     
         LA    R2,IOKEY                                                         
         USING NBRKEY,R2                                                        
         GOTO1 AIO                                                              
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFOK)   TEST REACHED END                           
         B     VALP6                                                            
*                                                                               
         ZIC   R4,LKEYCOMP                                                      
         EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   IOKEY(0),IOKEYSAV                                                
         BNE   VALP6                                                            
*                                                                               
         OC    QCABLE,QCABLE         ANY CABLE SYSTEM FILTER?                   
         BZ    VALP4A                                                           
         XC    APWORK,APWORK                                                    
         MVC   APWORK+2(L'NBRKSTA),NBRKSTA                                      
         GOTO1 VMSUNPK,APPARM,(X'80',APWORK),APWORK+5,APWORK+9                  
         CLC   QCABLE(4),APWORK+9    YES, MATCH CABLE SYSTEM?                   
         BNE   VALP4                      NO                                    
         DROP  R2                                                               
*                                                                               
VALP4A   GOTO1 AIO,FILGET2                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R3,AIOAREA2                                                      
         USING NBRKEY,R3                                                        
         LA    R3,NBRFSTEL         LOOK FOR SAVED DATA ELEMENT                  
VALP4B   CLI   0(R3),0                                                          
         BE    VALP4               DOESN'T EXIST, CAN'T TRANSFER                
         CLI   0(R3),NBRSELQ                                                    
         BE    VALP4C                                                           
         ZIC   RE,1(R3)                                                         
         AR    R3,RE                                                            
         B     VALP4B                                                           
*                                                                               
         USING NBRSELD,R3                                                       
VALP4C   CLI   BDPT,0              CHECK DAYPART/LENGTH FILTERS                 
         BE    *+14                                                             
         CLC   BDPT,NBRSDYPT                                                    
         BNE   VALP4                                                            
*                                                                               
         CLI   BSLN,0                                                           
         BE    VALP5                                                            
         CLC   BSLN,NBRSSLN                                                     
         BNE   VALP4                                                            
*                                                                               
VALP5    LA    R8,1(R8)            AUGMENT RECORD COUNT                         
         BAS   RE,SAVEREC          SAVE RECORD DETAILS IN TABLE                 
         B     VALP4               NEXT RECORD                                  
         DROP  R3                                                               
*                                                                               
VALP6    LTR   R8,R8               TEST ANY RECORDS                             
         BZ    VALP92                                                           
*                                                                               
         LA    R9,RTABL            SORT THEM                                    
         LA    R0,L'RTSORT                                                      
*                                                                               
         GOTO1 VXSORT,APPARM,(0,LARECTAB),(R8),(R9),(R0),0                      
*                                                                               
         L     R4,LARECTAB                                                      
         USING RTABD,R4                                                         
         MVC   IOKEY(L'NBRKEY),APRECKEY                                         
*                                                                               
VALP7    ST    R4,LANXTREC                                                      
         OC    RTSORT,RTSORT       TEST END OF RECORDS                          
         BZ    VALP40                                                           
******** OC    QSTA,QSTA           TEST STATION FILTER                          
******** BNZ   VALP7A                                                           
         CLC   RTSTA,LSVSTA        NO-TEST NEW STATION                          
         BE    VALP7A                                                           
         MVC   LSVSTA,RTSTA        YES-GET STATION DETAILS                      
         XC    APDUB,APDUB                                                      
         MVC   APDUB+2(3),0(R6)                                                 
         GOTO1 VMSUNPK,APPARM,(X'80',APDUB),APWORK,APWORK+4                     
         CLI   APWORK+4+4,C' '                                                  
         BNE   *+16                                                             
         CLI   QMED,C'T'                                                        
         BNE   *+8                                                              
         MVI   APWORK+4+4,C'T'                                                  
         GOTO1 AGETSTA,APWORK+4                                                 
         MVC   LPST,ACWORK+24                                                   
         USING EXTRAWKD,RF                                                      
         L     RF,AEXTRAWK                                                      
         NI    XTRAFLG1,X'FF'-X'40'  REPEAT CHKAUTH CODE                        
         DROP  RF                                                               
*                                                                               
VALP7A   LA    R3,IOKEY                                                         
         USING NBRKEY,R3                                                        
         MVC   NBRKSTA,RTSTA                                                    
         MVC   NBRKKBUY,RTKBUYS                                                 
         MVC   NBRKNBSQ,RTSEQ                                                   
         LA    R1,DIRHI+IO2                                                     
         B     VALP8+4                                                          
*                                                                               
VALP8    LA    R1,DIRSQ+IO2                                                     
         GOTO1 AIO                                                              
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     VALP12                                                           
*                                                                               
         CLC   IOKEY(L'NBRKEY),IOKEYSAV                                         
         BNE   VALP12                                                           
*                                                                               
         GOTO1 AIO,FILGET2                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R3,AIOAREA2                                                      
         SR    RE,RE                                                            
         LA    R3,NBRFSTEL         FIND SAVED DATA ELEMENT                      
VALP8A   CLI   0(R3),0                                                          
         BE    VALP8                                                            
         CLI   0(R3),NBRSELQ                                                    
*****    BE    VALP8B              PROTECTIVE CODE WHERE NBRSBYLN               
         BE    VALP8C                 WAS NOT SET CORRECTLY (REX1A)             
         IC    RE,1(R3)                                                         
         AR    RE,R3                                                            
         B     VALP8A                                                           
*&&DO                                                                           
VALP8B   CLI   NBRSBYLN-NBRSELD(R3),0   ANY BUYLINE IN DESC ELEM?               
         BNE   VALP8C                                                           
         L     RF,AIOAREA2              NONE, ANY BUYLINE IN KEY?               
         OC    NBRKKBUY-NBRKEY(3,RF),NBRKKBUY-NBRKEY(RF)                        
         BNZ   VALP8                          YES, INVALID RECORD               
*&&                                                                             
VALP8C   MVC   LDETKEY,IOKEY       SAVE THE KEY                                 
         MVC   BSTA,RTSTA          BINARY STATION VALUE                         
         L     RE,AEXTRAWK                                                      
         USING EXTRAWKD,RE                                                      
         MVI   NBRCSNUM,0                                                       
         XC    NBRCSTBL,NBRCSTBL                                                
         DROP  RE                                                               
*                                                                               
VALP9    TM    LIND,LFSTREC        TEST FIRST DETAIL RECORD                     
         BZ    VALP10                                                           
         NI    LIND,FF-LFSTREC     YES-                                         
         GOTO1 ACHKID              CHECK BUY ID OPTION, IF ANY                  
         BNE   VALPX                                                            
*                                                                               
VALP10   MVI   LFLAG,0                                                          
         TM    CMPOPTS,CAMODLY     TEST DAILY SKED                              
         BO    *+8                                                              
         OI    LFLAG,LWEEKLY       NO-INDICATE WEEKLY SKED                      
****     CLI   BWDKELPO,0          TEST PACKAGE/ORBIT                           
****     BE    VALP11                                                           
****     TM    BWDINDS,BWDIORB     YES - TEST ORBIT                             
****     BZ    VALP13                                                           
****     OI    LFLAG,LORBIT        YES                                          
*                                                                               
VALP11   DS    0H                                                               
         OC    SVESLN,SVESLN       ANY "BUY ONLY THIS SLN"?                     
         BZ    VALP11A             NONE                                         
         CLC   SVESLN,NBRSSLN-NBRSELD(R3)                                       
         BE    VALP11A                                                          
         L     RF,AEXTRAWK                                                      
         USING EXTRAWKD,RF                                                      
         OI    XTRAFLG1,XF1NXESL   SOME LINES NOT XFR CAUSE OF SVESLN           
         B     VALP8               SO NOTHING WRITTEN FOR THIS DETAIL           
         DROP  RF                                                               
*                                                                               
VALP11A  BAS   RE,TRANSFER         NON-PACKAGE TRANSFER                         
         BNE   VALPX                                                            
****     CLI   BWDKELPO,0          TEST ORBIT                                   
****     BNE   VALP12                                                           
         MVC   IOKEY(13),LDETKEY   SET POINTER TO DETAIL RECORD KEY             
         GOTO1 AIO,DIRHI+IO2+IORDEL                                             
         B     VALP8                                                            
*                                                                               
VALP12   L     R4,LANXTREC                                                      
         LA    R4,RTABL(R4)                                                     
         B     VALP7                                                            
*&&DO                                                                           
VALP13   OI    LFLAG,LPKG          PACKAGE ---                                  
         MVC   LPKGMAKY,BWDKEY     SAVE PACKAGE MASTER KEY                      
         XC    LPKGSLAV,LPKGSLAV                                                
         MVI   LPKGMAST,0                                                       
         XC    LPKGMADA,LPKGMADA                                                
         BAS   RE,GETELEMS         SCAN RECORD FOR ELEMENTS                     
         ICM   R1,15,LABTREL       TEST FOR PREVIOUS BUY TRANSFER               
         BZ    *+14                                                             
         OI    LFLAG,LPKGREX       YES                                          
         MVC   LPKGMAST,BTRLINE-BTREL(R1)   SAVE MASTER LINE NUM                
         LA    R3,IOKEY                                                         
         MVI   BWDKELDY,0                                                       
         XC    BWDKELTM,BWDKELTM                                                
         MVI   BWDKELSQ,1                                                       
         LA    R1,MINHI2           READ PACKAGE LINES                           
         B     VALP14+4                                                         
*                                                                               
VALP14   LA    R1,MINSEQ2                                                       
         GOTO1 AMIN                                                             
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     VALP16                                                           
         CLC   IOKEY(BWDKELSQ-BWDKEY),IOKEYSAV                                  
         BNE   VALP16                                                           
         L     R3,AIOAREA2                                                      
         MVC   LDETKEY,BWDKEY      SAVE THE KEY                                 
         BAS   RE,TRANSFER         TRANSFER                                     
         BNE   VALPX                                                            
         TM    BWDINDS,BWDITRLK    TEST TRANSFER LOCK-OUT                       
         BO    VALP94              YES - ERROR EXIT                             
         MVC   IOKEY(13),LDETKEY   READ ALL PACKAGE LINES                       
         GOTO1 AMIN,MINHI2                                                      
         B     VALP14                                                           
*                                                                               
VALP16   L     R2,AIOAREA4         ALL PACKAGE LINES TRANSFERRED --             
         USING BUYRECD,R2                                                       
         TM    LFLAG,LPKGREX       TEST RE-TRANSFER                             
         BZ    VALP20                                                           
         OC    LPKGMADA,LPKGMADA   YES - TEST PACKAGE MASTER WAS READ           
         BZ    VALP94                    NO - TRANSFER LOCK-OUT                 
         CLI   LPKGSLAV,0          TEST ANY NEW SLAVES                          
         BE    VALP28              NO - PUT PACKAGE MASTER                      
         XC    APELEM,APELEM       YES --                                       
         SR    R0,R0               SEARCH RECORD FOR PACKAGE ELEMENT            
         LA    R4,BDELEM                                                        
*                                                                               
VALP18   CLI   0(R4),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R4),5                                                          
         BE    *+14                                                             
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     VALP18                                                           
         USING PKGELEM,R4                                                       
         ZIC   R8,PKGLEN           PACKAGE ELEMENT FOUND                        
         BCTR  R8,0                                                             
         EX    R8,*+8              DELETE IT                                    
         B     *+10                                                             
         MVC   APELEM(0),PKGELEM                                                
         GOTO1 ADELELS,BUYREC                                                   
         LA    R4,APELEM           ADD NEW SLAVES TO IT                         
         LA    R8,PKGELEM+1(R8)                                                 
         B     VALP22                                                           
*                                                                               
VALP20   LA    R4,APELEM           NOT RE-TRANSFER - BUILD PKG ELEM             
         MVI   PKGCODE,5                                                        
         MVI   PKGIND,1                                                         
         LA    R8,PKGLINES                                                      
VALP22   LA    R9,LPKGSLAV         MOVE SLAVES TO PACKAGE ELEMENT               
         CLI   0(R9),0             IF NO SLAVES, DON'T ADD PACKAGE ELEM         
         BE    VALP28                                                           
*                                                                               
VALP24   CLI   0(R9),0                                                          
         BE    VALP26                                                           
         MVC   0(1,R8),0(R9)                                                    
         LA    R8,1(R8)                                                         
         LA    R9,1(R9)                                                         
         B     VALP24                                                           
*                                                                               
VALP26   SR    R8,R4               SET ELEMENT LENGTH                           
         STC   R8,PKGLEN                                                        
         GOTO1 AADDELS,BUYREC      ADD PACKAGE ELEMENT                          
*                                                                               
VALP28   NI    LFLAG,FF-LPKG                                                    
         TM    LFLAG,LPKGREX       TEST RE-TRANSFER                             
         BZ    VALP30                                                           
         MVC   IODA,LPKGMADA       YES - PUT PACKAGE MASTER                     
         LH    R1,=Y(IODA3-WORKD)                                               
         LA    R1,0(R1,R7)                                                      
         MVC   0(4,R1),LPKGMADA                                                 
         GOTO1 AIO,FILGETU3                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,AIOAREA3                                                      
         MVC   IOADDR,AIOAREA4                                                  
         GOTO1 APUTBUY                                                          
         BNE   VALPX                                                            
         B     VALP32                                                           
*                                                                               
VALP30   CLI   LPKGMAST,0          NO - TEST ANY TRANSFER                       
         BE    VALP32                                                           
         MVC   IOADDR,AIOAREA4     YES - ADD PACKAGE MASTER                     
         GOTO1 AADDREC                                                          
         MVC   IOKEY(13),LPKGMAKY       GET PACKAGE HEADER                      
         GOTO1 AMIN,MINRD2                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,AIOAREA2         ADD BUY TRANSFER ELEMENT WITH                
         XC    APELEM,APELEM       PACKAGE MASTER BUY LINE                      
         LA    R4,APELEM                                                        
         USING BTREL,R4                                                         
         MVI   BTRELCD,BTRELCDQ                                                 
         LA    RE,BTRLINE+1-BTREL                                               
         STC   RE,BTRELLN                                                       
         MVC   BTRLINE,LPKGMAST                                                 
         MVC   BTRDATE,ASBDAT      TODAY'S DATE                                 
         GOTO1 AADDELS,BWDRECD                                                  
         GOTO1 AMIN,MINWRT2                                                     
         BE    VALP32                                                           
         DC    H'0'                                                             
*                                                                               
VALP32   L     R4,LANXTREC         NEXT RECORD                                  
         LA    R4,RTABL(R4)                                                     
         B     VALP7                                                            
*&&                                                                             
*                                  ALL TRANSFER DONE ---                        
*                                                                               
VALP40   TM    CMPOPTS2,CAMOBYRN   TEST TO TRANSFER BUYER NAME                  
         BO    *+12                OPTION SET EITHER IN CAMPAIGN                
         TM    CLTIND,CLTITRBN     OR IN CLIENT BWA PROFILE                     
         BZ    VALP41                                                           
         GOTO1 ABYRNAME            YES                                          
*                                                                               
*VALP41  TM    CLTIND,CLTIADDS     TEST ADDS USER                               
*        BZ    VALP42                                                           
VALP41   TM    AFLAG1,X'80'        TEST ADDS USER                               
         BZ    VALP42                                                           
         GOTO1 AADDS               YES-INFORM ADDS OF CHANGES                   
         BNE   VALPX                                                            
*                                                                               
VALP42   CLI   CLTBWPRO+2,C'N'     TEST FOR BUY TURNAROUND                      
         BE    VALP43                                                           
         GOTO1 AADDREQ                                                          
         BNE   TRANX                                                            
*                                                                               
VALP43   MVC   FVMSGNO,=AL2(FVFSET)    OUTPUT A SENSIBLE MESSAGE                
         XC    BWSMSG,BWSMSG                                                    
         MVC   BWSMSG(L'EXFRM1),EXFRM1                                          
         LA    R1,BWSMSG+8                                                      
         OC    QSTA,QSTA           TEST FOR STATION FILTER                      
         BNZ   VALP50                                                           
*                                                                               
         CP    LTOTBYLN,=P'1'      NO - OUTPUT THE BYLINES FOR EACH STA         
         BE    VALP44                                                           
         BL    VALP92                   NO BUY LINES ADDED                      
         MVI   0(R1),X'A2'         LOWER CASE 'S'                               
         LA    R1,1(R1)                                                         
*                                                                               
VALP44   MVC   1(L'EXFRM2,R1),EXFRM2                                            
         L     R8,AEXTRAWK                                                      
         AHI   R8,LSTABYTB-EXTRAWKD                                             
         OC    0(STABYL,R8),0(R8)  TEST ANY STATIONS TO DISPLAY                 
         BZ    VALP95                                                           
         USING STABYD,R8                                                        
         L     R4,AIOAREA3                                                      
         MVI   0(R4),C' '                                                       
         MVC   1(255,R4),0(R4)                                                  
*                                                                               
VALP45   OC    SBSTA,SBSTA         TEST NO MORE STATIONS                        
         BZ    VALP48                                                           
         MVC   0(5,R4),SBSTA                                                    
         CLI   4(R4),C'T'                                                       
         BNE   *+10                                                             
         MVI   4(R4),C' '                                                       
         BCTR  R4,0                                                             
         CLI   SBSTA,C'0'          TEST CABLE STATION                           
         BL    VALP46                                                           
         MVI   5(R4),C'/'          YES-DISPLAY NETWORK ALSO                     
         MVC   6(3,R4),SBSTA+5                                                  
         LA    R4,4(R4)                                                         
         CLI   SBSTA+7,C' '                                                     
         BH    VALP46                                                           
         BCTR  R4,0                                                             
*                                                                               
VALP46   MVI   5(R4),C'='                                                       
         UNPK  6(3,R4),SBLNLO                                                   
         OI    8(R4),X'F0'                                                      
         CP    SBLNLO,SBLNHI       TEST ONLY ONE BUYLINE                        
         BE    VALP47                                                           
         MVI   9(R4),C'-'                                                       
         UNPK  10(3,R4),SBLNHI                                                  
         OI    12(R4),X'F0'                                                     
*                                                                               
VALP47   LA    R4,15(R4)                                                        
         LA    R8,STABYL(R8)                                                    
         B     VALP45                                                           
*                                                                               
VALP48   L     R4,AIOAREA3                                                      
         GOTO1 VSQUASH,APPARM,(R4),256                                          
         L     RE,4(R1)                                                         
         CHI   RE,0                SQUASH CAME BACK WITH ZERO LENGTH?           
         BE    VALP95              YES, NOTHING TO DISPLAY                      
         BCTR  RE,0                                                             
         ZIC   RF,WRKCPEH                                                       
         AHI   RF,-9                                                            
         BM    VALP95                                                           
         CR    RE,RF               CHECK DISPLAY AREA IS BIG ENOUGH             
         BNH   *+6                                                              
         LR    RE,RF                                                            
         EX    RF,VPCLEAR                                                       
         EX    RE,VPMOVE                                                        
         OI    WRKCPEH+6,FVOXMT                                                 
         B     VALP95                                                           
*                                                                               
VPCLEAR  XC    WRKCPE(0),WRKCPE                                                 
VPMOVE   MVC   WRKCPE(0),0(R4)                                                  
*                                                                               
VALP50   CP    LBYLNLO,=P'0'       TEST ANY BUY LINES CREATED                   
         BE    VALP92                                                           
*                                                                               
         L     RF,AEXTRAWK                                                      
         USING EXTRAWKD,RF                                                      
         TM    XTRAFLG1,XF1NXESL   SOME LINES NOT XFR BECAUSE ESLN?             
         BZ    VALP55                                                           
         DROP  RF                                                               
VALP53   XC    BWSMSG,BWSMSG                                                    
         MVC   BWSMSG(L'EXFRM3),EXFRM3                                          
         MVC   BWSMSG+L'EXFRM3(L'EXFRM4),EXFRM4                                 
         ZIC   R1,SVESLN                                                        
         CVD   R1,APDUB                                                         
         UNPK  BWSMSG+L'EXFRM3(3),APDUB                                         
         OI    BWSMSG+L'EXFRM3+2,X'F0'                                          
         L     R1,AINP             TEST IF OVERLAY SET CURSOR                   
         OI    TIOBINDS-TIOBD(R1),TIOBALRM    SET ALARM TO BEEP                 
         B     VALP95                                                           
*                                                                               
VALP55   CP    LBYLNLO,LBYLNHI                                                  
         BE    *+12                                                             
         MVI   0(R1),X'A2'         LOWER CASE 'S'                               
         LA    R1,1(R1)                                                         
         UNPK  1(3,R1),LBYLNLO                                                  
         OI    3(R1),X'F0'                                                      
         CP    LBYLNLO,LBYLNHI                                                  
         BE    VALP90                                                           
         MVI   4(R1),C'-'                                                       
         UNPK  5(3,R1),LBYLNHI                                                  
         OI    7(R1),X'F0'                                                      
         LA    R1,4(R1)                                                         
*                                                                               
VALP90   MVC   5(L'EXFRM2,R1),EXFRM2                                            
         B     VALP95                                                           
***************                                                                 
* NO BUY LINES ADDED                                                            
***************                                                                 
VALP92   L     RF,AEXTRAWK                                                      
         USING EXTRAWKD,RF                                                      
         TM    XTRAFLG1,XF1NXESL   SOME LINES NOT XFR BECAUSE ESLN?             
         BNZ   VALP53                                                           
         DROP  RF                                                               
*                                                                               
         TM    LIND,LREXFR         NO BUY LINES ADDED -                         
         BO    *+14                                                             
         MVC   FVMSGNO,=AL2(FVNOXFR)        NO REGULAR TRANSFER                 
         B     VALP95                                                           
*                                                                               
         TM    LIND,LPUTREC        TEST ANY PUTREC'S                            
         BO    *+14                                                             
         MVC   FVMSGNO,=AL2(FVNOREX)        NO RE-TRANSFER                      
         B     VALP95                                                           
         MVC   FVMSGNO,=AL2(FVREXFR)        RE-TRANSFER COMPLETE                
         B     VALP95                                                           
*&&DO                                                                           
VALP94   MVC   IOKEY(13),LPKGMAKY  PACKAGE TRANSFER LOCK-OUT                    
         GOTO1 AMIN,MINRD2                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,AIOAREA2                                                      
         OI    BWDINDS,BWDITRLK                                                 
         GOTO1 AMIN,MINWRT2                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FVMSGNO,=AL2(FVPKGLOK)                                           
*&&                                                                             
VALP95   LA    R1,BWSKEYH                                                       
         ST    R1,FVADDR                                                        
         B     VALPX                                                            
*                                                                               
VALP97   MVC   FVMSGNO,=AL2(FVNOPRD)    PRODUCT OPTION MISSING                  
         B     VALPOPER                                                         
*                                                                               
VALP98   MVC   FVMSGNO,=AL2(FVNOPOLE)   POL ESTIMATE NOT OPEN                   
         B     VALPX                                                            
*                                                                               
VALP99   MVC   FVMSGNO,=AL2(FVNODAT)    DATES OPTION MISSING                    
*                                                                               
VALPOPER LA    R1,BWSOPTH          OPTIONS ERROR                                
         ST    R1,FVADDR                                                        
*                                                                               
VALPX    B     EXIT                                                             
         SPACE 1                                                                
EXFRM1   DC    CL8'Buy line'                                                    
EXFRM2   DC    CL26'transferred to buy program'                                 
EXFRM3   DC    CL33'** Records with a SLN other than '                          
EXFRM4   DC    CL27'000 were not transferred **'                                
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* SAVE RECORD DETAILS TO RECORD TABLE FOR SUBSEQUENT SORT             *         
*                                                                               
* ON ENTRY:    (R3)                A(SAVED DATA ELEMENT)                        
***********************************************************************         
         SPACE 1                                                                
SAVEREC  NTR1  ,                                                                
         USING NBRSELD,R3                                                       
         L     R8,LANXTREC         A(NEXT ENTRY IN TABLE)                       
         USING RTABD,R8                                                         
*                                                                               
         L     R2,AIOAREA2                                                      
         MVC   RTSTA,NBRKSTA-NBRRECD(R2)                                        
         MVC   RTKBUYS,NBRKKBUY-NBRRECD(R2)                                     
         MVC   RTSEQ,NBRKNBSQ-NBRRECD(R2)                                       
         LA    R4,SORTTAB                                                       
*&&DO                                                                           
         MVC   RTKSTACD,BWDKELST                                                
         CLI   BWDKELPO,0          TEST PACKAGE/ORBIT                           
         BE    SAVR2                                                            
         MVI   RTSEQ,5             YES-ORBITS 2ND TO LAST                       
         TM    BWDINDS,BWDIORB                                                  
         BO    SAVR10                                                           
         MVI   RTSEQ,6             PACKAGES LAST                                
         B     SAVR10                                                           
*                                                                               
SAVR2    CLI   0(R4),FF            GET DAY/TIME SEQUENCING CODE                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R4),0                                                          
         BE    *+14                                                             
         CLC   NBRSDAYS,0(R4)                                                   
         BNE   SAVR4                                                            
         CLC   NBRSTIMS(2),1(R4)                                                
         BL    SAVR4                                                            
         CLC   NBRSTIMS(2),3(R4)                                                
         BNL   SAVR4                                                            
         MVC   RTSEQ,5(R4)                                                      
         B     SAVR6                                                            
*                                                                               
SAVR4    LA    R4,6(R4)                                                         
         B     SAVR2                                                            
*&&                                                                             
SAVR6    MVI   RTDAYCD,1           DAY CODE                                     
         CLI   NBRSDAYS,X'7C'      1=MO-FR                                      
         BE    SAVR8                                                            
         MVI   RTDAYCD,7           7=MO-SU                                      
         CLI   NBRSDAYS,X'7F'                                                   
         BE    SAVR8                                                            
         MVI   RTDAYCD,8           8=SA-SU                                      
         CLI   NBRSDAYS,X'03'                                                   
         BE    SAVR8                                                            
         SR    R1,R1               2-6=MIXED DAYS                               
         SR    RF,RF               9-15=SINGLE DAY                              
         ZIC   R0,NBRSDAYS                                                      
         SRDL  R0,9                                                             
         SLDL  R0,1                                                             
         LTR   R0,R0                                                            
         BNZ   *+8                                                              
         BCT   RF,*-10                                                          
         LPR   RF,RF                                                            
         LTR   R1,R1                                                            
         BNZ   *+8                                                              
         LA    RF,7(RF)                                                         
         STC   RF,RTDAYCD                                                       
*                                                                               
SAVR8    SR    RE,RE               TIMES                                        
         ICM   RE,3,NBRSTIMS                                                    
         AHI   RE,-600             6A-MIDNIGHT,MIDIGHT-6A                       
         BNM   *+8                                                              
         AHI   RE,2400                                                          
         STCM  RE,3,RTTIMES                                                     
         ICM   RE,3,NBRSTIMS+2                                                  
         AHI   RE,-600                                                          
         BNM   *+8                                                              
         AHI   RE,2400                                                          
         STCM  RE,3,RTTIMES+2                                                   
*                                                                               
**SAVR10   MVC   RTKPO,BWDKELPO      RECORD KEY VALUES                          
SAVR10   MVC   RTKDAYS,NBRSDAYS                                                 
         MVC   RTKTIMES,NBRSTIMS                                                
         LA    R8,RTABL(R8)                                                     
         C     R8,LARECTBX                                                      
         BL    *+6                                                              
         DC    H'0'                                                             
         ST    R8,LANXTREC         SAVE A(NEXT ENTRY)                           
*                                                                               
SAVRX    B     EXIT                                                             
         DROP  R3                                                               
         SPACE 2                                                                
SORTTAB  DC    X'02',AL2(0600),AL2(2000),AL1(4)  SA    6A-8P                    
         DC    X'01',AL2(0600),AL2(1900),AL1(4)  SU    6A-7P                    
         DC    X'03',AL2(0600),AL2(2000),AL1(4)  SA-SU 6A-8P                    
         DC    X'01',AL2(1900),AL2(2000),AL1(2)  SU    7P-8P                    
         DC    X'00',AL2(0600),AL2(2000),AL1(1)  ALL   6A-8P                    
         DC    X'00',AL2(2000),AL2(2300),AL1(2)  ALL   8P-11P                   
         DC    X'00',AL2(2300),AL2(2401),AL1(3)  ALL   11P-MIDNIGHT             
         DC    X'00',AL2(0000),AL2(0600),AL1(3)  ALL   MIDNIGHT-6A              
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PERFORM BUY TRANSFER                                               
* INPUT  : R3 = A(BUY REVISION RECORD'S SAVED DATA ELEMENT)                     
* OUTPUT : FVMSGNO NE FVFOK IF ERROR                                            
***********************************************************************         
         SPACE 1                                                                
TRANSFER NTR1                                                                   
         USING NBRSELD,R3                                                       
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         XC    LSVBDAT2(L'LSVBDAT2*2),LSVBDAT2                                  
         NI    LIND,255-LSEPLINE-LSEPDLY                                        
***                                                                             
         L     RF,AEXTRAWK                                                      
         USING EXTRAWKD,RF                                                      
         XC    LDTRDAT3,LDTRDAT3                                                
         DROP  RF                                                               
***                                                                             
*                                                                               
         LA    R4,1                                                             
         OC    NBRSEDT2,NBRSEDT2                                                
         BZ    TRAN2                                                            
         LA    R4,2                                                             
         GOTO1 VDATCON,APPARM,(3,NBRSEDT2),(2,LSVBDAT2)                         
         OC    NBRSEDT3,NBRSEDT3                                                
         BZ    TRAN1                                                            
         LA    R4,3                                                             
         GOTO1 (RF),(R1),(3,NBRSEDT3),(2,LSVBDAT3)                              
*                                                                               
TRAN1    CLI   CLTBWPRO+9,C'Y'     TEST SEPARATE LINES FOR EFFECTIVE            
         BNE   *+8                 COSTS                                        
         OI    LIND,LSEPLINE       YES-INDICATE SEPARATE LINES                  
*                                                                               
TRAN2    TM    CMPOPTS,CAMODLY     TEST DAILY SKED                              
         BZ    TRAN4                                                            
         TM    CLTIND2,CLTISDLY    AND SEPARATE LINES REQUESTED                 
         BZ    TRAN4                                                            
         OI    LIND,LSEPDLY        YES-                                         
         MVC   LSVDAYS,NBRSDAYS    SAVE DAYS ROTATION                           
         ZIC   R4,CMPNWKS          GO ROUND ONCE FOR EACH DAY                   
*                                                                               
TRAN4    BAS   RE,GETELEMS         SCAN RECORD FOR ELEMENTS                     
         L     R1,AIOAREA2                                                      
         USING NBRKEY,R1                                                        
         OC    NBRKKBUY,NBRKKBUY   ALREADY BOUGHT?                              
         BNZ   TRAN8               YES                                          
         DROP  R1                                                               
*                                                                               
         OC    LASPWEL,LASPWEL     NO, TEST FOR ANY SCHEDULE                    
         BZ    TRAN24                  NONE, JUST DELETE THE RECORD             
*                                                                               
TRAN8    TM    LIND,LSEPDLY                                                     
         BZ    TRAN8C               - NOPE SEPARATE DAILY, SKIP PLZ             
         MVI   LDTRDAY,1           ONLY SEPARATE DAILY NEEDS THIS               
         BRAS  RE,SAVRDATE         SAVES BDSTART FROM BYLN IN LSPTDATE          
**                                 LSPTDATE IS NOW SET!!                        
TRAN8A   BRAS  RE,FIXFXDLY         A FIX FOR FIX DAILY                          
**                                 LDTRDAT2 IS NOW SET!!                        
*****  LDTRDAT2 IS THE CURRENT DATE IN THIS ITERATION (REPEATED MAX14X)         
**         IT IS USED IN GETOVRD TO SET UP COSTABLE FOR THIS DATE               
TRAN8C   BAS   RE,GETOVRDE         GET COST OVERRIDES                           
*                                                                               
         TM    LIND,LSEPDLY        TEST SEPARATE DAILY LINES                    
         BNZ   TRAN8E                                                           
*                                                                               
         L     R1,AEXTRAWK                                                      
         USING EXTRAWKD,R1                                                      
         XR    R0,R0                                                            
         IC    R0,COSTNUMB         ITERATE FOR EACH COST OVERRIDE               
         AR    R4,R0                                                            
         DROP  R1                                                               
*                                                                               
         B     TRAN10              YES-START WITH FIRST DAY IN SCHEDULE         
*        TM    LIND,LSEPDLY        TEST SEPARATE DAILY LINES                    
*        BZ    TRAN10                                                           
*RAN8C   MVI   LDTRDAY,1           YES-START WITH FIRST DAY IN SCHEDULE         
*                                                                               
*****  THIS IS THE SAME CODE AS TRAN10 BELOW                                    
TRAN8E   MVI   LBTRIND,0           YES-START WITH PRIMARY COST                  
         NI    MISCFLG1,X'FF'-MF1UP2OV   NOT UPTO OVERRIDES YET                 
         L     R1,AEXTRAWK                                                      
         USING EXTRAWKD,R1                                                      
         MVI   CURRCOVR,0          CURRENT COST OVERRIDE                        
         DROP  R1                                                               
*****                                                                           
*                                                                               
TRAN9    GOTO1 AFIXDLY             FIX RECORD FOR ONE DAY                       
         BNE   TRAN16                                                           
*                                                                               
TRAN9C   L     R1,AEXTRAWK                                                      
         USING EXTRAWKD,R1                                                      
*                                                                               
****  LSPTDATE IS THE DATE FOR THE BUYLINE SPOT                                 
         CLC   LSPTDATE,LDTRDATE                                                
         BE    TRAN11              WILL GO TO FIXCOST THEN REXFR                
*                                                                               
****  LDTRDAT3 IS THE DATE FOR THE LAST TIME THROUGH THIS CODE                  
****    THIS IS FOR SEPARATE DAILY LINES ONLY!!   MHC  03/23/04                 
         CLC   LDTRDAT3,LDTRDATE   SAME DATE AS BUY?                            
         BNE   TRAN12AA             - NOPE, NEW BUY (IF NO EFFECT DATE)         
         DROP  R1                                                               
         L     R1,AIOAREA2                                                      
         USING NBRKEY,R1                                                        
         OC    NBRKKBUY,NBRKKBUY                                                
         BNZ   TRAN11                                                           
         DROP  R1                                                               
         CLI   LBTRIND,0                                                        
         BE    TRAN12                                                           
         B     TRAN11                                                           
*                                                                               
TRAN10   MVI   LBTRIND,0           YES-START WITH PRIMARY COST                  
         NI    MISCFLG1,X'FF'-MF1UP2OV   NOT UPTO OVERRIDES YET                 
         L     R1,AEXTRAWK                                                      
         USING EXTRAWKD,R1                                                      
         MVI   CURRCOVR,0          CURRENT COST OVERRIDE                        
         DROP  R1                                                               
*                                                                               
TRAN10A  TM    LIND,LSEPLINE       TEST EFFECTIVE COSTS TO SEP LINES            
         BNZ   TRAN11                                                           
         L     R1,AIOAREA2                                                      
         USING NBRKEY,R1                                                        
         OC    NBRKKBUY,NBRKKBUY   TEST EFFECTIVE COSTS TO SEP LINES            
         BNZ   TRAN11                                                           
         CLI   LBTRIND,0                                                        
         BE    TRAN12                                                           
*                                                                               
TRAN11   GOTO1 AFIXCOST            FIX NBR RECORD FOR SINGLE COST               
         BNE   TRAN16                                                           
*                                                                               
TRAN12   L     R1,AIOAREA2         PREVIOUSLY BOUGHT?                           
         USING NBRKEY,R1           TEST FOR PREVIOUS BUY TRANSFER               
         OC    NBRKKBUY,NBRKKBUY                                                
         BNZ   TRAN12A                                                          
TRAN12AA CLI   LBTRIND,0                                                        
         BE    TRAN14                                                           
         DROP  R1                                                               
*                                                                               
TRAN12A  CLI   LBTRIND,0           PAST PRIMARY COST?                           
         BE    TRAN13              NO                                           
         TM    LIND,LSEPLINE       YES, AND SEPARATE EFFECTIVE COSTS?           
         BNZ   TRAN14                    YES                                    
*                                                                               
TRAN13   BAS   RE,REXFR            RE-TRANSFER                                  
         BNE   TRANX                                                            
         OI    LIND,LREXFR         INDICATE AT LEAST ONE RE-TRANSFER            
         B     TRAN16                                                           
*                                                                               
TRAN14   GOTO1 ABLDWKS,LBADD       BUILD WEEKS TABLE                            
         CLI   LNSPWKS,0           TEST NUMBER OF WEEKS WITH SPOTS              
         BE    TRAN16              NONE                                         
****  NEED TO WIPE OUT LASKED IF IT'S ADDBUY!!                                  
         XC    LASKED,LASKED       P0.00 COST BUG!!                             
****  NEED TO WIPE OUT LASKED IF IT'S ADDBUY!!                                  
         GOTO1 =A(ADDBUY),RR=APRELO    ADD THE BUY LINE(S)                      
         BNE   TRANX                                                            
**                                 ..UNTIL NEW DATE IF THERE ARE SPOTS          
         L     RF,AEXTRAWK                                                      
         USING EXTRAWKD,RF                                                      
         MVC   LDTRDAT3,LDTRDATE   UPDATE LDTRDAT3 FOR NEXT COMPARE             
         DROP  RF                                                               
*                                                                               
TRAN16   BRAS  RE,CHKAUTH                                                       
         TM    LIND,LSEPDLY                                                     
         BZ    TRAN16X             NOT SEPARATE DAILY, GET OUT                  
         L     R1,AEXTRAWK                                                      
         USING EXTRAWKD,R1                                                      
         CLC   LDTRDATE,LSPTDATE   BUY SPOT DATE?                               
         BNE   TRAN16X              - NOPE, AUGMENT LDTRDAY                     
*                                                                               
         CLC   COSTNUMB,CURRCOVR   ARE WE UP TO THE LIMIT (COSTNUMB)            
         BE    TRAN16X              - YUP, DONE WITH THIS DAY                   
         DROP  R1                                                               
*                                                                               
         TM    MISCFLG1,MF1UP2OV   ARE WE UP TO COST OVERRIDES?                 
         BNZ   TRAN16C             YES                                          
*                                                                               
         CLI   LBTRIND,0           NEXT EFFECTIVE COST                          
         BNE   TRAN16A                                                          
         OC    NBRSEDT2,NBRSEDT2                                                
         BZ    TRAN16B                                                          
         MVI   LBTRIND,BTRIEC2                                                  
         B     TRAN9C                                                           
*                                                                               
TRAN16A  OC    NBRSEDT3,NBRSEDT3                                                
         BZ    TRAN16B                                                          
         MVI   LBTRIND,BTRIEC3                                                  
         B     TRAN9                                                            
*                                                                               
TRAN16B  OI    MISCFLG1,MF1UP2OV                                                
         MVI   LBTRIND,0           NO MORE EFFECTIVE                            
*                                                                               
TRAN16C  L     R1,AEXTRAWK                                                      
         USING EXTRAWKD,R1                                                      
         XR    R0,R0                                                            
         IC    R0,CURRCOVR         CURRENT COST OVERRIDE POINTER                
         AHI   R0,1                                                             
         STC   R0,CURRCOVR                                                      
         DROP  R1                                                               
         B     TRAN9C                                                           
*                                                                               
TRAN16X  BCT   R4,*+8                                                           
         B     TRAN18                                                           
*                                                                               
         TM    LIND,LSEPDLY        TEST SEPARATE DAILY LINES                    
         BZ    TRAN17                                                           
         ZIC   R1,LDTRDAY          YES-ADVANCE TO NEXT DAY                      
         LA    R1,1(R1)                                                         
         STC   R1,LDTRDAY                                                       
         B     TRAN8A                                                           
*                                                                               
TRAN17   TM    MISCFLG1,MF1UP2OV   ARE WE UPTO COST OVERRIDES?                  
         BNZ   TRAN17C             YES                                          
*                                                                               
         CLI   LBTRIND,0           NEXT EFFECTIVE COST                          
         BNE   TRAN17A                                                          
         OC    NBRSEDT2,NBRSEDT2                                                
         BZ    TRAN17B                                                          
         MVI   LBTRIND,BTRIEC2                                                  
         B     TRAN10A                                                          
*                                                                               
TRAN17A  OC    NBRSEDT3,NBRSEDT3                                                
         BZ    TRAN17B                                                          
         MVI   LBTRIND,BTRIEC3                                                  
         B     TRAN10A                                                          
*                                                                               
TRAN17B  OI    MISCFLG1,MF1UP2OV                                                
         MVI   LBTRIND,0           NO MORE EFFECTIVE                            
*                                                                               
TRAN17C  L     R1,AEXTRAWK                                                      
         USING EXTRAWKD,R1                                                      
         XR    R0,R0                                                            
         IC    R0,CURRCOVR         CURRENT COST OVERRIDE POINTER                
         AHI   R0,1                                                             
         STC   R0,CURRCOVR                                                      
         DROP  R1                                                               
         B     TRAN11                                                           
*                                                                               
TRAN18   DS    0H                                                               
*                                                                               
*&&DO                                                                           
TRAN18   TM    LIND,LSEPLINE+LSEPDLY  TEST SEPARATE BUYLINES                    
         BZ    TRAN22                                                           
         ICM   R1,15,LASPWEL    YES-MOVE ORIGINAL SPOTS PER WEEK                
         BZ    TRAN20               ELEMENT BACK INTO THE RECORD                
         ZIC   RE,1(R1)                                                         
         BCTR  RE,0                                                             
         MVC   0(0,R1),LSVSPWEL                                                 
         EX    RE,*-6                                                           
*                                                                               
TRAN20   TM    LIND,LSEPDLY        TEST SEPARATE DAILY TRANSFER                 
         BZ    *+14                                                             
         MVC   NBRSDAYS,LSVDAYS    YES-RESTORE ORIGINAL DAYS ROTATION           
         B     TRAN22                                                           
         MVC   NBRSCST1,LSVCOST1   RESTORE EFFECTIVE COSTS AND DATES            
         MVC   NBRSCST2,LSVCOST2                                                
         MVC   NBRSCST3,LSVCOST3                                                
         MVC   NBRSEDT2,LSVDAT2                                                 
         MVC   NBRSEDT3,LSVDAT3                                                 
*&&                                                                             
TRAN24   XC    IOKEY,IOKEY                                                      
         LA    R3,IOKEY                                                         
         USING NBRKEY,R3                                                        
         MVC   IOKEY(13),LDETKEY                                                
         GOTO1 AIO,DIRRDUP+IO2                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 AIO,FILGETU2                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,AIOAREA2                                                      
         OC    NBRKKBUY,NBRKKBUY   OFF A BUY RECORD?                            
         BNZ   TRAN50              YES, CAN DELETE REVISION RECORD              
*                                                                               
         XR    R0,R0                                                            
         LA    R3,NBRFSTEL-NBRKEY(R3)                                           
TRAN30   CLI   0(R3),0             ANY SPOTS SCHEDULED?                         
         BE    TRANX               NONE, DON'T DELETE                           
         CLI   0(R3),NBRSPELQ                                                   
         BE    TRAN35                                                           
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     TRAN30                                                           
*                                                                               
         USING NBRSPELD,R3                                                      
TRAN35   CLI   NBRSPLEN,NBRSPSPW-NBRSPELD                                       
         BNH   TRANX                                                            
         XR    RE,RE                                                            
         IC    RE,NBRSPLEN                                                      
         AHI   RE,-(NBRSPSPW-NBRSPELD+1)                                        
         EX    RE,*+8                                                           
         B     *+10                                                             
         OC    NBRSPSPW(0),NBRSPSPW                                             
         BZ    TRANX                                                            
*                                                                               
TRAN50   L     R3,AIOAREA2                                                      
         USING NBRKEY,R3                                                        
         OI    NBRCNTL,X'80'       DELETE THE RECORD                            
         GOTO1 AIO,FILPUT2                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R3,IOKEY            DELETE THE KEY                               
         OI    NBRKCNTL,X'80'                                                   
         GOTO1 AIO,DIRWRT+IO2                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
TRANX    CLC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
*&&DO                                                                           
***********************************************************************         
* ROUTINE TO TEST RECORD SHOULD BE LOCKED OUT FROM TRANSFER           *         
* OUTPUT : BWDINDS SET TO BWDITRLK FOR LOCK-OUT                       *         
***********************************************************************         
         SPACE 1                                                                
TESTLOCK LR    R0,RE                                                            
         CLI   CLTBWPRO+9,C'Y'     TEST EFFECTIVE COSTS TO SEP BUYLINES         
         BNE   LOCK2                                                            
*                                                                               
**       CLC   BWDEFDT2,BWDTRED2   YES-TEST CHANGE IN EFFECTIVE DATES           
**       BNE   LOCK9                   YES-LOCK OUT                             
**       CLC   BWDEFDT3,BWDTRED3                                                
**       BNE   LOCK9                   YES-LOCK OUT                             
*                                                                               
         OC    BWDEFDT2,BWDEFDT2   YES-LOCK OUT ONLY IF EFFECTIVE DATES         
         BNZ   *+14                    WERE REMOVED SINCE LAST TRANSFER         
         OC    BWDTRED2,BWDTRED2                                                
         BNZ   LOCK9                                                            
         OC    BWDEFDT3,BWDEFDT3                                                
         BNZ   LOCK4                                                            
         OC    BWDTRED3,BWDTRED3                                                
         BNZ   LOCK9                                                            
         B     LOCK4                                                            
*                                                                               
LOCK2    OC    LABTREL2,LABTREL2   NO-TEST SEPARATE BUYLINES BEFORE             
         BNZ   LOCK9                  YES-LOCK OUT                              
         OC    LABTREL3,LABTREL3                                                
         BNZ   LOCK9                                                            
*                                                                               
LOCK4    TM    LIND,LSEPDLY        DAILY SCHED TO SEPARATE LINES?               
         BZ    LOCK6                                                            
         OC    LABTREL,LABTREL     YES, IF REGULAR TRANSFER BEFORE              
         BZ    LOCKX                                                            
         NI    LIND,255-LSEPDLY         THEN REGULAR TRANSFER NOW               
         B     LOCKX                                                            
*                                                                               
LOCK6    OC    LADTREL,LADTREL     NO, IF DAILY TRANSFER BEFORE                 
         BZ    LOCKX                                                            
         OI    LIND,LSEPDLY            THEN DAILY TRANSFER NOW                  
         B     LOCKX                                                            
*                                                                               
LOCK9    OI    BWDINDS,BWDITRLK    LOCK OUT                                     
*                                                                               
LOCKX    LR    RE,R0                                                            
         BR    RE                                                               
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* GETS THE OVERRIDES IN THE BUY REVISION RECORD                                 
*                                                                               
* ON ENTRY:    AIOAREA2            A(BUY REVISION RECORD)                       
*                                                                               
* ON EXIT:     COSTABLE            A(COST OVERRIDE TABLE)                       
*              COSTNUMB            NUMBER OF COST OVERRIDES                     
***********************************************************************         
GETOVRDE NTR1                                                                   
         L     R4,AEXTRAWK                                                      
         USING EXTRAWKD,R4                                                      
*                                                                               
         MVI   COSTNUMB,0          # OF COST OVERRIDES                          
         XC    COSTABLE,COSTABLE   CLEAR OUR TABLE                              
         LA    RE,COSTABLE                                                      
         L     R2,AIOAREA2                                                      
         USING NBRKEY,R2                                                        
         LA    R2,NBRFSTEL                                                      
GETOV10  CLI   0(R2),0             DONE WITH THE BUY REVISION RECORD?           
         BE    GETOVX              YES, NO MORE OVERRIDE ELEMENTS               
         CLI   0(R2),NBRCOELQ                                                   
         BE    GETOV20                                                          
GETOV15  XR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     GETOV10                                                          
*                                                                               
         USING NBRCOELD,R2                                                      
GETOV20  TM    LIND,LSEPDLY        WE DOING SEPARATE DAILY?                     
         BZ    GETOV23              - NOPE, NO NEED FOR LDTRDAT2                
         CLC   LDTRDAT2,NBRCODAT   IS IT THE SPOT'S DATE?                       
         BNE   GETOV15              - NOPE, DON'T NEED THIS OVERRIDE            
*                                                                               
GETOV23  LA    R1,COSTABLE                                                      
         CLI   COSTNUMB,0                                                       
         BE    GETOV30                                                          
         XR    RF,RF                                                            
         IC    RF,COSTNUMB                                                      
GETOV25  CLC   NBRCOCST,0(R1)      THIS COST ALREADY IN TABLE?                  
         BE    GETOV15             YES                                          
         LA    R1,4(R1)            BUMP TO THE NEXT COST                        
         BCT   RF,GETOV25          CHECK ALL COSTS IN TBL FOR THIS COST         
*                                                                               
GETOV30  CLI   COSTNUMB,MAXCOSTS                                                
         BNH   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   0(L'NBRCOCST,R1),NBRCOCST                                        
         XR    RF,RF                                                            
         IC    RF,COSTNUMB                                                      
         AHI   RF,1                                                             
         STC   RF,COSTNUMB                                                      
         B     GETOV15                                                          
         DROP  R2                                                               
*                                                                               
GETOVX   XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* LOCATE BWS RECORD ELEMENTS                                                    
* INPUT  : R3 = A(BUY REVISION RECORD'S SAVED DATA ELEMENT)                     
* OUTPUT : ADDRESSES ARE SET                                                    
*          RECORD'S BUY ID IS SET IF THERE'S AN ID ELEMENT                      
***********************************************************************         
GETELEMS NTR1                                                                   
         SR    R0,R0               SCAN RECORD FOR ELEMENTS                     
         XC    LASPWEL,LASPWEL                                                  
         XC    LADEMEL,LADEMEL                                                  
         XC    LAUPGEL,LAUPGEL                                                  
         XC    LAODTEL,LAODTEL                                                  
         XC    LAIDEL,LAIDEL                                                    
*        XC    LACS2EL,LACS2EL                                                  
         XC    LSVCS2,LSVCS2                                                    
         XC    LADTREL,LADTREL                                                  
         XC    LACMTEL,LACMTEL                                                  
*                                                                               
         USING NBRSELD,R3                                                       
         LR    RF,R3                                                            
         B     GETE7                                                            
*                                                                               
GETE1    CLI   0(RF),0             END OF RECORD?                               
         BE    GETE8                                                            
*                                                                               
         LA    R1,LASPWEL          SAVE A(SPOTS/WEEK ELEMENT)                   
         CLI   0(RF),NBRSPELQ                                                   
         BE    GETE2                                                            
         LA    R1,LADEMEL          SAVE A(SPOTS/WEEK ELEMENT)                   
         CLI   0(RF),NBRDMELQ                                                   
         BE    GETE2                                                            
         LA    R1,LAUPGEL          SAVE A(UPGRADE ELEMENT)                      
         CLI   0(RF),NBRUPELQ                                                   
         BE    GETE2                                                            
         LA    R1,LAODTEL          SAVE A(OVERRIDE ELEMENT)                     
         CLI   0(RF),NBRODELQ                                                   
         BE    GETE2                                                            
         LA    R1,LAIDEL           SAVE A(ID ELEMENT)                           
         CLI   0(RF),NBRIDELQ                                                   
         BE    GETE2                                                            
*        LA    R1,LACS2EL          SAVE A(COS2 ELEMENT)                         
         CLI   0(RF),NBRC2ELQ                                                   
         BNE   GETE1E                                                           
         MVC   LSVCS2,2(RF)        SAVE OFF THE CS2                             
         B     GETE7                                                            
GETE1E   CLI   0(RF),NBRCMELQ                                                   
         BNE   GETE3                                                            
         OC    LACMTEL,LACMTEL                                                  
         BNZ   GETE7                                                            
         ST    RF,LACMTEL          SAVE A(COMMENT ELEMENT)                      
         B     GETE7                                                            
*                                                                               
GETE2    ST    RF,0(R1)                                                         
         B     GETE7                                                            
*                                                                               
GETE3    DS    0H                                                               
*&&DO                                                                           
GETE3    CLI   0(RF),SPIELCDQ      TEST SPILL DEMO ELEMENT                      
         BNE   *+12                                                             
         OI    LIND,LSPILLEL       YES                                          
         B     GETE7                                                            
         CLI   0(RF),DTRELCDQ                                                   
         BNE   GETE7                                                            
         OC    LADTREL,LADTREL                                                  
         BNZ   GETE7                                                            
         ST    RF,LADTREL          SAVE A(FIRST DAILY SCHEDULE ELEMENT)         
*&&                                                                             
GETE7    IC    R0,1(RF)                                                         
         AR    RF,R0                                                            
         B     GETE1                                                            
*                                                                               
GETE8    XC    LRECBYID,LRECBYID                                                
         ICM   RF,15,LAIDEL        TEST ID ELEMENT                              
         BZ    GETEX                                                            
         MVC   LRECBYID,BWID-BWIDEL(RF)  YES-SAVE RECORD'S BUY ID               
*                                                                               
GETEX    B     EXIT                                                             
         EJECT                                                                  
         DROP  R6                                                               
***********************************************************************         
* ROUTINE TO RE-TRANSFER TO BUY PROGRAM                               *         
***********************************************************************         
         SPACE 1                                                                
REXFR    NTR1                                                                   
         L     R3,AIOAREA2                                                      
         USING NBRKEY,R3                                                        
         XC    LXFRTOT,LXFRTOT                                                  
         L     RE,AEXTRAWK                                                      
         AHI   RE,LSKEDS-EXTRAWKD                                               
         LR    R8,RE               R8 IS USED LATER                             
         LA    RF,LSKEDSL                                                       
         XCEFL ,                                                                
         SR    R0,R0                                                            
         SR    R6,R6                                                            
         LA    R3,NBRFSTEL                                                      
REX1     CLI   0(R3),0                                                          
         BE    REXX                                                             
         CLI   0(R3),NBRSELQ                                                    
         BE    REX1A                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     REX1                                                             
*                                                                               
         USING NBRSELD,R3                                                       
REX1A    CLI   NBRSBYLN,0          MISSING BUYLINE NUMBER?                      
         BNE   REX1A00                                                          
         L     RF,AIOAREA2                                                      
         USING NBRKEY,RF                                                        
         OC    NBRKKBUY,NBRKKBUY   YES, SHOULD WE HAVE A BUYLINE #?             
         BZ    REX1A00                  NO                                      
         CLI   NBRKKBUY,0               YES, 1ST BYTE OF KBUY IS 0?             
         BNE   *+14                                                             
         MVC   NBRSBYLN,NBRKKBUY+1           YES, BYLN IS IN 2ND BYTE           
         B     REX1A00                                                          
         MVC   NBRSBYLN,NBRKKBUY+2           NO,  BYLN IS IN 3RD BYTE           
         DROP  RF                                                               
*                                                                               
REX1A00  MVC   0(1,R8),NBRSBYLN                                                 
         SR    R0,R0                                                            
*&&DO                                                                           
         L     R4,LABTREL          BUILD ORIGINAL TRANSFER TOTAL SPOTS          
         USING BTREL,R4            PER WEEK                                     
         TM    LIND,LSEPLINE                                                    
         BZ    REX2                                                             
         LA    R4,BWDEL                                                         
*                                                                               
REX2     CLI   0(R4),0             TEST ANY MORE BUYLINES                       
         BE    REX6                                                             
         CLI   0(R4),BTRELCDQ                                                   
         BNE   REX5                                                             
         TM    LIND,LSEPLINE       YES-TEST SEP LINES FOR EFF COSTS             
         BZ    REX3                                                             
         MVC   APBYTE,BTRIND       YES-TEST CORRECT EFFECTIVE COST              
         NI    APBYTE,BTRIEC2+BTRIEC3                                           
         CLC   APBYTE,LBTRIND                                                   
         BNE   REX5                                                             
*                                                                               
REX3     MVC   0(1,R8),BTRLINE     BUY LINE NUMBER                              
         LA    R1,BTRSPW                                                        
         TM    BTRIND,BTRIDATE     TEST ELEMENT HAS DATE IN IT                  
         BO    *+8                                                              
         AHI   R1,-3               NO-OLD STYLE ELEMENT                         
         LA    RE,1                                                             
         ZIC   RF,BTRELLN                                                       
         AR    RF,R4                                                            
         BCTR  RF,0                                                             
         LA    R9,LXFRTOT                                                       
*                                                                               
REX4     IC    R0,0(R9)            SPOTS SO FAR THIS WEEK                       
         IC    R6,0(R1)            SPOTS THIS WEEK THIS BUYLINE                 
         AR    R0,R6               ADD TO TOTAL                                 
         STC   R0,0(R9)            AND STORE                                    
         LA    R9,1(R9)                                                         
         BXLE  R1,RE,REX4          DO FOR ALL WEEKS                             
         LA    R8,LSKEDENL(R8)     NEXT BUY LINE                                
*                                                                               
REX5     IC    R0,1(R4)            NEXT ELEMENT                                 
         AR    R4,R0                                                            
         B     REX2                                                             
*                                                                               
REX6     XC    LWKINDS,LWKINDS                                                  
         CLI   CLTBWPRO,C'N'       TEST FOR DISALLOW RETRANSFER OF              
         BE    *+14                ALREADY TRANSFERRED WEEKS                    
         OC    LDTINDS,LDTINDS     OR SOME WEEKS ARE EXCLUDED                   
         BZ    REX10                                                            
         LA    R6,LWKINDS          YES -                                        
         LA    R9,LXFRTOT                                                       
         LA    RE,LDTINDS                                                       
         LA    R0,53                                                            
*                                                                               
REX7     TM    0(RE),LEXCLD        TEST EXCLUDE THIS WEEK                       
         BO    REX8                YES                                          
         CLI   0(R9),0             TEST THIS WEEK HAS TRANSFER SPOTS            
         BE    REX9                                                             
         CLI   CLTBWPRO,C'N'       YES-TEST DISALLOW TRANSFER                   
         BNE   REX9                                                             
*                                                                               
REX8     OI    0(R6),LFRZ          FREEZE THE WEEK                              
         OI    LFLAG,LFREEZE                                                    
*                                                                               
REX9     LA    R6,1(R6)                                                         
         LA    R9,1(R9)                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,REX7                                                          
*&&                                                                             
REX10    XC    LBUYTOT,LBUYTOT     FIND TOTAL SPOTS/WEEK ACCORDING              
*                                                                               
*****    GOTO1 =A(CHKDARLK),RR=APRELO    LOCKED BY DARE?                        
*****    BE    REXX                YES                                          
*                                                                               
         LA    R2,IOKEY            TO BUY RECORDS                               
         USING BUYRECD,R2                                                       
         XC    BUYKEY,BUYKEY       BUILD BUY KEY                                
         MVC   BUYKAM,BAGYMD       AGENCY-MEDIA                                 
         MVC   BUYKCLT,BCLT        CLIENT                                       
         MVC   BUYKPRD,BPRD        PRODUCT                                      
****     TM    LIND,LPOL           TEST POOL BUY                                
****     BZ    *+8                                                              
****     MVI   BUYKPRD,FF          YES - SET PRODUCT IN KEY TO POOL             
         MVC   BUYMSTA(2),BMKT     MARKET                                       
         OC    LOVRMKT,LOVRMKT     TEST MARKET OVERRIDE                         
         BZ    *+10                                                             
         MVC   BUYMSTA(2),LOVRMKT                                               
         L     R1,AIOAREA2                                                      
         USING NBRKEY,R1                                                        
         MVC   BUYMSTA+2(3),NBRKSTA  STATION                                    
         MVC   BUYKEST,BEST        ESTIMATE                                     
         MVC   BUYKBUY(3),NBRKKBUY   BUY DETAILS                                
         DROP  R1                                                               
         L     R8,AEXTRAWK         BUY LINE NUMBER                              
         AHI   R8,LSKEDS-EXTRAWKD                                               
         MVI   LCHGIND,0           (INITIALIZE CHANGE INDICATOR)                
*                                                                               
REX11    CLI   0(R8),0             TEST ANY MORE BUY LINES                      
         BE    REX32                                                            
         MVC   IOADDR,AIOAREA3                                                  
         XC    LWKINDS,LWKINDS                                                  
         GOTOR GETBUY,0            GET THE BUY RECORD                           
         BE    REX11A                                                           
*                                                                               
         LA    R2,BWSMSGH                                                       
         XC    8(L'BWSMSG,R2),8(R2)                                             
         MVC   8(L'MISSLINR,R2),MISSLINR                                        
         ZIC   R1,0(R8)                                                         
         CVD   R1,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  8+L'MISSLINR(3,R2),APDUB+6(2)                                    
         OI    6(R2),X'80'                                                      
         LA    R2,BWSRECH                                                       
         DC    H'0'                                                             
         DC    C'$ABEND'                                                        
*                                                                               
MISSLINR DC    C'CANNOT TRANSFER!  MISSING BUY LINE '                           
*                                                                               
REX11A   L     R2,AIOAREA3                                                      
*****                                                                           
         MVC   LBUYKSAV,IOKEY                                                   
         GOTO1 =A(CHKDARLK),APPARM,(RC),RR=APRELO    LOCKED BY DARE?            
         BE    REXX                YES                                          
         MVC   IOKEY,LBUYKSAV                                                   
*****                                                                           
         LA    R1,BDELEM                                                        
         SR    R0,R0               FIND BWS TRANSFER ELEMENT                    
*                                                                               
REX12    CLI   0(R1),0                                                          
         BE    REX13                                                            
         CLI   0(R1),BWSCODEQ                                                   
         BE    *+14                                                             
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     REX12                                                            
*                                                                               
         CLI   1(R1),7             TEST TRANSFER DATE IN ELEMENT                
         BNH   REX13                                                            
         MVC   APFULL(3),BWSDATE-BWSELEM(R1) YES-SAVE IT                        
         B     REX13                                                            
*&&DO                                                                           
         LA    R4,BWDEL            LOOK FOR XFER ELEMENT IN BWS RECORD          
REX12A   CLI   0(R4),0                                                          
         BE    REX92                                                            
         CLI   0(R4),BTRELCDQ                                                   
         BNE   REX12B                                                           
         USING BTREL,R4                                                         
         CLC   BTRLINE,0(R8)                                                    
         BNE   REX12C                                                           
         TM    BTRIND,BTRIDATE     TEST ELEMENT HAS TRANSFER DATE               
         BZ    REX92               NO-LOCKOUT                                   
         CLC   BTRDATE,APFULL      YES-THE DATES MUST MATCH                     
         BL    REX92               ELSE LOCKOUT                                 
         B     REX13                                                            
*                                                                               
REX12B   CLI   0(R4),DTRELCDQ      DAILY TRANSFER ELEMENT                       
         BNE   REX12C                                                           
         USING DTREL,R4                                                         
         CLC   DTRLINE,0(R8)                                                    
         BNE   REX12C                                                           
         CLI   DTRELLN,6           TEST ELEMENT HAS TRANSFER DATE               
         BNH   REX92               NO-LOCKOUT                                   
         CLC   DTRDATE,APFULL      YES-THE DATE MUST MATCH                      
         BNE   REX92               ELSE LOCKOUT                                 
         B     REX13                                                            
*                                                                               
REX12C   IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     REX12A                                                           
*&&                                                                             
REX13    MVC   LMASPRD,BDMASPRD    SET MASTER PRODUCT FROM OLD ONE              
         BAS   RE,SETPRD           IT MIGHT CHANGE                              
*                                                                               
         GOTO1 ABUYCHG             DETERMINE CHANGES SINCE LAST XFR             
         SR    R0,R0                                                            
         LA    R4,BDELEM           SCAN BUY ELEMENTS                            
         USING REGELEM,R4                                                       
*                                                                               
REX14    CLI   0(R4),0             TEST END OF BUY RECORD                       
         BE    REX30                                                            
         CLI   0(R4),6             NON-POOL BUY ELEMENT                         
         BE    REX16                                                            
         CLI   0(R4),11            POOL BUY ELEMENT                             
         BE    REX15                                                            
         CLI   0(R4),7             NON-POOL OTO ELEMENT                         
         BE    REX16                                                            
         CLI   0(R4),12            POOL OTO ELEMENT                             
         BE    REX15                                                            
         CLI   0(R4),X'10'         TEST FOR AFFIDAVIT ELEMENT                   
         BNE   REX28                                                            
         OI    LFLAG,LAFFDVT       YES - FLAG                                   
         OI    1(R8),LAFD                                                       
         B     REX28                                                            
*                                                                               
REX15    TM    RSTATUS,X'20'       DO WE HAVE A COST OVERRIDE?                  
         BZ    REX16                - NOPE                                      
         LTR   R1,R3               SAVE THE REGISTER OF R3                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
REX15A   CLI   0(R3),0             NO MORE ELEMENTS?                            
         BE    REX15DEL             - NOPE, UH OH                               
         CLI   0(R3),X'31'         WE HAVE A POOL COST OVERRIDE?                
         BE    REX15CHK             - YUP, NOW WE CHECK IT OUT                  
*                                                                               
REX15BMP XR    R0,R0                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0               BUMP PLZ                                     
         B     REX15A                                                           
*                                                                               
         USING NBRCOELD,R3                                                      
REX15CHK CLC   NBRCODAT,RDATE      ARE THE DATES THE SAME?                      
         BNE   REX15BMP             - NOPE, BUMP THE ELEMENT                    
         LR    R3,R1               RESTORE R3                                   
         B     REX16                - FOUND IT, WE'RE OK                        
*                                                                               
REX15DEL LR    R3,R1               RESTORE R3                                   
         MVI   0(R4),X'FF'         DELETE THIS POOL BUY ELEMENT                 
         MVI   APELEM,X'FF'                                                     
         GOTO1 ADELELS,BUYREC                                                   
         OI    LCHGIND,LEFFCOST    TURN ON THIS FLAG                            
         B     REX14               R4 ALREADY POINTING TO NEXT ELEMENT          
*                                  ...SINCE THIS ONE IS DELETED ALREADY         
         DROP  R3                                                               
         USING NBRSELD,R3                                                       
*                                                                               
REX16    OC    RPAY,RPAY           TEST PAID                                    
         BZ    REX16A                                                           
         OI    1(R8),LPAY          YES                                          
*                                                                               
****  WE DON'T USE LEFFCOST FOR THIS ANYMORE!!!      MHC                        
*        TM    LCHGIND,LSTDATE+LCST+LEFFCOST+LNEWPRD                            
         TM    LCHGIND,LSTDATE+LCST+LEFFCOST+LNEWPRD                            
         BZ    REX16A                                                           
         OI    NBRSINDS,NBRSITLK   YES, LOCK THIS SCHEDULE                      
         OI    1(R8),LDONTCHG      DON'T CHANGE THE BUY                         
         B     REX30                                                            
*                                                                               
REX16A   TM    CMPOPTS,CAMODLY     TEST DAILY SKED                              
         BZ    REX16A10                                                         
         LR    RF,R5                                                            
         AHI   RF,CMPDATSP-TWAD                                                 
         CLC   RDATE,0(RF)         YES-COMPARE DATE TO FIRST DAY                
         BL    REX28               LOW-IGNORE                                   
         B     REX16A20                                                         
REX16A10 CLC   RDATE,CMPSTMNP      COMPARE BUY DATE TO START MONDAY             
         BL    REX28               LOW - IGNORE IT                              
*                                                                               
REX16A20 CLI   LBTRIND,0                                                        
         BNE   REX16B                                                           
         TM    RSTATUS-REGELEM(R4),X'20'   RATE OVERRIDE?                       
         BNZ   REX28               YES, WE WANT THE SPOTS @ BUY COST            
         B     REX17                                                            
*                                                                               
REX16B   TM    RSTATUS-REGELEM(R4),X'20'   RATE OVERRIDE?                       
         BZ    REX28                  NO, WE WANT SPOTS WITH OVERRIDES          
         CLI   LBTRIND,BTRIEC2     2ND COST?                                    
         BNE   REX16C              NO                                           
         CLC   RPCOST,LSVCOST2     YES, A MATCH?                                
         BE    REX17                    YES                                     
         B     REX28                                                            
*                                                                               
REX16C   CLC   RPCOST,LSVCOST3     3RD COST A MATCH?                            
         BNE   REX28               NO                                           
*                                                                               
REX17    LR    RE,R5               FIND WHICH WEEK THIS BUY DATE IS IN          
         AHI   RE,CMPDATSP-TWAD    FIND WHICH WEEK THIS BUY DATE IS IN          
         LA    RF,2(R8)            RF = A(SPOTS PER WEEK FIELD)                 
         LA    R6,LWKINDS                                                       
         LA    R9,LBUYTOT                                                       
         ZIC   R0,CMPNWKS                                                       
*                                                                               
REX18    CLC   RDATE,0(RE)         IS BUY DATE IN THIS WEEK                     
         BL    *+14                                                             
         CLC   RDATE,2(RE)   <===  HAS TO BE IN THE WEEK                        
         BNH   REX20                                                            
         LA    RE,4(RE)            NO - NEXT WEEK                               
         LA    RF,2(RF)                                                         
         LA    R6,1(R6)                                                         
         LA    R9,1(R9)                                                         
         BCT   R0,REX18                                                         
         B     REX28               BUY DATE GT PERIOD END - IGNORE IT           
*&&DO                                                                           
REX20    TM    RSTATUS,X'10'       DARE MAKEGOOD PENDING ?                      
         BNZ   REX26               YES, FREEZE THIS WEEK                        
*&&                                                                             
REX20    ZIC   R1,0(RF)            ACCUMULATE NUMBER OF SPOTS/WEEK              
         LA    RE,1                IN THIS BUYLINE                              
         CLI   RCODE,X'0B'                                                      
         BNL   *+8                                                              
         IC    RE,RNUM                                                          
*                                                                               
         TM    RSTATUS,X'80'       MINUS SPOT?                                  
         BZ    *+6                                                              
         LNR   RE,RE                                                            
*                                                                               
         AR    R1,RE                                                            
         STC   R1,0(RF)                                                         
         IC    R1,0(R9)            ACCUMULATE TOTAL SPOTS/WEEK OVER             
         AR    R1,RE               ALL BUYLINES                                 
         STC   R1,0(R9)                                                         
*                                                                               
REX21    TM    0(R6),LFRZ          TEST WEEK IS ALREADY FROZEN                  
         BO    REX28               YES - NEXT BUY ELEMENT                       
         OC    RPAY,RPAY           TEST SPOT IS PAID                            
         BNZ   REX26               YES - FREEZE THIS WEEK                       
         CLI   RCODE,6             FOR NON-POOL BUY --                          
         BNE   REX22                                                            
         OC    RBILL,RBILL         TEST FOR BILL DATE                           
         BNZ   REX26               YES - FREEZE THIS WEEK                       
         B     REX24                                                            
*                                  FOR POOL BUY --                              
REX22    CLI   RLEN,10             TEST FOR ALLOCATED PRODUCT(S)                
         BNH   REX24                                                            
********************  RPBILL NOT USED ANYMORE  ************************         
*        OC    RPBILL,RPBILL       YES - TEST FOR BILL DATE                     
*        BNZ   REX26                     YES - FREEZE THIS WEEK                 
********************  RPBILL NOT USED ANYMORE  ************************         
         CLI   RLEN,14             TEST FOR PIGGYBACK ALLOCATED                 
         BNH   REX24                                                            
********************  RPBILL NOT USED ANYMORE  ************************         
*        OC    RPBILL+L'RPALLOC(L'RPBILL),RPBILL+L'RPALLOC   BILL DATE?         
*        BNZ   REX26                     YES - FREEZE THIS WEEK                 
********************  RPBILL NOT USED ANYMORE  ************************         
*                                                                               
REX24    IC    R0,1(R4)            LOOK AT NEXT ELEMENT                         
         AR    R4,R0                                                            
         CLI   0(R4),X'10'         TEST NEXT ELEMENT IS AFFIDAVIT               
         BNE   REX14               NO - WEEK IS OK                              
*                                                                               
REX26    OI    0(R6),LFRZ          FREEZE THIS WEEK                             
         OI    LFLAG,LFREEZE                                                    
*                                                                               
REX28    IC    R0,1(R4)            NEXT ELEMENT                                 
         AR    R4,R0                                                            
         B     REX14                                                            
*                                                                               
REX30    LA    R8,LSKEDENL(R8)     NEXT BUY LINE                                
         LA    R2,IOKEY                                                         
         B     REX11               READ NEXT BUY RECORD                         
*                                                                               
REX32    DS    0H                                                               
*&&DO                                                                           
REX32    LA    RE,LXFRTOT          COMPARE SPOTS/WEEK BETWEEN                   
         LA    RF,LBUYTOT          LAST BUY TRANSFER AND WHAT THE               
         LA    R6,LWKINDS          BUY RECORDS NOW SAY                          
         LA    R0,53                                                            
*                                                                               
REX34    CLC   0(1,RE),0(RF)                                                    
         BE    *+12                                                             
         OI    0(R6),LFRZ          NOT EQUAL - FREEZE THIS WEEK                 
         OI    LFLAG,LFREEZE                                                    
         LA    R6,1(R6)                                                         
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,REX34                                                         
*&&                                                                             
         ICM   R4,15,LASPWEL       MOVE CURRENT SKED TO SKED TABLE              
         BZ    REX36                                                            
         USING NBRSPELD,R4                                                      
         LA    R1,NBRSPSPW                                                      
         ZIC   RF,1(R4)                                                         
         AR    RF,R4                                                            
         ZIC   RE,CMPNWKS                                                       
         LA    RE,NBRSPSPW(RE)                                                  
         CR    RE,RF               TEST ELEMENT HAS MORE WEEKS THAN             
         BNL   *+6                 NUMBER OF WEEKS IN CAMPAIGN PERIOD           
         LR    RF,RE               YES-LIMIT NUMBER OF WEEKS                    
         BCTR  RF,0                                                             
         LA    RE,1                                                             
         L     R8,AEXTRAWK                                                      
         AHI   R8,LSKEDS+2-EXTRAWKD                                             
         MVC   1(1,R8),0(R1)       BWS SPOTS/WEEK                               
         LA    R8,2(R8)                                                         
         BXLE  R1,RE,*-10                                                       
*                                                                               
REX36    L     R6,AEXTRAWK                                                      
         AHI   R6,LSKEDS-EXTRAWKD                                               
******** TM    LIND,LPOL                                                        
******** BO    REX38                                                            
******** LA    R8,LSKEDENL(R6)        FOR NON-POOL BUY,                         
******** OC    0(LSKEDENL,R8),0(R8)   MUST BE ONLY ONE BUY LINE                 
******** BZ    REX38                                                            
******** DC    H'0'                                                             
*                                                                               
REX38    CLI   0(R6),0             TEST END OF EXISTING BUY LINES               
         BE    REX43                                                            
         LA    R4,LWKINDS                                                       
         LA    R8,2(R6)                                                         
         LA    R0,53                                                            
*                                                                               
REX40    TM    0(R4),LFRZ          TEST WEEK FROZEN                             
         BO    REX42                                                            
         CLC   0(1,R8),1(R8)       COMPARE BUY SPOTS TO BWS SPOTS               
         BNL   REX41                                                            
         LA    R1,LSKEDENL(R8)     LOW --                                       
         CLI   0(R1),0             TEST ANY BUY SPOTS FOR THIS WEEK IN          
         BE    REX41               NEXT BUYLINE                                 
         ZIC   RE,1(R8)            YES -                                        
         ZIC   RF,0(R8)            DIFFERENCE BETWEEN BUY SPOTS AND             
         SR    RE,RF               BWS SPOTS GOES INTO BWS SPOTS FOR            
         STC   RE,1(R1)            NEXT BUYLINE                                 
         STC   RF,1(R8)            FOR THIS BYLINE, BWS SPTS = BUY SPTS         
*                                                                               
REX41    CLC   0(1,R8),1(R8)       TEST ANY CHANGE IN #SPOTS/WEEK               
         BE    REX42                                                            
         OI    1(R6),LCHG          YES - INDICATE CHANGE FOR BUYLINE            
*                                                                               
REX42    LA    R4,1(R4)            NEXT WEEK                                    
         LA    R8,2(R8)                                                         
         BCT   R0,REX40            DO FOR ALL WEEKS                             
*                                                                               
         LA    R6,LSKEDENL(R6)                                                  
         B     REX38               DO FOR ALL BUYLINES                          
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESS ALL THE BUYLINES                                            *         
***********************************************************************         
         SPACE 1                                                                
REX43    L     R6,AEXTRAWK                                                      
         AHI   R6,LSKEDS-EXTRAWKD                                               
*&&DO                                                                           
         TM    LIND,LSEPDLY        TEST SEPARATE DAILY TRANSFER                 
         BZ    REX44                                                            
         L     R1,LACURDTR         YES-JUST DELETE THE CURRENT DAILY            
         MVI   0(R1),FF                TRANSFER ELEMENT                         
         MVI   APELEM,FF                                                        
         B     REX45                                                            
*                                                                               
REX44    TM    LIND,LSEPLINE       TEST EFF COSTS TO SEPARATE LINES             
         BO    REX46               YES                                          
         MVI   APELEM,BTRELCDQ     NO-DELETE ALL EXISTING BUY TRANSFER          
*                                     ELEMENTS FROM BWS RECORD                  
REX45    GOTO1 ADELELS,BWDRECD                                                  
         B     REX49                                                            
*                                                                               
REX46    DS    0H                                                               
*                                                                               
REX46    LA    R4,BWDEL            YES-DELETE APPROPRIATE BUY TRANSFER          
         USING BTREL,R4                ELEMENTS                                 
         SR    R0,R0                                                            
*                                                                               
REX47    CLI   0(R4),0                                                          
         BE    REX49                                                            
         CLI   0(R4),BTRELCDQ                                                   
         BNE   REX48                                                            
         MVC   APBYTE,BTRIND                                                    
         NI    APBYTE,BTRIEC2+BTRIEC3                                           
         CLC   APBYTE,LBTRIND                                                   
         BNE   REX48                                                            
         MVI   0(R4),FF                                                         
         MVI   APELEM,FF                                                        
         GOTO1 ADELELS,BWDRECD                                                  
         B     REX47                                                            
*                                                                               
REX48    IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     REX47                                                            
         DROP  R4                                                               
*&&                                                                             
REX49    OC    0(LSKEDENL,R6),0(R6)   TEST END OF BUYLINES                      
         BZ    REX90                                                            
*                                                                               
         L     R0,AEXTRAWK         DID WE GO BEYOND TABLE (21 LINES)?           
         AHI   R0,LSKEDSX-EXTRAWKD                                              
         CR    R6,R0                                                            
         BNL   REX90               YES, NO MORE ENTRIES                         
*                                                                               
         ST    R6,LASKED           SAVE ADDRESS OF THIS SCHEDULE ENTRY          
         MVI   LNSPTS,255          PRE-SET MAX SPOTS/WEEK                       
         CLI   0(R6),0             TEST NEW BUY LINE TO BE CREATED              
         BE    REX66                                                            
****                                                                            
         TM    LCHGIND,LEFFCOST    DO WE HAVE A COST OVERRIDE FROM BUY?         
         BNZ   REX49AA              - YES WE DO, DON'T GET BUY AGAIN            
****                      BECAUSE WE DELETED THE OVERRIDE SPOT ALREADY!         
****                           MHC      10/14/03                                
         LA    R2,IOKEY            NO -                                         
         MVC   LBUYKSAV,IOKEY      SAVE BUY KEY                                 
         MVC   IOADDR,AIOAREA3                                                  
         GOTOR GETBUY,0            GET THE BUY RECORD                           
         BE    *+6                                                              
         DC    H'0'                                                             
REX49AA  L     R2,AIOAREA3                                                      
*                                                                               
         SR    R0,R0                                                            
         LA    R1,BDELEM                                                        
REX49A   CLI   0(R1),0                                                          
         BE    REX49B                                                           
         CLI   0(R1),X'71'         COST2 ELEM?                                  
         BE    REX49A1              - YUP, DELETE                               
         CLI   0(R1),X'73'         SECOND COST ELEM?                            
         BE    REX49C              LEAVE AMOUNT ALONE, AS PER MEL               
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     REX49A                                                           
*                                                                               
REX49A1  LR    R0,R1               SAVE OFF R1                                  
         MVI   APELEM,X'71'        DELETE ALL X'71' ELEMENTS                    
         GOTO1 ADELELS,BUYREC                                                   
         LR    R1,R0                                                            
         SR    R0,R0                                                            
         B     REX49A                                                           
*                                                                               
REX49B   L     RF,ATWA                                                          
         AHI   RF,SAVAREA-TWAD                                                  
         USING SAVAREA,RF                                                       
         OC    SVCOST2,SVCOST2     ADD SECOND COST ELEM?                        
         BNZ   REX49B0                                                          
         TM    LIND2,LCOS2         ELIGIBLE FOR COS2?                           
         BZ    REX49C               - NOPE                                      
         OC    LSVCS2,LSVCS2          LOCAL COS2?                               
         BNZ   REX49B2                                                          
         B     REX49C                                                           
********                                                                        
REX49B0  LA    R1,1                COST2 ONLY HAS ONE OF THE SINGLE             
REX49B1  C     R1,SVCOST2             BITS?                                     
         BNE   *+6                                                              
         DC    H'0'                DIE! DIE! DIE!                               
         SLL   R1,1                                                             
         C     R1,=X'80000000'        ARE WE DONE WITH TRAP?                    
         BNE   REX49B1                                                          
********                                                                        
REX49B2  XC    APELEM,APELEM                                                    
         LA    RE,APELEM                                                        
         USING COS2ELEM,RE                                                      
         MVI   0(RE),X'73'         NO DEFINED DSECT LABELS                      
         MVI   1(RE),COS2LENQ                                                   
         MVC   2(L'SVCOST2,RE),SVCOST2                                          
*                                                                               
         OC    LSVCS2,LSVCS2       WE HAVE A LOCAL COS2?                        
         BZ    REX49BX              - NOPE, ADD THE ELEMENT                     
         MVI   0(RE),X'71'         COS2 DOLLAR AMOUNT, NOT FACTOR               
*        L     R1,LACS2EL          WE'RE GONNA PULL DETAIL COS2                 
*        MVC   2(L'SVCOST2,RE),2(R1)                                            
         MVC   2(L'LSVCS2,RE),LSVCS2                                            
         DROP  RE,RF                                                            
*                                                                               
REX49BX  GOTO1 AADDELS,BUYREC                                                   
*                                                                               
REX49C   CLI   CLTBWPRO+7,C'N'     TEST DEMO CHANGES ALLOWED                    
         BE    REX55                                                            
         OC    LADEMEL,LADEMEL     TEST FOR BWS DEMO ELEMENT                    
         BZ    REX50                                                            
         MVI   APELEM,2            YES - DELETE DEMO ELEMENT FROM               
         GOTO1 ADELELS,BUYREC            BUY RECORD                             
*                                                                               
REX50    OC    LAUPGEL,LAUPGEL     TEST FOR BWS UPGRADE ELEMENT                 
         BNZ   REX52                                                            
         OC    LAODTEL,LAODTEL           OR BWS OVERRIDE ELEMENT                
         BNZ   REX52                                                            
         OC    CMPUP,CMPUP               OR DEFAULT CAMPAIGN UPGRADE            
         BZ    REX54                                                            
*                                                                               
REX52    MVI   APELEM,X'62'        YES - DELETE UPGRADE ELEMENT FROM            
         GOTO1 ADELELS,BUYREC            BUY RECORD                             
*                                                                               
REX54    GOTO1 ADEMADD             ADD DEMO AND UPGRADE ELEMENTS TO BUY         
         BNE   REXX                                                             
*                                                                               
REX55    MVI   APELEM,X'66'        DELETE BUY RECORD COMMENT                    
         GOTO1 ADELELS,BUYREC                                                   
         OC    LACMTEL,LACMTEL     TEST FOR BWS COMMENT ELEMENT                 
         BZ    REX56                                                            
         GOTO1 ACMTADD             YES - ADD COMMENT ELEMENT                    
*&&DO                                                                           
REX56    MVI   APELEM,X'70'        DELETE OLD ID ELEMENT                        
         GOTO1 ADELELS,BUYREC                                                   
         GOTO1 AIDADD              OPTIONALLY ADD NEW ID ELEMENT                
         BNE   REXX                                                             
*&&                                                                             
REX56    GOTO1 AWARNADD                                                         
*&&DO                                                                           
         TM    LFLAG,LORBIT        TEST ORBIT                                   
         BZ    REX60                                                            
         MVI   APELEM,X'67'        YES - ADD NEW ORBIT ELEMENT                  
         GOTO1 ADELELS,BUYREC                                                   
         GOTO1 AORBADD                                                          
         BNE   REXX                                                             
*&&                                                                             
REX60    TM    LIND,LPOL           FOR POOL BUY  --                             
         BZ    REX66                                                            
         LA    R4,BDELEM           FIND LENGTH OF RECORD WITHOUT                
         SR    R0,R0               SPOT ELEMENTS                                
         LA    R1,BDELEM-BUYRECD                                                
*                                                                               
REX61    CLI   0(R4),0                                                          
         BE    REX62                                                            
         IC    R0,1(R4)                                                         
         CLI   0(R4),X'0B'                                                      
         BL    *+12                                                             
         CLI   0(R4),X'18'                                                      
         BNH   *+6                                                              
         AR    R1,R0                                                            
         AR    R4,R0                                                            
         B     REX61                                                            
*                                                                               
REX62    BAS   RE,GETMAXSP         GET MAX N'SPOTS FOR THIS BUY RECORD          
         LA    R6,2(R6)                                                         
         LA    R0,53                                                            
         LA    R4,LWKINDS                                                       
         SR    R1,R1               R1 = N'WEEKS GETTING MORE SPOTS              
         SR    R8,R8               R8 = N'SPOTS STAYING IN BUY RECORD           
         SR    R9,R9               R9 = N'SPOTS TO ADD TO BUY RECORD            
         SR    RE,RE                                                            
         SR    RF,RF                                                            
*                                                                               
REX63    IC    RE,0(R6)                                                         
         TM    0(R4),LFRZ          TEST WEEK IS FROZEN                          
         BO    REX64               YES-ALL EXISITING SPOTS STAY                 
         IC    RF,1(R6)                                                         
         CR    RE,RF               COMPARE EXISTING TO PROPOSED                 
         BL    *+10                                                             
         LR    RE,RF               NOT LOW - N'PROPOSED SPOTS STAY              
         B     REX64                                                            
         SR    RF,RE               LOW-ALL EXISTING SPOTS STAY                  
         AR    R9,RF                   ADD THE DIFFERENCE                       
         LA    R1,1(R1)                AUGMENT N'WEEKS GETTING MORE             
*                                                                               
REX64    AR    R8,RE                                                            
         LA    R4,1(R4)                                                         
         LA    R6,2(R6)                                                         
         BCT   R0,REX63            NEXT WEEK                                    
*                                                                               
         LTR   R9,R9               TEST ANY SPOTS TO BE ADDED                   
         BZ    REX66                                                            
         L     RF,LMAXSPTS         SET MAX SPOTS PER WEEK HIGH                  
         STC   RF,LNSPTS                                                        
         SR    RF,R8               RF=MAX N'SPOTS THAT CAN BE ADDED             
         BP    *+10                                                             
         SR    RF,RF               NOT POSITIVE-ALL NEW SPOTS GO TO             
         B     REX65                            NEXT BUYLINE                    
         CR    R9,RF               TEST SPTS TO BE ADDED GT MAX ALLOWED         
         BNH   REX66                                                            
         SR    RE,RE               YES-DIVIDE TO GIVE MAX N'SPOTS THAT          
         DR    RE,R1                   CAN BE ADDED PER WEEK                    
*                                                                               
REX65    STC   RF,LNSPTS           SET MAX N'SPOTS CAN BE ADDED PER WK          
*                                                                               
*                                                                               
REX66    ZIC   R0,CMPNWKS          DUMMY UP A SPOTS/WEEK ELEMENT                
         LA    R4,LWKINDS                                                       
         L     R6,LASKED                                                        
         LA    R8,2(R6)                                                         
         LA    R9,LSPWEL                                                        
         USING NBRSPEL,R9                                                       
         MVI   NBRSPEL,NBRSPELQ    ELEMENT CODE                                 
         LR    RE,R0                                                            
         LA    RE,NBRSPSPW-NBRSPEL(RE)                                          
         STC   RE,NBRSPLEN         ELEMENT LENGTH                               
         LA    R9,NBRSPSPW         BUILD SPOTS PER WEEK                         
         ZIC   R5,LNSPTS           R5=MAX SPOTS ALLOWED TO ADD PER WEEK         
*                                                                               
REX68    ZIC   RE,0(R8)                                                         
         LR    R1,RE               R1=N'SPOTS FOR THIS WEEK                     
         TM    0(R4),LFRZ          TEST WEEK IS FROZEN                          
         BO    REX70               YES-SPOTS STAY PUT                           
         ZIC   RF,1(R8)                                                         
         LR    R1,RF                                                            
         SR    RF,RE               TEST SPOTS TO ADD                            
         BNP   REX70                                                            
         SR    RF,R5               YES-TEST GREATER THAN MAX ALLOWED            
         BNP   REX70                                                            
         SR    R1,RF               YES-THIS BUYLINE ONLY GETS MAX               
         STC   R1,1(R8)                                                         
         IC    RE,LSKEDENL+1(R8)   AND ADD REMAINDER TO NEXT BUYLINE            
         AR    RE,RF                                                            
         STC   RE,LSKEDENL+1(R8)                                                
         OI    LSKEDENL+1(R6),LCHG MAKE SURE NEXT BUYLINE IS CHANGED            
*                                                                               
REX70    STC   R1,0(R9)            STORE N'SPOTS INTO SPW ELEMENT               
*                                                                               
REX72    LA    R4,1(R4)            NEXT WEEK                                    
         LA    R8,2(R8)                                                         
         LA    R9,1(R9)                                                         
         BCT   R0,REX68            DO FOR ALL WEEKS                             
*                                                                               
         L     R4,LASPWEL          LASPWEL=A(DUMMY SPOTS PER WEEK ELEM          
         LA    R1,LSPWEL                     FOR BLDWKS)                        
         ST    R1,LASPWEL                                                       
*                                                                               
         LA    R1,LBADD            PASS ADD/CHANGE INDICATOR TO BLDWKS          
         CLI   0(R6),0                                                          
         BE    *+8                                                              
         LA    R1,LBCHA                                                         
         GOTO1 ABLDWKS             BUILD WEEKS TABLE                            
         ST    R4,LASPWEL                                                       
         DROP  R9                                                               
*                                                                               
         CLI   0(R6),0             TEST NEW BUY LINE TO BE ADDED                
         BNE   REX73                                                            
         TM    LIND,LPOL           YES - MUST BE POOL BUY                       
         BO    *+6                                                              
         DC    H'0'                                                             
         CLI   LNSPWKS,0           TEST ANY WEEKS WITH SPOTS                    
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                  YES-GO ADD THE NEW BUY LINE                  
         L     R5,ATWA             RESTORE R5 OTHERWISE TWAD IS MESSED          
*                                                                               
         GOTO1 =A(ADDBUY),RR=APRELO                                             
         B     REX89                                                            
*                                                                               
REX73    MVC   LBUYCOST,NBRSCST1   ALTER EXISTING BUY LINE                      
         CLI   CLTBWPRO+6,C'N'     TEST COST CHANGES ALLOWED                    
         BNE   REX73B                                                           
         SR    RF,RF                NO-COST TO BUY COST                         
         ICM   RF,7,BDCOST                                                      
         TM    BDCIND2,X'20'       TEST CANADIAN                                
         BO    REX73A                                                           
         TM    BDCIND2,X'10'       US BUY IN DOLLARS?                           
         BZ    REX73A                                                           
         MHI   RF,100              YES, CONVERT TO PENNIES                      
REX73A   STCM  RF,15,LBUYCOST                                                   
REX73B   L     R5,ATWA                                                          
         GOTO1 ABUYDESC,LBCHA                                                   
         BNE   REXX                                                             
         GOTO1 =A(CALCSEDY),RR=APRELO                                           
*                                                                               
***  WE DON'T NEED LEFFCOST ANYMORE BECAUSE IT IS ALREADY CHANGED IN            
***  REX15!!   NOTE: LEFFCOST IS FOR COST OVERRIDE FROM BUY PROGRAM             
***      TM    LCHGIND,LSTDATE+LEFFCOST+LSLN+LNEWPRD   TEST ANY CHANGES         
         TM    LCHGIND,LSTDATE+LSLN+LNEWPRD            TEST ANY CHANGES         
         BZ    REX74         THAT WILL EFFECT SPOT ELEMENTS                     
         GOTO1 ASPOTCHG      YES-GO CHANGE THEM                                 
         BNE   REXX                COMING BACK WITH AN ERROR?  YES              
*                                                                               
REX74    DS    0H                                                               
*************************************                                           
*  SKIPPING THIS BLOCK BECAUSE WE SHOULD ALWAYS TRANSFER                        
*************************************                                           
*&&DO                                                                           
         TM    1(R6),LCHG          TEST ANY CHANGE TO NUMBER OF SPOTS           
         BZ    REX81               IN THIS BUYLINE                              
*&&                                                                             
*************************************                                           
*  SKIPPING THIS BLOCK BECAUSE WE SHOULD ALWAYS TRANSFER                        
*************************************                                           
*                                                                               
* ADD/DELETE SPOT ELEMENTS IN BUY RECORD                                        
*                                                                               
         OI    LFLAG,LFSTWK                                                     
         LA    R0,53                                                            
         LA    R4,LWKINDS                                                       
         LA    R5,LWKTAB                                                        
         LA    R8,2(R6)            R8 = A(SPOTS/WEEK FIELD IN SKED TBL)         
         L     R9,ATWA                                                          
         AHI   R9,CMPDATSP-TWAD                                                 
*                                                                               
REX76    TM    0(R4),LFRZ          TEST WEEK IS FROZEN                          
         BO    REX80               YES - LEAVE WEEK ALONE                       
         TM    LIND,LPOL                                                        
         BO    REX78                                                            
** THE FOLLOWING CHECK CAN ONLY BE VALID FOR NON-POOL BECAUSE POOL CAN          
** HAVE OVERRIDE COSTS WHICH CAN DISGUISE THE NUMBER.  FOR POOL, WE             
** NEED TO CHECK IF THE OVERRIDE NUMBER OF SPOTS ARE ALSO THE SAME              
*****  PATCH                                                                    
         CLI   0(R5),X'FF'         ARE WE DONE?                                 
         BE    REX76C                                                           
         MVC   1(1,R8),1(R5)        - NOPE, REPLACE                             
*****  PATCH                                                                    
REX76C   CLC   0(1,R8),1(R8)       TEST CHANGE TO NUMBER OF SPOTS               
         BE    REX80               NO - SKIP THIS WEEK                          
         GOTO1 ANPOLSPT            ADD/DELETE NON-POOL SPOTS                    
         BNE   REX99                                                            
         B     REX80                                                            
*                                                                               
REX78    GOTO1 APOOLSPT            ADD/DELETE POOL SPOTS                        
         BNE   REX99               EXIT FOR RECORD OVERFLOW                     
*                                                                               
REX80    LA    R4,1(R4)            NEXT WEEK                                    
         LA    R5,8(R5)                                                         
         LA    R8,2(R8)                                                         
         LA    R9,4(R9)                                                         
         NI    LFLAG,FF-LFSTWK                                                  
         BCT   R0,REX76            DO FOR ALL WEEKS                             
*                                                                               
         TM    LIND,LPOL           ARE WE BUYING POL?                           
         BZ    REX81               NO, THIS DOESN'T CONCERN NON-POL             
         CLI   LBADDCHA,LBADD      ARE WE ADDING OR CHANGING?                   
         BE    REX81                                                            
         TM    MISCFLG1,MF1UP2OV   UPTO OVERRIDE COSTS?                         
         BNZ   REX81                                                            
REX80C00 LA    R9,BDELEM                                                        
         XR    R0,R0                                                            
REX80C05 CLI   0(R9),0             NO SPOTS?                                    
         BE    REX81               LEAVE ALONE                                  
         CLI   0(R9),X'0B'                                                      
         BE    REX80C10                                                         
         CLI   0(R9),X'0C'                                                      
         BE    REX80C10                                                         
         IC    R0,1(R9)                                                         
         AR    R9,R0                                                            
         B     REX80C05                                                         
*                                                                               
REX80C10 L     R8,LAWKST                                                        
         LTR   R8,R8               ANYTHING?                                    
         BZ    REX81               NONE                                         
         LA    R0,2(R8)            THIS IS WHAT IS NORMALLY SET                 
         ST    R0,APPARM                                                        
         CLC   2(2,R8),RDATE-REGELEM(R9)   1ST WK DATE > 1ST SPOT DATE?         
         BNH   REX80C20            YES, DON'T CHANGE BDSTART                    
         LA    R0,RDATE-REGELEM(R9)                                             
         ST    R0,APPARM                                                        
REX80C20 MVI   APPARM,2                                                         
         GOTO1 VDATCON,APPARM,,(3,BDSTART)   BUY START DATE                     
*                                                                               
REX81    MVI   APELEM,X'6A'        DELETE OLD VAT ELEMENT                       
         GOTO1 ADELELS,BUYREC                                                   
         GOTO1 AVATADD             ADD NEW ONE                                  
*                                                                               
         MVI   APELEM,X'6B'        DELETE OLD PST ELEMENT                       
         GOTO1 ADELELS,BUYREC                                                   
         OC    LPST,LPST           TEST FOR PST CODES                           
         BZ    REX82                                                            
         GOTO1 APSTADD             YES-ADD PST ELEMENT                          
         XC    BDNTAX,BDNTAX       CLEAR OUT THE TAX IF WE HAVE PST             
*                                                                               
REX82    MVI   APELEM,X'61'        DELETE OLD MASTER CLIENT ELEMENT             
         GOTO1 ADELELS,BUYREC                                                   
         OC    CLTMTCLT,CLTMTCLT   TEST MASTER TRAFFIC CLIENT CODE              
         BZ    REX83                                                            
         GOTO1 AMCLTADD            YES-ADD MASTER CLIENT ELEMENT                
*                                                                               
REX83    MVI   APELEM,BWSCODEQ     DELETE OLD BWS TRANSFER ELEMENT              
         GOTO1 ADELELS,BUYREC                                                   
         BAS   RE,ADDBWS           AND ADD CURRENT ONE                          
*                                                                               
         XR    R1,R1               NO REASON CODE REQ FOR TRANSFERS             
         LA    R6,BDELEM                                                        
REX84    CLI   0(R6),0                                                          
         BE    REX86                                                            
         CLI   0(R6),RCELCODQ      X'90' ELEM                                   
         BE    REX85                                                            
         IC    R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     REX84                                                            
*                                                                               
         USING RCELEM,R6                                                        
REX85    MVI   RCFLDID,RCID_NONE                                                
         DROP  R6                                                               
*                                                                               
REX86    MVC   IOKEY,LBUYKSAV                                                   
         MVC   IOADDR,AIOAREA1                                                  
         GOTOR GETBUY,IOLOCK       GET THE BUY RECORD FOR UPDATE                
         L     R1,AIOAREA1                                                      
         MVC   IOADDR,AIOAREA3                                                  
*****                                                                           
         L     RF,LASKED                                                        
         TM    1(RF),LDONTCHG      DON'T CHANGE THE BUYLINE?                    
         BNZ   REX88               DON'T CHANGE                                 
*****                                                                           
         GOTO1 APUTBUY             PUT THE BUY RECORD                           
         BNE   REXX                                                             
*                                                                               
REX88    XC    APELEM,APELEM       BUY TRANSFER ELEM FOR DETAIL RECORD          
         LA    R6,APELEM                                                        
         USING BTREL,R6                                                         
         MVI   BTRELCD,BTRELCDQ                                                 
         MVC   BTRLINE,BUYKBUY     BUY LINE                                     
         TM    LIND,LSEPLINE                                                    
         BZ    *+10                                                             
         OC    BTRIND,LBTRIND                                                   
         MVC   BTRDATE,ASBDAT      TODAY'S DATE                                 
         OI    BTRIND,BTRIDATE     INDICATE ELEMENT CONTAINS THE DATE           
         LA    R4,LSPWEL                                                        
         USING NBRSPEL,R4                                                       
         ZIC   RE,NBRSPLEN                                                      
         AHI   RE,-(NBRSPSPW-NBRSPEL)                                           
         BNZ   *+6                                                              
         DC    H'0'                                                             
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   BTRSPW(0),NBRSPSPW                                               
         LA    RE,BTRSPW-BTREL+1(RE)                                            
         STC   RE,BTRELLN                                                       
         TM    LIND,LSEPDLY        TEST SEPARATE DAILY TRANSFER                 
         BZ    *+8                                                              
         BAS   RE,BLDDTR           YES-BUILD DAILY TRANSFER ELEMENT             
*****    GOTO1 AADDELS,BWDRECD     ADD BUY TRANSFER ELEMENT                     
*                                                                               
REX89    L     R6,LASKED           NEXT BUY LINE                                
         LA    R6,LSKEDENL(R6)                                                  
         B     REX49                                                            
*                                                                               
REX90    B     REXX                ALL BUY LINES PROCESSED - EXIT               
*                                                                               
REX92    DS    0H                                                               
***REX92    OI    BWDINDS,BWDITRLK    BUY TRANSFER LOCK-OUT                     
         B     REXX                EXIT                                         
*                                                                               
REX99    MVC   FVXTRA,SPACES       BUY RECORD OVERFLOW                          
         XC    APDUB,APDUB                                                      
         L     R1,AIOAREA2                                                      
         MVC   APDUB+2(3),NBRKSTA-NBRKEY(R1)                                    
         GOTO1 VMSUNPK,APPARM,(X'80',APDUB),APWORK,APWORK+4                     
         MVC   FVXTRA(4),APWORK+4    DISPLAY STATION AND BUYLINE                
         LA    RF,FVXTRA+3                                                      
         CLI   FVXTRA+3,C' '                                                    
         BNH   *+8                                                              
         LA    RF,1(RF)                                                         
         CLI   APWORK+4,C'0'         TEST CABLE STATION                         
         BL    *+18                                                             
         MVI   0(RF),C'/'                                                       
         MVC   1(3,RF),APWORK+9                                                 
         LA    RF,4(RF)                                                         
         MVC   1(2,RF),=C'LN'                                                   
         L     R1,LASKED                                                        
         ZIC   RE,0(R1)                                                         
         CVD   RE,APDUB                                                         
         UNPK  4(3,RF),APDUB                                                    
         OI    6(RF),X'F0'                                                      
*                                                                               
REXX     CLC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
         DROP  R3,R6                                                            
         EJECT                                                                  
***********************************************************************         
* ADD BWS TRANSFER ELEMENT TO BUYLINE                                 *         
***********************************************************************         
         SPACE 1                                                                
ADDBWS   LR    R0,RE                                                            
         USING BUYRECD,R2                                                       
         XC    APELEM,APELEM                                                    
         LA    R4,APELEM                                                        
         USING BWSELEM,R4                                                       
         MVI   BWSCODE,BWSCODEQ                                                 
         MVI   BWSLEN,BWSLENQ                                                   
         MVC   BWSBYR,QBYR                                                      
         MVC   BWSCAM,BCAM                                                      
         XC    BWSCAM,=X'FFFF'                                                  
         MVC   BWSDATE,ASBDAT      TODAY'S DATE                                 
         GOTO1 AADDELS,BUYREC                                                   
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* SET MASTER PRODUCT                                                  *         
* OUTPUT : LMASPRD. BDMASPRD WILL GET SET LATER ON FROM THIS FIELD    *         
***********************************************************************         
         SPACE 1                                                                
SETPRD   DS    0H                                                               
         TM    LIND,LPOL           TEST POL BUY                                 
         JZ    SETPRDX             NO-THERE'S NO BDMASPRD                       
         XC    APHALF,APHALF                                                    
         MVC   APHALF(1),BPRD      CAMPAIGN PRODUCT                             
         CLI   CMPPRD1,0           TEST FOR CAMPAIGN PIGGYBACKS                 
         JE    SETPRD2                                                          
         MVC   APHALF(1),CMPPRD1   YES                                          
         MVC   APHALF+1(1),CMPPRD2                                              
         J     SETPRD4                                                          
*                                                                               
SETPRD2  CLI   BPRD,FF             TEST CAMPAIGN PRODUCT = POL                  
         JNE   SETPRD4                                                          
         CLI   INOPRD,0            YES-SET MASTER PRODUCT FROM THE              
         JE    SETPRDX                 PRD OPTION IF IT'S GIVEN                 
         MVC   APHALF,INOPRD                                                    
*                                                                               
SETPRD4  MVC   LMASPRD,APHALF                                                   
*                                                                               
SETPRDX  BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* BUILD A DAILY TRANSFER ELEMENT FROM A REGULAR BUY TRANSFER ELEMENT  *         
* INPUT  : APELEM CONTAINS BUY TRANSFER ELEMENT                       *         
* OUTPUT : APELEM CONTAINS DAILY TRANSFER ELEMENT                     *         
***********************************************************************         
         SPACE 1                                                                
BLDDTR   LR    R0,RE                                                            
         LA    R1,APELEM                                                        
         USING BTREL,R1                                                         
         XC    APWORK,APWORK                                                    
         LA    RF,APWORK                                                        
         USING DTREL,RF                                                         
         MVI   DTRELCD,DTRELCDQ                                                 
         MVI   DTRELLN,DTRELLNQ                                                 
         MVC   DTRDAY,LDTRDATE                                                  
         MVC   DTRLINE,BTRLINE                                                  
         ZIC   RE,LDTRDAY                                                       
         LA    RE,BTRSPW-1(RE)                                                  
         MVC   DTRSPOTS,0(RE)                                                   
         MVC   DTRDATE,ASBDAT                                                   
         LA    RE,DTRELLNQ                                                      
         BCTR  RE,0                                                             
         MVC   APELEM(0),APWORK                                                 
         EX    RE,*-6                                                           
         LR    RE,R0                                                            
         BR    RE                                                               
         DROP  R1,RF                                                            
         EJECT                                                                  
***********************************************************************         
* CALCULATE MAX NUMBER OF SPOTS THAT WILL FIT IN REMAINDER OF BUYREC  *         
* INPUT:  R1=LENGTH OF BUY RECORD                                     *         
* OUTPUT: R1=LMAXSPTS=MAX NUMBER OF SPOTS                             *         
***********************************************************************         
         SPACE 1                                                                
         USING BUYRECD,R2                                                       
GETMAXSP LNR   R1,R1                                                            
****     LA    R1,4000(R1)   <==== FOR 4K RECS                                  
         AHI   R1,6000       <==== FOR 6K RECS                                  
         SR    R0,R0                                                            
         D     R0,=F'24'           ALLOW 24 BYTES/SPOT AS PER MEL               
         ST    R1,LMAXSPTS                                                      
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ERROR EXITS                                                         *         
***********************************************************************         
         SPACE 1                                                                
         USING TWAD,R5                                                          
ESAT     MVC   FVMSGNO,=AL2(FVIACTSA)                                           
         B     ERRORX                                                           
EMKT     MVC   FVMSGNO,=AL2(FVSTAXFR)                                           
         B     ERRORX                                                           
EEST     MVC   FVMSGNO,=AL2(FVESTLOK)                                           
         B     ERRORX                                                           
ECLT     MVC   FVMSGNO,=AL2(FVCFRZN)                                            
         B     ERRORX                                                           
PWLCKED  MVC   FVMSGNO,=AL2(FVPWLOCK)                                           
         B     ERRORX                                                           
C2LCKED  MVC   FVMSGNO,=AL2(FVC2LOCK)                                           
         B     ERRORX                                                           
EENO     MVC   FVMSGNO,=AL2(FVESTNO)                                            
         B     ERRORX                                                           
ENOPOLES MVC   FVMSGNO,=AL2(FVNOPEST)                                           
         B     ERRORX                                                           
*                                                                               
ERRORX   LA    R1,BWSKEYH                                                       
         ST    R1,FVADDR                                                        
EXIT     XIT1  ,                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CONSTANTS                                                           *         
***********************************************************************         
         SPACE 1                                                                
SPACES   DC    80C' '                                                           
*                                                                               
FF       EQU   X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* READING THE STATION MASTER RECORD TO GET THE OVERRIDING CALL LETTERS          
*  INPUT:      APBYTE USED TO CONTAIN THE NSI OR BBM INDICATOR                  
*              APWORK+64 HAS LAST ITERATION'S STATION                           
*              APWORK+72 HAS LAST ITERATION'S STA OVERRIDE CALL LETTRS          
*              APWORK+76 HAS LAST ITERATION'S CODE                              
* OUTPUT:      APFULL USED TO CONTAIN OVERRIDING STATION CALL LETTERS           
***********************************************************************         
STAMASRD NTR1 BASE=*,LABEL=*                                                    
         L     R3,AIOAREA2                                                      
         USING NBRRECD,R3                                                       
         LA    RE,NBRFSTEL                                                      
         USING NBRSELD,RE                                                       
         CLC   =X'0D6B',0(R3)      DO WE HAVE A BUY REVISION RECORD?            
         BE    *+6                                                              
         DC    H'0'                DIE FOR NOW                                  
*                                                                               
         MVC   APWORK(L'IOKEY),IOKEY   SAVE OFF IOKEY                           
         XC    IOKEY,IOKEY                                                      
         LA    RF,IOKEY                                                         
         USING STARECD,RF                                                       
         MVI   STAKEY,C'0'                                                      
         MVC   STAKEY+1(STAKEYLN-1),STAKEY                                      
         MVI   STAKTYPE,C'S'                                                    
         MVC   STAKMED,QMED                                                     
         CLC   NBRSSTA,APWORK+64   SAME STATION?                                
         BNE   SMREAD10             - NOPE, NEED TO READ IT                     
         CLC   APBYTE,APWORK+76    SAME CODE?  (NSI/BBM?)                       
         BNE   SMREAD10             - NOPE, NEED TO READ IT                     
         MVC   APFULL,APWORK+72    USE SAME STATION OVERRIDE LETTERS            
         B     SMREADX                                                          
*                                                                               
SMREAD10 DS    0H                                                               
         MVC   STAKCALL,NBRSSTA                                                 
         MVC   APWORK+64(8),NBRSSTA   SAVE OFF FOR FUTURE COMPARES              
         DROP  R3,RE                                                            
         MVC   STAKAGY,CUAALF                                                   
         MVC   APFULL,IOADDR       SAVE OFF THE IOADDR                          
         MVC   IOADDR,AIOAREA4     WE'LL USE AIOAREA4                           
*                                                                               
         GOTO1 AIO,IOSTAFIL+IORD                                                
         BE    *+6                                                              
         DC    H'0'                THIS SHOULD NEVER HAPPEN                     
*                                                                               
         LA    RF,IOKEY                                                         
         MVC   STAKCLT,QCLT        SEE IF THERE IS CLIENT SPECIFIC REC          
         DROP  RF                                                               
         GOTO1 AIO,IOSTAFIL+IORD                                                
*                                                                               
         MVC   IOADDR,APFULL       WE'RE MOVING IT BACK                         
         L     RF,AIOAREA4                                                      
         USING STARECD,RF                                                       
         CLI   APBYTE,C'0'         NSI?                                         
         BNE   *+14                                                             
         MVC   APFULL,SRS1CALL     SAVE OFF STATION OVERRIDE                    
         MVI   APWORK+77,SQNORS1I   X'10'                                       
         CLI   APBYTE,C'1'         BBM?                                         
         BNE   *+14                                                             
         MVC   APFULL,SRS2CALL     SAVE OFF STATION OVERRIDE                    
         MVI   APWORK+77,SQNORS2I   X'20'                                       
***                                                                             
         MVC   APWORK+72(4),APFULL   SAVE OFF STA OVERRIDE LETTERS              
         MVC   APWORK+76(1),APBYTE   SAVE OFF CODE                              
         NC    APWORK+77(1),SFLAG1   SEE IF WE NEED TO SUPPRESS IMPS            
         DROP  RF                                                               
*                                                                               
SMREADX  MVC   IOKEY,APWORK        PUT THE OLD KEY BACK                         
         J     EXIT                                                             
***********************************************************************         
* GET BUY RECORD                                                      *         
* INPUT  : IOKEY = BUY RECORD KEY                                     *         
*          IOADDR = A(RECORD BUFFER)                                  *         
*          R1 = 0 FOR GETREC                                          *         
*               IOLOCK FOR GETREC FOR UPDATE                          *         
* OUTPUT : CC EQ - BUY RECORD READ SUCCESSFULLY                       *         
*          CC NE - RECORD NOT FOUND                                   *         
***********************************************************************         
         SPACE 1                                                                
GETBUY   NTR1  BASE=*,LABEL=*                                                   
         LR    R4,R1                                                            
         GOTO1 AIO,DIRHI           READ BUY RECORD POINTER                      
         BNE   GBNEX                                                            
         CLC   IOKEY(BUYKBUY+2-BUYKEY),IOKEYSAV    TEST RECORD IS THERE         
         BNE   GBNEX                                                            
         LA    R1,FILGET(R4)                                                    
         BASR  RE,RF               READ BUY RECORD                              
         BNE   GBNEX                                                            
         TM    LFLAG,LPKGREX       TEST PACKAGE RETRANSFER                      
         BZ    GBEQX                                                            
         L     R2,IOADDR                                                        
         USING BUYRECD,R2                                                       
         CLC   LPKGMAST,BUYKBUY    TEST THIS IS PACKAGE MASTER RECORD           
         BNE   GBEQX                                                            
         MVC   LPKGMADA,IODA       YES - SAVE ITS D/A                           
*                                                                               
GBEQX    CR    RB,RB                                                            
         B     *+6                                                              
GBNEX    LTR   RB,RB                                                            
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* THIS SUBROUTINE FINDS THE COMPRESSED DATE FOR BUY SPOT (LSPTDATE)   *         
* WHEN RETRANSFERRING, NEW SPOTS WILL BE ADDED ON SEPARATE LINES      *         
***********************************************************************         
SAVRDATE NTR1  BASE=*,LABEL=*                                                   
         LA    R2,IOKEY            TO BUY RECORDS                               
         USING BUYRECD,R2                                                       
         XC    BUYKEY,BUYKEY       BUILD BUY KEY                                
         MVC   BUYKAM,BAGYMD       AGENCY-MEDIA                                 
         MVC   BUYKCLT,BCLT        CLIENT                                       
         MVC   BUYKPRD,BPRD        PRODUCT                                      
         MVC   BUYMSTA(2),BMKT     MARKET                                       
         OC    LOVRMKT,LOVRMKT     TEST MARKET OVERRIDE                         
         BZ    *+10                                                             
         MVC   BUYMSTA(2),LOVRMKT                                               
         L     R1,AIOAREA2                                                      
         USING NBRKEY,R1                                                        
         MVC   BUYMSTA+2(3),NBRKSTA  STATION                                    
         MVC   BUYKEST,BEST        ESTIMATE                                     
         MVC   BUYKBUY(3),NBRKKBUY   BUY DETAILS                                
         DROP  R1                                                               
*                                                                               
         MVC   IOADDR,AIOAREA4                                                  
         XC    LWKINDS,LWKINDS                                                  
         GOTO1 GETBUY,0            GET THE BUY RECORD                           
*                                                                               
         L     R2,AIOAREA4                                                      
         LA    R8,BDELEM                                                        
         USING BDELEM,R8                                                        
*                                                                               
         GOTO1 VDATCON,APPARM,(3,BDSTART),(2,APWORK)                            
*                                                                               
         L     RF,AEXTRAWK                                                      
         USING EXTRAWKD,RF                                                      
         MVC   LSPTDATE,APWORK                                                  
         DROP  RF                                                               
*                                                                               
*AVRD20  CLI   0(R8),0             NO MORE ELEMS??                              
*        BE    SAVRDX               - NO SPOTS                                  
*        CLI   0(R8),X'06'                                                      
*        BL    SAVRDBMP                                                         
*        CLI   0(R8),X'0D'                                                      
*        BH    SAVRDBMP                                                         
*        USING REGELEM,R8                                                       
*        L     RF,AEXTRAWK                                                      
*        USING EXTRAWKD,RF                                                      
*        MVC   LSPTDATE,RDATE                                                   
*        DROP  RF                                                               
*        B     SAVRDX                                                           
*                                                                               
*AVRDBMP XR    R0,R0                                                            
*        IC    R0,1(R8)                                                         
*        AR    R8,R0                                                            
*        B     SAVRD20                                                          
*                                                                               
SAVRDX   J     EXIT                                                             
***********************************************************************         
* THIS SUBROUTINE FINDS THE COMPRESSED DATE FOR THIS DAY (LDTRDAT2)   *         
* WHEN RETRANSFERRING, NEW SPOTS WILL BE ADDED ON SEPARATE LINES      *         
***********************************************************************         
FIXFXDLY NTR1  BASE=*,LABEL=*                                                   
         XR    R1,R1                                                            
         IC    R1,LDTRDAY          LET'S SEE HOW MANY TIMES WE BUMP             
         BCTR  R1,0                STARTS WITH 0                                
         MHI   R1,4                                                             
*                                                                               
         L     R2,ATWA             R2 SHOULD POINT TO RIGHT DATE NOW            
         AHI   R2,CMPDATSP-TWAD                                                 
         LA    R2,0(R1,R2)                                                      
         L     RF,AEXTRAWK                                                      
         USING EXTRAWKD,RF                                                      
*                                                                               
         MVC   LDTRDAT2,0(R2)      NEED THE DATE                                
*****    STCM  R2,3,LDTRDAT2                                                    
*******  GOTO1 VDATCON,APPARM,(3,(R2)),(2,LDTRDAT2)                             
         DROP  RF                                                               
*                                                                               
FIXFXDX  J     EXIT                                                             
***********************************************************************         
* CHECKS THE CLIENT RECORD TO MAKE SURE IT'S A TRADE   FOR COS2                 
*                                     MHC  06/29/04                             
***********************************************************************         
COS2CHK  NTR1  BASE=*,LABEL=*                                                   
         MVC   APWORK(L'IOKEY),IOKEY   SAVE OFF THE IOKEY                       
***                                                                             
         LA    R4,IOKEY            BUILD KEY OF CLIENT RECORD                   
         USING CLTHDRD,R4                                                       
         XC    CKEY,CKEY                                                        
         MVI   CKEYTYPE,0                                                       
         MVC   CKEYAM,BAGYMD                                                    
         MVC   CKEYCLT,BCLT                                                     
         GOTO1 AIO,FILRD2          READ CLIENT RECORD                           
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,AIOAREA2                                                      
         TM    COPT4,COP4TRD       TRADE?                                       
         BO    COS2YES              - YUP                                       
         TM    COPT3,COP3COSQ      OPTIONAL?                                    
         BNO   COS2NO               - NOPE                                      
         DROP  R4                                                               
*                                                                               
COS2YES  CR    RE,RE               SET THE CONDITION                            
         B     COS2CHKX                                                         
COS2NO   CR    RE,RA                                                            
COS2CHKX MVC   IOKEY,APWORK        RESTORE THE IOKEY                            
         J     EXIT                                                             
***********************************************************************         
* BUILD AND ADD BUY RECORD(S)                                         *         
* BUY TRANSFER ELEMENT IS ADDED TO BWS RECORD FOR EACH BUY LINE ADDED *         
*******                                                               *         
* ALSO ADDS A DARE batch RECORD IF THE BUY DOESN'T ALREADY BELONG TO  *         
*     A  SENT DARE ORDER  OR  DARE BATCH                              *         
***********************************************************************         
ADDBUY   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
*****    GOTO1 =A(CHKDARLK),RR=APRELO    LOCKED BY DARE?                        
*****    BE    ADDBX               YES                                          
*                                                                               
**** ADDB00   L     R3,AIOAREA2                                                 
         USING NBRSELD,R3                                                       
ADDB00   LA    R2,IOKEY                                                         
         USING BUYRECD,R2                                                       
         XC    BUYKEY,BUYKEY       BUILD BUY KEY                                
         MVC   BUYKAM,BAGYMD       AGENCY-MEDIA                                 
         MVC   BUYKCLT,BCLT        CLIENT                                       
         MVC   BUYKPRD,BPRD        PRODUCT                                      
         TM    LIND,LPOL           TEST POOL BUY                                
         BZ    *+8                                                              
         MVI   BUYKPRD,FF          YES - SET PRODUCT IN KEY TO POOL             
         MVC   BUYMSTA(2),BMKT     MARKET                                       
         OC    LOVRMKT,LOVRMKT     TEST MARKET OVERRIDE                         
         BZ    *+10                                                             
         MVC   BUYMSTA(2),LOVRMKT                                               
         MVC   BUYMSTA+2(3),BSTA   STATION                                      
         MVC   BUYKEST,BEST        ESTIMATE                                     
*                                                                               
         GOTO1 ANXTBYLN            GET NEXT AVAILABLE LINE NUMBER               
         BNE   ADDBX                                                            
*                                                                               
         L     R2,AIOAREA3         BUILD BUY RECORD                             
         XC    BUYREC(256),BUYREC                                               
         MVC   BUYKEY,IOKEY        MOVE THE KEY                                 
         LA    R1,NDELEM-BUYREC                                                 
         STCM  R1,3,BUYRLEN        RECORD LENGTH                                
         MVC   BUYALPHA,CUAALF     ALPHA AGENCY CODE                            
         MVC   LBUYCOST,NBRSCST1                                                
         CLI   LBTRIND,BTRIEC2                                                  
         BNE   *+10                                                             
         MVC   LBUYCOST,LSVCOST2                                                
         CLI   LBTRIND,BTRIEC3                                                  
         BNE   *+10                                                             
         MVC   LBUYCOST,LSVCOST3                                                
*                                                                               
         XC    LMASPRD,LMASPRD                                                  
         BAS   RE,SETPRD           SET THE MASTER PRODUCT                       
*                                                                               
         GOTO1 ABUYDESC,LBADD      ADD BUY DESCRIPTION ELEMENT                  
         BNE   ADDBX                                                            
*                                                                               
         MVC   LBUYKSAV,IOKEY                                                   
         GOTO1 =A(CHKDARLK),APPARM,(RC),RR=APRELO    LOCKED BY DARE?            
         BE    ADDBX               YES                                          
         MVC   IOKEY,LBUYKSAV                                                   
*                                                                               
         GOTO1 ADEMADD             ADD DEMO AND UPGRADE ELEMENTS                
         BNE   ADDBX                                                            
*                                                                               
         LA    R1,SAVAREA                                                       
         USING SAVAREA,R1                                                       
         OC    SVCOST2,SVCOST2                                                  
         BNZ   ADDB00BA                                                         
         TM    LIND2,LCOS2         ELIGIBLE FOR COS2?                           
         BZ    ADDB00C              - NOPE                                      
         OC    LSVCS2,LSVCS2       WE HAVE A LOCAL COS2?                        
         BNZ   ADDB00B1             - NOPE, ADD THE ELEMENT                     
         B     ADDB00C                                                          
********                                                                        
ADDB00BA LA    RF,1                COST2 ONLY HAS ONE OF THE SINGLE             
ADDB00B0 C     RF,SVCOST2             BITS?                                     
         BNE   *+6                                                              
         DC    H'0'                DIE! DIE! DIE!                               
         SLL   RF,1                                                             
         C     RF,=X'80000000'        ARE WE DONE WITH TRAP?                    
         BNE   ADDB00B0                                                         
********                                                                        
ADDB00B1 XC    APELEM,APELEM                                                    
         LA    RE,APELEM                                                        
         USING COS2ELEM,RE                                                      
         MVI   0(RE),X'73'         NO DEFINED DSECT LABELS                      
         MVI   1(RE),COS2LENQ                                                   
         MVC   2(L'SVCOST2,RE),SVCOST2                                          
*                                                                               
         TM    LIND2,LCOS2         WE HAVE A LOCAL COS2?                        
         BZ    ADDB00B5             - NOPE, ADD THE ELEMENT                     
         MVI   0(RE),X'71'         COS2 DOLLAR, NOT FACTOR                      
*        L     R1,LACS2EL          WE'RE GONNA PULL DETAIL COS2                 
*        MVC   2(L'SVCOST2,RE),2(R1)                                            
         MVC   2(L'LSVCS2,RE),LSVCS2                                            
         DROP  R1,RE                                                            
*                                                                               
ADDB00B5 GOTO1 AADDELS,BUYREC                                                   
*                                                                               
ADDB00C  GOTO1 ACMTADD             ADD COMMENT ELEMENT                          
*                                                                               
         GOTO1 AWARNADD                                                         
*                                                                               
         GOTO1 AIDADD              OPTIONALLY ADD ID ELEMENT                    
         BNE   ADDBX                                                            
*                                                                               
         TM    LIND,LPOL           TEST FOR NON-POOL                            
         BO    ADDB1                                                            
         CLI   CMPPRD1,0           AND PIGGYBACKS                               
         BE    ADDB1                                                            
         GOTO1 APBKADD             YES - ADD PIGGYBACK ELEMENT                  
*                                                                               
ADDB1    TM    LFLAG,LPKG          TEST PACKAGE                                 
         BZ    ADDB2                                                            
         CLI   LPKGMAST,0          YES - TEST MASTER LINE SET YET               
         BE    ADDB3                                                            
         XC    APELEM,APELEM       YES - THIS MUST BE A SLAVE                   
         LA    R4,APELEM                 ADD PACKAGE ELEMENT                    
         USING PKGELEM,R4                                                       
         MVI   PKGCODE,5                                                        
         MVI   PKGLEN,4                                                         
         MVI   PKGIND,2                                                         
         MVC   PKGLINES,LPKGMAST                                                
         GOTO1 AADDELS,BUYREC                                                   
         B     ADDB3                                                            
*                                                                               
ADDB2    TM    LFLAG,LORBIT        TEST ORBIT                                   
         BZ    ADDB3                                                            
         GOTO1 AORBADD             YES - ADD ORBIT ELEMENT                      
         BNE   ADDBX                                                            
*                                                                               
ADDB3    GOTO1 =A(CALCSEDY),RR=APRELO                                           
         BAS   RE,ADDBWS           ADD BWS TRANSFER ELEMENT                     
*                                                                               
         SR    R1,R1               GET MAX N'SPOTS THAT'LL FIT                  
         ICM   R1,3,BUYRLEN                                                     
         BAS   RE,GETMAXSP                                                      
         SR    R0,R0               CALCULATE MAX SPOTS PER WEEK                 
         ZIC   RF,LNSPWKS          FOR WHEN MORE THAN 1 BUYLINE                 
         DR    R0,RF                                                            
         STC   R1,LMAXSPW                                                       
*                                                                               
ADDB4    XC    APELEM,APELEM       BUILD SPOT ELEMENTS                          
         LA    R8,APELEM                                                        
         USING REGELEM,R8                                                       
         XC    APWORK,APWORK       BUY TRANSFER ELEM FOR DETAIL RECORD          
         LA    R6,APWORK                                                        
         USING BTREL,R6                                                         
         MVI   BTRELCD,BTRELCDQ                                                 
         MVC   BTRLINE,BUYKBUY     BUY LINE                                     
         TM    LIND,LSEPLINE                                                    
         BZ    *+10                                                             
         OC    BTRIND,LBTRIND      EFFECTIVE COST INDICATOR                     
         MVC   BTRDATE,ASBDAT      TODAY'S DATE                                 
         OI    BTRIND,BTRIDATE     INDICATE ELEM HAS TRANSFER DATE              
         LA    R6,BTRSPW           BUILD SPOTS PER WEEK                         
         TM    LIND,LPOL           TEST FOR POOL ESTIMATE OPEN                  
         BZ    ADDB18                                                           
         MVI   RCODE,11            YES - POOL ORIGINAL ELEMENT                  
         CLI   CMPPRD1,0           TEST FOR PIGGYBACKS                          
         BNE   ADDB6                                                            
*                                                                               
         CLI   BPRD,FF             NO - TEST FOR CAMPAIGN PRODUCT=POL           
         BE    ADDB8                                                            
*                                                                               
         MVC   RPPRD,BPRD          NO - ALLOCATED PRD IS CAMPAIGN PRD           
         MVC   RPTIME,BDSEC             TIME SHARE IS SPOT LENGTH               
         MVI   RLEN,14                                                          
         B     ADDB9                                                            
*                                                                               
ADDB6    MVI   RLEN,18             PIGGYBACKS- SET ACTIVE AND PASSIVE           
         MVC   RPPRD,CMPPRD1                   PRODUCTS AND TIME SHARES         
         MVC   RPTIME,CMPLEN1                                                   
         MVC   RPPRD+L'RPALLOC(1),CMPPRD2                                       
         MVC   RPTIME+L'RPALLOC(1),CMPLEN2                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         IC    RE,CMPLEN1                                                       
         IC    RF,CMPLEN2                                                       
         AR    RE,RF                                                            
*                                                                               
         CLM   RE,1,BDSEC          LENGTHS ADD UP?                              
         BE    ADDB6A10            YES                                          
*                                                                               
         TM    BDSEC,X'01'         ODD # OF SECONDS?                            
         BNZ   ADDB98              YES, CAN'T SPLIT EVENLY                      
         CLC   CMPLEN1,CMPLEN2     EVEN SPLIT OF SECONDS?                       
         BNE   ADDB98              NO, THEN WE CAN'T SPLIT                      
         ZIC   R1,BDSEC            SPLIT EVENLY IN HALF                         
         SRL   R1,1                                                             
         STC   R1,RPTIME                                                        
         STC   R1,RPTIME+L'RPALLOC                                              
*                                                                               
ADDB6A10 TM    CMPIND,CMPIFR1      TEST FREE RIDER - BRAND 1 PAYS ALL           
         BZ    *+8                                                              
         OI    RSTATUS,RSB1PALQ    X'08' - BRAND 1 PAYS ALL (PIGGYBACK)         
         B     ADDB9                                                            
***************                                                                 
* CAMPAIGN'S PRODUCT IS POL                                                     
***************                                                                 
ADDB8    MVI   RLEN,10             CAMPAIGN PRODUCT = POOL                      
*                                                                               
         CLI   INOPRD,0            ANY PRODUCT OPTION?                          
         BE    ADDB9               NO, NONE                                     
*                                                                               
         MVI   RLEN,14             YES, ASSUME SINGLE PRODUCT FIRST             
         MVC   RPPRD,INOPRD                                                     
         MVC   RPTIME,BDSEC                                                     
*                                                                               
         CLI   INOPRD+1,0                TEST FOR PIGGYBACK                     
         BE    ADDB9                                                            
         MVI   RLEN,18                                                          
         MVC   RPTIME,INOPRD+2           YES - SET THE TIME SHARES              
         MVC   RPPRD+L'RPALLOC(1),INOPRD+1     AND SECONDARY PRODUCT            
         MVC   RPTIME+L'RPALLOC(1),INOPRD+3                                     
*                                                                               
         ZIC   RE,INOPRD+2         CALCULATE TOTAL TIME IN OPTIONS              
         ZIC   RF,INOPRD+3                                                      
         AR    RE,RF                                                            
         CLM   RE,1,BDSEC                                                       
         BE    ADDB9                                                            
*                                                                               
         TM    BDSEC,X'01'         ODD # OF SECONDS?                            
         BNZ   ADDB98              YES, CAN'T SPLIT EVENLY                      
         CLC   INOPRD+2(1),INOPRD+3   EVEN SPLIT OF SECONDS?                    
         BNE   ADDB98                                                           
         ZIC   R1,BDSEC            SPLIT EVENLY IN HALF                         
         SRL   R1,1                                                             
         STC   R1,RPTIME                                                        
         STC   R1,RPTIME+L'RPALLOC                                              
*                                                                               
ADDB9    LA    R4,LWKTAB                                                        
         LA    R0,53                                                            
         MVC   LNSPTS,LMAXSPTS+3                                                
         CLC   LSPTOT,LMAXSPTS     TEST MORE THAN MAX SPOTS/RECORD              
         BNH   ADDB10                                                           
         MVC   LNSPTS,LMAXSPW      YES - RESTRICT TO MAX SPOTS/WEEK             
*                                                                               
ADDB10   CLI   0(R4),FF            TEST END OF TABLE                            
         BE    ADDB16                                                           
         OC    0(2,R4),0(R4)       TEST ANY SPOTS THIS WEEK                     
         BZ    ADDB14                                                           
         LA    R9,SAVAREA                                                       
         USING SAVAREA,R9                                                       
         CLC   2(2,R4),SVELKSDT                                                 
         BL    ADDB11                                                           
         CLC   2(2,R4),SVELKNDT                                                 
         BH    ADDB11                                                           
         DROP  R9                                                               
*                                                                               
LCKDSPTS LA    R2,BWSMSGH                                                       
         XC    8(L'BWSMSG,R2),8(R2)                                             
         MVC   8(L'SPTSLCKD,R2),SPTSLCKD                                        
         OI    6(R2),X'80'                                                      
         LA    R2,BWSKEYH                                                       
         DC    H'0'                                                             
         DC    C'$ABEND'                                                        
SPTSLCKD DC    C'CANNOT TRANSFER, SPOTS IN LOCKED PERIOD!'                      
*                                                                               
ADDB11   TM    LIND,LSEPDLY        SEPARATE DAILY?                              
         BZ    ADDB11A              - NOPE, NO NEED FOR CLEAN UP                
         L     RF,AEXTRAWK                                                      
         USING EXTRAWKD,RF                                                      
         XC    OVRDELEM,OVRDELEM                                                
         DROP  RF                                                               
         NI    LIND2,X'FF'-LOVRDONE                                             
*                                                                               
ADDB11A  MVC   RDATE,2(R4)         BUY DATE                                     
         NI    RSTATUS,FF-X'20'                                                 
         CLC   LBTRIND,4(R4)                                                    
         BE    ADDB11C                                                          
         MVC   RPCOST,5(R4)        NO-SPOT COST                                 
         TM    4(R4),X'C0'         TEST FOR COST OVERRIDE                       
         BZ    ADDB11C                                                          
         OI    RSTATUS,X'20'       RATE OVERRIDE                                
*                                                                               
ADDB11C  ZIC   R9,LNSPTS           SET NUMBER OF SPOTS WE'LL PROCESS            
         CLC   LNSPTS,1(R4)        THIS WEEK                                    
         BNH   *+8                                                              
         IC    R9,1(R4)                                                         
         STC   R9,0(R6)            STORE IN BUY TRANSFER ELEMENT                
*                                                                               
ADDB11E  TM    LIND,LSEPDLY        WE DOING DAILY??                             
         BNO   ADDB12               - NOPE, NOT THE CASE                        
         TM    LIND2,LOVRDONE      WE DONE WITH COST OVERRIDES?                 
         BO    ADDB12               - YUP                                       
         GOTO1 OVRDFIX             FOR LSEPDLY ONLY!!                           
         BNE   ADDBX                                                            
*                                                                               
ADDB12   GOTO1 AADDELS,BUYREC                                                   
         BNE   ADDBX                                                            
ADDB12E  L     RE,LSPTOT           DECREMENT TOTAL SPOTS LEFT                   
         BCTR  RE,0                                                             
         ST    RE,LSPTOT                                                        
         LH    RE,0(R4)            DECREMENT SPOTS LEFT THIS WEEK               
         BCTR  RE,0                                                             
         STH   RE,0(R4)                                                         
         BCT   R9,ADDB11E                                                       
*                                                                               
ADDB14   LA    R4,8(R4)            NEXT WEEK                                    
         LA    R6,1(R6)                                                         
         BCT   R0,ADDB10           DO FOR ALL WEEKS                             
*                                                                               
ADDB16   B     ADDB24              NOW WRITE THE BUY RECORD                     
*                                                                               
ADDB18   MVI   RCODE,6             NON-POOL SPOTS                               
         MVI   RLEN,10                                                          
         CLI   CMPPRD1,0           TEST FOR PIGGYBACKS                          
         BE    *+8                                                              
         MVI   RLEN,12             YES - EXPAND ELEMENT                         
         LA    R4,LWKTAB                                                        
         LA    R0,53                                                            
*                                                                               
ADDB20   CLI   0(R4),FF            TEST END OF SCHEDULE                         
         BE    ADDB24                                                           
         OC    0(2,R4),0(R4)       TEST ANY SPOTS THIS WEEK                     
         BZ    ADDB22                                                           
         LA    R9,SAVAREA                                                       
         USING SAVAREA,R9                                                       
         CLC   2(2,R4),SVELKSDT                                                 
         BL    ADDB21                                                           
         CLC   2(2,R4),SVELKNDT                                                 
         BNH   LCKDSPTS                                                         
         DROP  R9                                                               
*                                                                               
ADDB21   MVC   RDATE,2(R4)         BUY DATE                                     
         MVC   RNUM,1(R4)          NUMBER OF SPOTS                              
         MVC   0(1,R6),1(R4)       STORE IN BUY TRANSFER ELEMENT                
         GOTO1 AADDELS,BUYREC      ADD BUY ELEMENT                              
         BNE   ADDBX                                                            
*                                                                               
ADDB22   LA    R4,8(R4)            NEXT WEEK                                    
         LA    R6,1(R6)                                                         
         BCT   R0,ADDB20           DO FOR ALL WEEKS                             
*                                                                               
*                                                                               
ADDB24   MVI   APELEM,X'6A'        DELETE EXISTING VAT ELEMENT                  
         GOTO1 ADELELS,BUYREC                                                   
         GOTO1 AVATADD             ADD VAT ELEMENT                              
         BNE   ADDBX                                                            
         OC    LPST,LPST           TEST FOR PST CODES                           
         BZ    ADDB25                                                           
         GOTO1 APSTADD             YES-ADD PST ELEMENT                          
         XC    BDNTAX,BDNTAX       CLEAR OUT THE TAX IF WE HAVE PST             
*                                                                               
ADDB25   OC    CLTMTCLT,CLTMTCLT   TEST MASTER TRAFFIC CLIENT                   
         BZ    ADDB26                                                           
         GOTO1 AMCLTADD            YES-ADD MASTER CLIENT ELEMENT                
*                                                                               
ADDB26   DS    0H                                                               
*                                                                               
ADDB27   DS    0H                                                               
*                                                                               
***  WE'RE CALLING SPBUYVAL HERE  - APWORK GETS WIPED OUT                       
         MVC   APFULL,AIOAREA3                                                  
         BRAS  RE,VALBUY                                                        
         BNE   ADDB40              WE'RE DONE WITH THIS BUYLINE                 
***  WE'RE CALLING SPBUYVAL HERE                                                
*                                                                               
         MVC   IOADDR,AIOAREA3     ADD BUY RECORD                               
         GOTO1 AADDREC                                                          
*&&DO                                                                           
         L     R1,AIOAREA1                                                      
         MVC   0(L'APELEM,R1),APELEM    SAVE BUY ELEMENT                        
         LA    RE,APWORK                                                        
         SR    R6,RE                                                            
         STC   R6,APWORK+BTRELLN-BTREL    BUY TRANSFER ELEM LENGTH              
         BCTR  R6,0                                                             
         MVC   APELEM(0),APWORK                                                 
         EX    R6,*-6                                                           
         TM    LIND,LSEPDLY        TEST SEPARATE DAILY TRANSFER                 
         BZ    *+8                                                              
         BAS   RE,BLDDTR           YES-BUILD DAILY TRANSFER ELEMENT             
         GOTO1 AADDELS,BWDRECD     ADD BUY TRANSFER ELEMENT                     
         L     R1,AIOAREA1                                                      
         MVC   APELEM,0(R1)        RESTORE BUY ELEMENT                          
*&&                                                                             
         AP    LTOTBYLN,=P'1'      AUGMENT TOTAL BUY LINES ADDED                
*                                                                               
         L     RE,AEXTRAWK                                                      
         USING EXTRAWKD,RE                                                      
         TM    BTCHFLG1,BFL1DSNT   HAVE AN ORDER THAT WAS EVER SENT?            
         BNZ   ADDB40              YES, THEN NO BATCH ORDER HERE                
         DROP  RE                                                               
         BAS   RE,DARBTCHR         SET UP DARE BATCH RECORD                     
         MVC   IOADDR,AIOAREA3                                                  
*                                                                               
ADDB40   TM    LIND,LPOL           TEST POOL BUY                                
         BZ    ADDBX               NO-DONE                                      
         OC    LSPTOT,LSPTOT       TEST ANY SPOTS LEFT                          
         BZ    ADDBX               NO-DONE                                      
         CLI   BUYKBUY,FF          NEXT BUYLINE                                 
         BL    *+6                                                              
         DC    H'0'                                                             
         ZIC   RE,BUYKBUY          AUGMENT BUY LINE NUMBER                      
         LA    RE,1(RE)                                                         
         CHI   RE,255              CHECK NO MORE THAN 255                       
         BH    ADDB99                                                           
         STC   RE,BUYKBUY                                                       
         OC    QSTA,QSTA           TEST FOR STATION FILTER                      
         BNZ   ADDB50                                                           
         L     R8,LASTABYE         NO - AUGMENT LAST BUY LINE NUM               
         USING STABYD,R8                IN STATION BUYLINE TABLE                
         AP    SBLNHI,=P'1'                                                     
         B     ADDB50+6                                                         
*                                                                               
ADDB50   AP    LBYLNHI,=P'1'                                                    
         MVI   APELEM,11           DELETE ALL BUY ELEMENTS                      
         GOTO1 ADELELS,BUYREC                                                   
         B     ADDB4               PROCESS REMAINING SPOTS                      
*                                                                               
ADDB98   MVC   FVMSGNO,=AL2(FVIPIG)  INVALID PIGGYBACK SLNS                     
         OI    FVERRIND,FVEUNWND                                                
         B     ADDBX                                                            
*                                                                               
ADDB99   MVC   FVMSGNO,=AL2(FVMAXLIN)  TOO MANY LINES ON STA/EST                
         OI    FVERRIND,FVEUNWND                                                
*                                                                               
ADDBX    CLC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
         DROP  R3,R6                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************                                             
* ADDS/UPDATES THE DARE BATCH RECORD                                            
*                                                                               
* ON ENTRY:    AIOAREA3            A(BUY RECORD)                                
***********************************                                             
DARBTCHR NTR1                                                                   
         CLI   QMED,C'T'           BATCH RECORDS ONLY FOR TV & RADIO            
         BE    *+12                                                             
         CLI   QMED,C'R'                                                        
         BNE   DBTCHX                                                           
*                                                                               
         XC    IOKEY,IOKEY         READ DARE PROFILE                            
         MVC   IOKEY(4),=C'sDAR'  <== Need the lowercase s                      
         MVC   IOKEY+4(2),CUAALF                                                
         MVC   IOKEY+6(1),QMED                                                  
         MVC   IOKEY+7(3),QCLT                                                  
         GOTO1 VGETPROF,APPARM,IOKEY,APWORK,VDMGR                               
         OC    APWORK(16),APWORK                                                
         BZ    DBTCHX                                                           
         CLI   APWORK+4,C'Y'       USING DARE BATCH ORDERING?                   
         BNE   DBTCHX              NO, NOTHING TO DO HERE                       
*                                                                               
         XC    IOKEY,IOKEY         SEE IF WE HAVE A BATCH ORDER ALREADY         
         LA    R4,IOKEY                                                         
         USING DBTKEY,R4                                                        
         MVI   DBTKTYP,DBTKTYPQ                                                 
         MVI   DBTKSTYP,DBTKSTYQ                                                
         L     R2,AIOAREA3                                                      
         USING BUYKEY,R2                                                        
         MVC   DBTKAGMD,BUYKAM                                                  
         MVC   DBTKMKT(L'BUYMSTA),BUYMSTA                                       
***                                                                             
         CLI   DBTKSTA,X'E8'       IS THIS FOR A CABLE STATION?                 
         BL    *+8                                                              
         NI    DBTKSTA+2,X'80'     YES, WE ONLY WANT SYSCODE LEVEL              
***                                                                             
         MVC   DBTKCLT,BUYKCLT                                                  
         MVC   DBTKEST,BUYKEST                                                  
         CLI   BUYKPRD,X'FF'       POL BUY?                                     
         BNE   DBTCH02                                                          
         MVC   DBTKPRD,BDMASPRD    YES, PRODUCT CODES STORED HERE               
         MVC   DBTKPRD2,BDMASPRD+1                                              
         B     DBTCH08                                                          
*                                                                               
DBTCH02  MVC   DBTKPRD,BUYKPRD     NON-POL, PRODUCT CODES STORED HERE           
         L     RE,AIOAREA3         RE = A(BUY RECORD)                           
         AHI   RE,BDELEM-BUYKEY                                                 
         XR    RF,RF                                                            
DBTCH04  CLI   0(RE),0                                                          
         BE    DBTCH08                                                          
         CLI   0(RE),X'04'         LOOK FOR PIGGYBACK ELEM                      
         BE    DBTCH06                                                          
         IC    RF,1(RE)                                                         
         AR    RE,RF                                                            
         B     DBTCH04                                                          
*                                                                               
DBTCH06  MVC   DBTKPRD2,PBPROD-PBELEM(RE)  GOT A PIGGYBACK PRODUCT CODE         
*                                                                               
DBTCH08  CLI   DBTKPRD,X'00'       IF NO PRODUCT ALLOCATED                      
         BNE   *+8                                                              
         MVI   DBTKPRD,X'FF'       THEN POL                                     
         DROP  R2,R4                                                            
*                                                                               
         GOTO1 AIO,DIRHI+IO1                                                    
         BNE   DBTCH10                                                          
         CLC   IOKEY(L'DBTKEY),IOKEYSAV                                         
         BE    DBTCHX                                                           
*                                                                               
DBTCH10  L     RE,AIOAREA1                                                      
         LA    RF,2000                                                          
         XCEFL                                                                  
*                                                                               
         XC    IOKEY,IOKEY         SEE IF WE HAVE A DARE ORDER                  
         LA    R4,IOKEY                                                         
         USING DBTKEY,R4                                                        
         MVC   IOKEY,IOKEYSAV                                                   
*                                                                               
         L     R4,AIOAREA1                                                      
         USING DBTKEY,R4                                                        
         L     R2,AIOAREA3                                                      
         USING BUYKEY,R2                                                        
         MVC   DBTKEY,IOKEYSAV                                                  
*                                                                               
         MVC   DBTRLEN,=AL2(DBTRFRST-DBTKEY)                                    
*                                                                               
         XC    APELEM,APELEM       SETUP AN INFO ELEMENT SO WE KNOW             
         LA    RE,APELEM               WHEN WE CREATED THIS BATCH REC           
         USING DBINFELD,RE                                                      
         MVI   DBINFEL,DBINFELQ                                                 
         MVI   DBINFLEN,DBINFLNQ                                                
         LA    R1,DBINFDTC              PWOS JULIAN                             
         ST    R1,APPARM+4                                                      
         MVI   APPARM+4,19                                                      
         DROP  RE                                                               
         GOTO1 VDATCON,APPARM,(5,0),,0  FOR TODAY                               
         GOTO1 AADDELS,DBTKEY                                                   
*                                                                               
         GOTO1 AIO,FILADD1         YES, ADD IT AS WE HAVE IT NOW                
*                                                                               
DBTCHX   B     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* THIS GETS CALLED WITHIN NEW LOOP IF IT IS SEPARATE DAILY            *         
*      IT ADDS COST OVERRIDES TO SPOTS IF THEY SHOULD HAVE IT         *         
***********************************************************************         
OVRDFIX  NTR1  BASE=*,LABEL=*                                                   
         L     RF,AEXTRAWK                                                      
         USING EXTRAWKD,RF                                                      
         OC    OVRDELEM,OVRDELEM   DO WE HAVE ANYTHING HERE?                    
         BZ    OVRDFX20             - FIRST TIME THROUGH                        
         L     R2,OVRDELEM                                                      
         B     OVRDFX40            NEED TO BUMP IT TO NEXT FIRST!!              
*                                                                               
OVRDFX20 L     R1,AIOAREA2                                                      
         USING NBRRECD,R1                                                       
         LA    R2,NBRFSTEL         WE'RE LOOKING FOR THE FIRST X'31'            
*                                                                               
OVRDFX30 CLI   0(R2),0             WE'RE DONE?                                  
         BE    OVRDDONE             - YUP                                       
         CLI   0(R2),X'31'         COST OVERRIDE ELEMENT?                       
         BE    OVRDFX50             - FOUND IT                                  
*                                                                               
OVRDFX40 XR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     OVRDFX30                                                         
*                                                                               
         USING NBRCOELD,R2                                                      
OVRDFX50 CLC   NBRCODAT,LDTRDATE   THE DATE WE'RE ON?                           
         BNE   OVRDFX40             - NOPE, BUMP THIS ELEM                      
         LA    R8,APELEM                                                        
         USING REGELEM,R8                                                       
         OI    RSTATUS,X'20'       WE HAVE A COST OVERRIDE                      
         MVC   RPCOST,NBRCOCST+1   RPOST (3 BYTES) NBRCOCST (4 BYTES)           
         ST    R2,OVRDELEM         SAVE CURRENT POSITION OF R2 (X'31')          
         B     OVRDFXYS                                                         
*                                                                               
OVRDDONE OI    LIND2,LOVRDONE      WE'RE DONE WITH OVERRIDES THIS DATE          
         NI    RSTATUS,X'FF'-X'20'                                              
         XC    RPCOST,RPCOST       EXTRA FIELDS                                 
*                                                                               
OVRDFXYS CR    RB,RB               EQ                                           
OVRDFXX  J     EXIT                                                             
         DROP  R1,R2,RF                                                         
         SPACE 1                                                                
***********************************************************************         
* CALL SPAUTH TO UPDATE SUPERDESK AUTHORIZATION RECORDS               *         
***********************************************************************         
         SPACE 1                                                                
CHKAUTH  NTR1  BASE=*,LABEL=*                                                   
         L     RF,AEXTRAWK                                                      
         USING EXTRAWKD,RF                                                      
         TM    XTRAFLG1,XAUTH      ALREADY WENT THROUGH CODE?                   
         BNZ   CHKAUTHZ            YES, GET OUT                                 
         TM    XTRAFLG1,XSDE       SDESK AUTH OPEN FOR PRD OPTION?              
         BO    CKAUTH10            YES                                          
         DROP  RF                                                               
*                                                                               
         L     RF,ATWA                                                          
         AHI   RF,SVINDS-TWAD                                                   
         TM    0(RF),SVIEAUTH      SDESK AUTH OPEN FOR CAMPAIGN PRD?            
         BNO   CHKAUTHZ            NO, GET OUT                                  
*                                                                               
CKAUTH10 MVC   APHALF+0(1),BPRD                                                 
         MVC   APHALF+1(1),CMPPRD2                                              
         BRAS  RE,SETPRD                                                        
         CLI   APHALF+1,0          IS THERE A PIGGYBACK PRODUCT?                
         BE    CKAUTH50            NO                                           
         TM    CLTIND2,CLTPONLY    POL ONLY?                                    
         BZ    CKAUTH20                                                         
         CLI   CLTPROF+0,C'0'      TRUE POL?                                    
         BE    CKAUTH50            IF TRUE POL DON'T READ EST RECORD            
*                                                                               
CKAUTH20 MVC   APBYTE,APHALF+1                                                  
         BRAS  RE,GETPIG           GET 3 BYTE PRODUCT MNEMONIC                  
*                                                                               
         USING ESTHDRD,R2                                                       
         LA    R2,IOKEY            READ ESTIMATE RECORD FOR PIGGYBACK           
         XC    EKEY,EKEY                                                        
         MVC   EKEYAM,BAGYMD                                                    
         MVC   EKEYCLT,BCLT                                                     
         MVC   EKEYPRD,APFULL      PIGGYBACK PRODUCT                            
         MVC   EKEYEST,BEST                                                     
*                                                                               
         GOTO1 AIO,DIRHI+IO1                                                    
         BNE   *+14                                                             
         CLC   EKEY,IOKEYSAV                                                    
         BE    *+6                                                              
         DC    H'00'                                                            
         GOTO1 AIO,FILGET1         GET ESTIMATE HDR                             
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIOAREA1                                                      
         TM    EFLAG1,EF1SDE       PIGGYBACK AUTHORIZATION OPEN?                
         BZ    CHKAUTHZ            NO, GET OUT                                  
         DROP  R2                                                               
*                                                                               
* NEED TO ALPHABETIZE PRD1 AND PRD2                                             
         CLI   INOPRD,0            PRD CODE HERE?                               
         BE    CKAUTH30            NO, CHECK CAMPAIGN                           
         CLC   POLPRD1,POLPRD2     ALREADY ALPHABETIZED?                        
         BH    CKAUTH40            NO                                           
         B     CKAUTH50            YES                                          
*                                                                               
CKAUTH30 CLC   CMPPRDC,APFULL      ALREADY ALPHABETIZED?                        
         BNH   CKAUTH50            YES                                          
*                                                                               
CKAUTH40 MVC   APBYTE,APHALF       SWAP THEM                                    
         MVC   APHALF(1),APHALF+1                                               
         MVC   APHALF+1(1),APBYTE                                               
*                                                                               
         USING BWHRECD,R2                                                       
CKAUTH50 XC    APFULL,APFULL                                                    
         LA    R2,IOKEY            READ CAMPAIGN MARKET HEADER REC              
         XC    IOKEY,IOKEY                                                      
         MVI   BWHKTYP,BWHKTYPQ                                                 
         MVI   BWHKSUB,BWHKSUBQ                                                 
         MVC   BWHKAGMD,BAGYMD                                                  
         OC    BWHKAGMD,BBYRMASK                                                
         MVC   BWHKBYR,BBYR                                                     
         MVC   BWHKCAM,BCAM                                                     
         MVC   BWHKMKT,BMKT                                                     
         GOTO1 AIO,DIRHI+IO1                                                    
         BNE   *+14                                                             
         CLC   BWHKEY(BWHKSEQ-BWHKEY),IOKEYSAV                                  
         BE    *+6                                                              
         DC    H'00'                                                            
         GOTO1 AIO,FILGET1         GET ESTIMATE HDR                             
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIOAREA1                                                      
         LA    R2,BWHEL            FIRST ELEMENT                                
         USING INFELD,R2                                                        
CKAUTH60 CLI   0(R2),0                                                          
         BE    CKAUTH80                                                         
         CLI   0(R2),INFELCDQ                                                   
         BE    CKAUTH70                                                         
         ZIC   R1,1(R2)                                                         
         AR    R2,R1                                                            
         B     CKAUTH60                                                         
CKAUTH70 MVC   APFULL(3),INFADDED                                               
         DROP  R2                                                               
*                                                                               
         PUSH  USING                                                            
CKAUTH80 XC    APWORK,APWORK       CALL SPAUTH                                  
*                                                                               
         USING SPAUTHD,APWORK                                                   
         MVC   SPACOM,ACOM                                                      
         L     RF,AIOAREA1                                                      
         ST    RF,SPAIO                                                         
         MVC   SPAKAM,BAGYMD                                                    
         MVC   SPAKCLT,BCLT                                                     
         MVC   SPAKPRD(2),APHALF                                                
         MVC   SPAKEST,BEST                                                     
         MVC   SPAKMKT,BMKT                                                     
         MVC   SPAKSTA,BSTA                                                     
         MVI   SPAUPDT,SPAUPXFR    UPDATE NWS TRANSFER DATE                     
         GOTO1 VDATCON,APPARM,(3,CMPSTDT),(2,SPASDTE)  START DATE               
         GOTO1 (RF),(R1),(3,CMPND),(2,SPAEDTE)         END DATE                 
         GOTO1 (RF),(R1),(5,0),(2,SPANWSDT)            NWS XFER DATE            
         OC    APFULL,APFULL                           HAVE WORK ADD DT         
         BZ    CKAUTH90                                NO                       
         GOTO1 (RF),(R1),(3,APFULL),(2,SPAWRKDT)       WORK REC ADD DT          
CKAUTH90 GOTO1 VSPAUTH,APWORK                                                   
         CLI   SPAERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         POP   USING                                                            
*                                                                               
CHKAUTHX L     RF,AEXTRAWK                                                      
         USING EXTRAWKD,RF                                                      
         OI    XTRAFLG1,XAUTH      TURN FLAG ON                                 
         DROP  RF                                                               
CHKAUTHZ J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* EXTENTION ROUTINES                                                  *         
***********************************************************************         
         SPACE 1                                                                
EXTRA    NMOD1 0,**B37X**,RA                                                    
         L     RC,APALOCAL                                                      
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         SRL   RF,24                                                            
         SLL   RF,2                                                             
         B     *+4(RF)                                                          
*                                                                               
         B     VALDT                                                            
         B     BLDWKS                                                           
         B     NXTBUYLN                                                         
         B     BUYCHG                                                           
         B     SPOTCHG                                                          
         B     POOLSPT                                                          
         B     NPOOLSPT                                                         
         B     ADDREQ                                                           
         B     CMTADD                                                           
         B     IDADD                                                            
         B     PBKADD                                                           
         B     ORBADD                                                           
         B     WARNADD                                                          
         B     VATADD                                                           
         B     PSTADD                                                           
         B     CHKID                                                            
         B     BYRNAME                                                          
         B     ADDS                                                             
         B     MCLTADD                                                          
         B     FIXDAILY                                                         
         B     FIXCOST                                                          
         B     PUTBUY                                                           
         B     ADDREC                                                           
*                                                                               
EQXIT    CR    RE,RE                                                            
         B     XIT                                                              
*                                                                               
NEQXIT   LTR   RB,RB                                                            
         B     XIT                                                              
*                                                                               
EXIT1    CLC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
XIT      XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* VALIDATE DATES OPTION AGAINST THE CAMPAIGN DATES                    *         
* OUTPUT : CC EQ - LDTINDS SET WITH WEEKS THAT ARE EXCLUDED           *         
*          CC NE - INVALID DATES                                      *         
***********************************************************************         
         SPACE 1                                                                
VALDT    CLI   INFSTART,0          TEST START YEAR GIVEN                        
         BNE   VALDT1                                                           
         ZIC   RE,CMPSTDT          NO-USE CAMPAIGN START YEAR                   
         STC   RE,INFSTART                                                      
         CLC   INFSTART+1(2),CMPSTDT+1 TEST M/D BEFORE CAMPAIGN START           
         BNL   VALDT1                                                           
         LA    RE,1(RE)            YES-USE NEXT YEAR                            
         STC   RE,INFSTART                                                      
*                                                                               
VALDT1   CLI   INFEND,0            TEST END YEAR GIVEN                          
         BNE   VALDT2                                                           
         ZIC   RE,CMPND            NO-USE CAMPAIGN END YEAR                     
         STC   RE,INFEND                                                        
         CLC   INFEND+1(2),CMPND+1     TEST M/D AFTER CAMPAIGN END              
         BNH   VALDT2                                                           
         BCTR  RE,0                YES-USE PREVIOUS YEAR                        
         STC   RE,INFEND                                                        
*                                                                               
VALDT2   CLC   INFSTART,INFEND     CHECK END NOT BEFORE START                   
         BH    VALDT9                                                           
         CLC   INFSTART,CMPSTDT    CHECK DATES ARE WITHIN CAMPAIGN              
         BL    VALDT8                                                           
         CLC   INFEND,CMPND                                                     
         BH    VALDT8                                                           
         GOTO1 VDATCON,APPARM,(3,INFSTART),(2,APFULL)                           
         GOTO1 (RF),(R1),(3,INFEND),(2,APFULL+2)                                
         LA    R2,LDTINDS                                                       
         LR    R3,R5               SET WEEKS THAT ARE EXCLUDED                  
         AHI   R3,CMPDATSP-TWAD                                                 
         MVI   APBYTE,0            FROM TRANSFER                                
         ZIC   R0,CMPNWKS                                                       
*                                                                               
VALDT3   CLI   APBYTE,2            TEST BEYOND END DATE                         
         BE    VALDT4                                                           
         CLI   APBYTE,0            TEST BEFORE START DATE                       
         BNE   VALDT5                                                           
         CLC   APFULL(2),2(R3)     YES-TEST START IN THIS WEEK                  
         BH    VALDT4                                                           
         MVI   APBYTE,1            YES-                                         
         CLC   APFULL+2(2),2(R3)   TEST ALSO ENDS IN THIS WEEK                  
         BH    VALDT6                                                           
         MVI   APBYTE,2            YES-NEXT WEEK WILL BE BEYOND END             
         B     VALDT6                                                           
*                                                                               
VALDT4   OI    0(R2),LEXCLD        EXCLUDE THIS WEEK FROM TRANSFER              
         B     VALDT6                                                           
*                                                                               
VALDT5   CLC  APFULL+2(2),2(R3)    TEST END IN THIS WEEK                        
         BH   VALDT6                                                            
         MVI  APBYTE,2                                                          
*                                                                               
VALDT6   LA   R2,1(R2)             NEXT CAMPAIGN WEEK                           
         LA   R3,4(R3)                                                          
         BCT  R0,VALDT3                                                         
         B    VALDTX                                                            
*                                                                               
VALDT8   MVC   FVMSGNO,=AL2(FVIDTCAM)                                           
         B     *+10                                                             
VALDT9   MVC   FVMSGNO,=AL2(FVIDAT)                                             
         LA    R1,BWSOPTH                                                       
         ST    R1,FVADDR                                                        
*                                                                               
VALDTX   B     EXIT1                                                            
         EJECT                                                                  
***********************************************************************         
* BUILD WEEKS TABLE                                                   *         
* TABLE ENTRY FOR EACH WEEK IN CAMPAIGN PERIOD :                      *         
*    +0(1) X'FF' END-OF-TABLE                                         *         
*    +0(2) #SPOTS                                                     *         
*    +2(2) START DATE                                                 *         
*    +4(1) X'80'=COST OVERRIDE FOR EFFECTIVE DATE 2                   *         
*          X'40'=COST OVERRIDE FOR EFFECTIVE DATE 3                   *         
*    +5(3) COST OVERRIDE                                              *         
* INPUT  : R1 = LBADD WHEN BUILDING NEW BUY RECORD                    *         
*               LBCHA WHEN CHANGING EXISTING BUY RECORD               *         
* OUTPUT : LSPTOT  = TOTAL NUMBER OF SPOTS                            *         
*          LAWKEN  = A(LAST WEEK IN TABLE WITH NON-ZERO SPOTS)        *         
*          LAWKST  = A(FIRST WEEK IN TABLE WITH NON-ZERO SPOTS)       *         
*          LNSPWKS = NUMBER OF WEEKS WITH SPOTS                       *         
***********************************************************************         
         SPACE 1                                                                
         USING NBRSELD,R3                                                       
BLDWKS   STC   R1,LBADDCHA         SAVE ADD/CHANGE INDICATOR                    
         MVI   LDAYDSPL,0                                                       
         TM    LFLAG,LORBIT        TEST ORBIT                                   
         BO    BLDW2               YES - FIRST DAY = MONDAY                     
         SR    RE,RE               FIND FIRST DAY OF SCHEDULE WEEK              
         SR    RF,RF                                                            
         ICM   RF,8,NBRSDAYS                                                    
******   BNZ   *+8                 TEST PACKAGE (DAYS=0)                        
******   ICM   RF,8,BWDPODAY       YES                                          
         SLDL  RE,1                                                             
         SR    R1,R1                                                            
         ICM   R1,1,ESTOWSDY                                                    
         BZ    BLDW1                                                            
         AHI   R1,-8                                                            
         LPR   R1,R1                                                            
         SR    RF,RF                                                            
         ICM   RE,1,NBRSDAYS                                                    
******   BNZ   *+8                                                              
******   IC    RE,BWDPODAY                                                      
         SRDL  RE,1                                                             
         BCT   R1,*-4                                                           
         LR    R1,RF                                                            
         SR    RF,RF                                                            
         SRDL  RE,7                                                             
         OR    RF,R1                                                            
         SR    R1,R1                                                            
*                                                                               
BLDW1    SLDL  RE,1                                                             
         LTR   RE,RE                                                            
         BNZ   *+12                                                             
         LA    R1,1(R1)                                                         
         B     *-14                                                             
         STC   R1,LDAYDSPL         SAVE DAY DISPLACMENT (0=MO,1=TU,ETC)         
*                                                                               
BLDW2    XC    LEFDT2,LEFDT2       SET EFFECTIVE DATES                          
         XC    LEFDT3,LEFDT3                                                    
*                                                                               
         OC    NBRSEDT2,NBRSEDT2                                                
         BZ    BLDW4                                                            
         MVC   LEFDT2,LSVBDAT2                                                  
         OC    NBRSEDT3,NBRSEDT3                                                
         BZ    BLDW4                                                            
         MVC   LEFDT3,LSVBDAT3                                                  
*                                                                               
BLDW4    L     R6,LASPWEL                                                       
         USING NBRSPEL,R6                                                       
         LA    RE,LWKTAB           CHANGED TO XCEFL FOR 53 WEEKS                
         LA    RF,L'LWKTAB                                                      
         XCEFL                                                                  
*                                                                               
         LA    R2,LWKTAB                                                        
         LA    R4,LDTINDS                                                       
         LA    RE,1                SET RE,RF FOR BXLE                           
         ZIC   RF,NBRSPLEN                                                      
         AR    RF,R6                                                            
         ZIC   R1,CMPNWKS                                                       
         LA    R1,NBRSPSPW(R1)                                                  
         CR    R1,RF               TEST ELEMENT HAS MORE WEEKS THAN             
         BNL   *+6                 NUMBER OF WEEKS IN CAMPAIGN PERIOD           
         LR    RF,R1               YES-LIMIT NUMBER OF WEEKS                    
         BCTR  RF,0                                                             
         LA    R6,NBRSPSPW                                                      
         LA    R8,CMPSTMON         CAMPAIGN START MONDAY                        
         TM    CMPOPTS,CAMOWKS     OR FLIGHT START MONDAY                       
         BZ    *+8                                                              
         LA    R8,CMPFLSTM                                                      
         LA    R9,CMPSTMNP                                                      
         TM    CMPOPTS,CAMODLY     TEST DAILY SKED                              
         BZ    BLDW5                                                            
         L     R8,ATWA             YES-START WITH FIRST DAY                     
         LR    R9,R8                                                            
         AHI   R8,CMPDATSD-TWAD                                                 
         AHI   R9,CMPDATSP-TWAD                                                 
BLDW5    MVI   APBYTE,1                                                         
         MVI   APFLAG,0                                                         
         XC    LSPTOT,LSPTOT                                                    
         XC    LAWKST,LAWKST                                                    
         XC    LAWKEN,LAWKEN                                                    
         MVI   LNSPWKS,0                                                        
*                                                                               
BLDW6    TM    CMPOPTS,CAMODLY     TEST DAILY SKED                              
         BO    BLDW7               YES-DATE IS THE CURRENT DAY                  
         SR    R1,R1               NO-TEST DAYS START ON MONDAY                 
         ICM   R1,1,LDAYDSPL                                                    
         BZ    BLDW7               YES-USE MONDAY DATE                          
         ST    R1,APPARM+8         NO-CALCULATE START DATE                      
         STM   RE,RF,APDUB                                                      
         GOTO1 VADDAY,APPARM,(R8),APWORK                                        
         GOTO1 VDATCON,APPARM,(0,APWORK),(2,2(R2))                              
         LM    RE,RF,APDUB                                                      
         B     BLDW8                                                            
*                                                                               
BLDW7    MVC   2(2,R2),0(R9)                                                    
*                                                                               
BLDW8    CLI   0(R6),0             TEST FOR ANY SPOTS THIS WEEK                 
         BE    BLDW10              NO                                           
*                                                                               
         CLI   LBADDCHA,LBADD      YES-TEST ADDING NEW BUYLINE                  
         BNE   *+12                                                             
         TM    0(R4),LEXCLD        AND THIS WEEK IS EXCLUDED                    
         BO    BLDW10              YES-SKIP                                     
         CLC   2(2,R2),LESTST      CHECK DATE IS WITHIN ESTIMATE                
         BL    BLDW10              PERIOD                                       
         CLC   2(2,R2),LESTND                                                   
         BH    BLDW10                                                           
         ZIC   R1,LNSPWKS          AUGMENT NUMBER OF WEEKS WITH SPOTS           
         LA    R1,1(R1)                                                         
         STC   R1,LNSPWKS                                                       
         ST    R2,LAWKEN                                                        
         MVC   1(1,R2),0(R6)       NUMBER OF SPOTS                              
         L     R1,LSPTOT                                                        
         AH    R1,0(R2)                                                         
         ST    R1,LSPTOT           ACCUMULATE TOTAL                             
         OC    LAWKST,LAWKST                                                    
         BNZ   BLDW10                                                           
         ST    R2,LAWKST           A(FIRST ENTRY WITH NON-ZERO SPOTS)           
*                                                                               
BLDW10   OC    LEFDT2,LEFDT2       TEST FOR EFFECTIVE DATE 2                    
         BZ    BLDW14                                                           
         CLI   APBYTE,1            YES - IGNORE FOR FIRST WEEK                  
         BE    BLDW14                                                           
         CLC   LEFDT2,2(R9)        TEST EFF DATE 2 EFFECTIVE YET                
         BH    BLDW14                                                           
         MVI   APFLAG,X'80'        COST2                                        
         LA    R1,NBRSCST2                                                      
         OC    LEFDT3,LEFDT3       TEST FOR EFFECTIVE DATE 3                    
         BZ    BLDW12                                                           
         CLC   LEFDT3,2(R9)        YES - TEST EFF DATE 3 EFFECTIVE YET          
         BH    BLDW12                                                           
         MVI   APFLAG,X'40'        COST 3                                       
         LA    R1,NBRSCST3                                                      
*                                                                               
BLDW12   MVC   4(4,R2),0(R1)                                                    
         OC    4(1,R2),APFLAG      INDICATE COST OVERRIDE                       
*                                                                               
BLDW14   CLI   APBYTE,1            TEST FIRST WEEK                              
         BNE   BLDW15                                                           
         L     R8,ATWA                                                          
         LR    R9,R8                                                            
         AHI   R8,CMPDATSD-TWAD                                                 
         AHI   R9,CMPDATSP-TWAD                                                 
         MVI   APBYTE,0                                                         
BLDW15   LA    R2,8(R2)            NEXT WEEK                                    
         LA    R4,1(R4)                                                         
         LA    R8,6(R8)                                                         
         LA    R9,4(R9)                                                         
         BXLE  R6,RE,BLDW6                                                      
         MVI   0(R2),FF            MARK END OF TABLE                            
*                                                                               
BLDWX    B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* FIND NEXT AVAILABLE LINE NUMBER                                               
* INPUT  : R2 = A(IOKEY)                                                        
*        : IOKEY HAS A-M/CLT/PRD/MKT-STA/EST                                    
* OUTPUT : IOKEY HAS BUYREC KEY A-M/CLT/PRD/MKT-STA/EST/LINE-NUMBER             
***********************************************************************         
         SPACE 1                                                                
NXTBUYLN OC    QSTA,QSTA           ANY STATION FILTER?                          
         BZ    NXTB10                                                           
         CP    LBYLNLO,=P'0'       YES - TEST FOR FIRST TIME                    
         BE    NXTBHI                                                           
*                                                                               
         TM    LIND,LXFR1STP       WENT THROUGH A FIRST PASS?                   
         BNZ   NXTBHI              YES, LOOK FOR GAPS                           
         AP    LBYLNHI,=P'1'       NO, NEXT LINE NUMBER                         
*                                                                               
         CP    LBYLNHI,=P'255'     GOT A BUYLINE OVER 1 BYTE?                   
         BNH   *+12                                                             
         OI    LIND,LXFR1STP       YES, WENT THROUGH A FIRST PASS               
         B     NXTBHI              FIND NEXT AVAILABLE 1 BYTE BYLN              
*                                                                               
         ZAP   APDUB,LBYLNHI       LINE CAN'T POSSBLY EXIST IF WE               
         CVB   R1,APDUB                DIDN'T COMPLETE THE 1ST PASS             
         B     NXTB90                                                           
*                                                                               
NXTB10   L     R8,AEXTRAWK         NO STATION FILTER                            
         AHI   R8,LSTABYTB-EXTRAWKD                                             
         USING STABYD,R8                                                        
*                                                                               
NXTB20   ST    R8,LASTABYE                                                      
         OC    SBSTA,SBSTA                                                      
         BZ    NXTB30                                                           
*                                                                               
         L     R1,AIOAREA2                                                      
         USING NBRKEY,R1                                                        
         XC    APWORK,APWORK                                                    
         MVC   APWORK+2(L'NBRKSTA),NBRKSTA                                      
         DROP  R1                                                               
         GOTO1 VMSUNPK,APPARM,(X'80',APWORK),APWORK+5,APWORK+9                  
*                                                                               
         CLC   SBSTA,APWORK+9                                                   
         BE    *+12                                                             
         LA    R8,STABYL(R8)                                                    
         B     NXTB20                                                           
*                                                                               
         TM    LIND,LXFR1STP       WENT THROUGH A FIRST PASS?                   
         BNZ   NXTBHI              YES, LOOK FOR GAPS                           
         AP    SBLNHI,=P'1'        NO, NEXT LINE NUMBER                         
*                                                                               
         CP    SBLNHI,=P'255'      GOT A BUYLINE OVER 1 BYTE?                   
         BNH   *+12                                                             
         OI    LIND,LXFR1STP       YES, WENT THROUGH A FIRST PASS               
         B     NXTBHI              FIND NEXT AVAILABLE 1 BYTE BYLN              
*                                                                               
         ZAP   APDUB,SBLNHI                                                     
         CVB   R1,APDUB                                                         
         B     NXTB90                                                           
*                                                                               
NXTB30   MVC   SBSTA,APWORK+9      WAS NEVER IN TABLE SO WE DON'T HAVE          
         ZAP   SBLNLO,=P'0'            A VALUE YET FOR THIS YET                 
*                                                                               
         USING BUYRECD,R2                                                       
NXTBHI   LA    R1,DIRHIU                                                        
         B     *+8                                                              
*                                                                               
NXTBSEQ  LA    R1,DIRSQU                                                        
         GOTO1 AIO                                                              
         BNL   *+6                                                              
         DC    H'0'                BAD ERROR                                    
*                                                                               
         BE    NXTB40              NO ERRORS                                    
*                                                                               
         TM    IOERR,IOEDEL        RECORD DELETED?                              
         BO    *+6                 YES                                          
         DC    H'0'                                                             
*                                                                               
NXTB40   CLC   IOKEY(BUYKBUY+1-BUYKEY),IOKEYSAV                                 
         BNE   NXTB50                                                           
         TM    LIND,LXFR1STP       LOOKING FOR GAPS?                            
         BZ    NXTBSEQ             NO, LOOKING FOR THE LAST BUYLINE             
         ZIC   R1,IOKEY+BUYKBUY+1-BUYKEY                                        
         ZIC   R0,IOKEYSAV+BUYKBUY+1-BUYKEY                                     
         SR    R1,R0               FIND THE GAP BETWEEN THE 2 BUYLINES          
         CHI   R1,1                                                             
         BNH   NXTBSEQ             NO GAP, NEED TO FIND A GAP                   
*                                                                               
NXTB50   MVC   IOKEY,IOKEYSAV                                                   
         ZIC   R1,BUYKBUY+1                                                     
         LA    R1,1(R1)                                                         
*                                                                               
         CHI   R1,255                  MAXIMUM LINE NUMBER REACHED?             
         BNH   NXTB56                                                           
         TM    LIND,LXFR1STP           LOOKING FOR GAPS?                        
         BZ    NXTB53                                                           
         MVC   FVMSGNO,=AL2(FVMAXLN)   YES                                      
         B     NXTBX                                                            
*                                                                               
NXTB53   OI    LIND,LXFR1STP       NO, WE NEED TO LOOK FOR GAPS NOW             
         XC    IOKEY+BUYKBUY-BUYKEY(L'BUYKBUY),IOKEY+BUYKBUY-BUYKEY             
         B     NXTBHI                                                           
*                                                                               
NXTB56   CVD   R1,APDUB                                                         
         OC    QSTA,QSTA                                                        
         BNZ   NXTB60                                                           
         L     R8,LASTABYE                                                      
         CP    SBLNLO,=P'0'                                                     
         BNE   *+10                                                             
         ZAP   SBLNLO,APDUB                                                     
         ZAP   SBLNHI,APDUB                                                     
         B     NXTB90                                                           
*                                                                               
NXTB60   CP    LBYLNLO,=P'0'                                                    
         BNE   *+10                                                             
         ZAP   LBYLNLO,APDUB                                                    
         ZAP   LBYLNHI,APDUB                                                    
*                                                                               
NXTB90   STC   R1,BUYKBUY          MOVE NEXT BUY LINE NUMBER TO KEY             
         MVI   BUYKBUY+1,1                                                      
         MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
NXTBX    B     EXIT1                                                            
         EJECT                                                                  
***********************************************************************         
* COMPARE BUY REVISION RECORD TO BUY RECORD FOR CHANGES                         
* OUTPUT : LCHGIND = CHANGE INDICATORS                                          
***********************************************************************         
         SPACE 1                                                                
BUYCHG   L     R2,AIOAREA3                                                      
         USING BUYRECD,R2                                                       
         L     R3,AIOAREA2                                                      
         USING NBRKEY,R3                                                        
         LA    R3,NBRFSTEL                                                      
         SR    R0,R0                                                            
BCHG1    CLI   0(R3),0                                                          
         BNE   *+6                                                              
         DC    H'0'                SHOULDN'T BECAUSE WE CHECKED ABOVE           
         CLI   0(R3),NBRSELQ                                                    
         BE    BCHG1A                                                           
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     BCHG1                                                            
*                                                                               
         USING NBRSELD,R3                                                       
BCHG1A   CLC   NBRSSLN,BDSEC       SPOT LENGTH CHANGE?                          
         BE    *+8                                                              
         OI    LCHGIND,LSLN        YES                                          
*                                                                               
****     TM    LFLAG,LORBIT        TEST ORBIT                                   
****     BO    BCHG4               YES - DAYS=0 ANYHOW                          
         MVC   APBYTE,NBRSDAYS                                                  
****     CLI   NBRSDAYS,0          TEST PACKAGE                                 
****     BNE   *+10                                                             
****     MVC   APBYTE,BWDPODAY     YES-USE PACKAGE DAYS                         
         CLC   APBYTE,BDDAY                                                     
         BE    BCHG4                                                            
*                                                                               
         OI    LCHGIND,LDAYS       DAYS CHANGE                                  
         GOTO1 VDAYUNPK,APPARM,APBYTE,(7,APWORK)                                
         GOTO1 VDAYUNPK,APPARM,BDDAY,(7,APWORK+7)                               
         LA    RE,APWORK                                                        
         LA    R0,7                                                             
*                                                                               
BCHG2    CLC   0(1,RE),7(RE)                                                    
         BE    *+12                                                             
         OI    LCHGIND,LSTDATE     START DAY CHANGE                             
         B     BCHG4                                                            
         CLI   0(RE),C'.'                                                       
         BNE   BCHG4                                                            
         LA    RE,1(RE)                                                         
         BCT   R0,BCHG2                                                         
*                                                                               
BCHG4    CLC   NBRSTIMS(L'NBRSTIMS*2),BDTIMST   TIMES CHANGED?                  
         BE    BCHG6                                                            
         OI    LCHGIND,LTIMES                   YES                             
         CLC   NBRSTIMS,BDTIMST    TIME EXPANSION?                              
         BH    *+14                                                             
         CLC   NBRSTIME,BDTIMEND                                                
         BNL   BCHG6                                                            
         OI    LCHGIND,LTIMNOTX    TIMES CHANGE - NOT EXPANSION                 
*                                                                               
BCHG6    CLI   CLTBWPRO+6,C'N'     TEST COST CHANGES ALLOWED                    
         BE    BCHG10                                                           
         SR    RF,RF                                                            
         ICM   RF,7,BDCOST                                                      
         TM    BDCIND2,X'20'       CANADIAN BUY?                                
         BNZ   BCHG7                                                            
         TM    BDCIND2,X'10'       US BUY IN DOLLARS?                           
         BZ    BCHG7                                                            
         MHI   RF,100              YES CONVERT TO PENNIES                       
BCHG7    TM    BDCIND,X'01'        MINUS?                                       
         BZ    *+6                                                              
         LNR   RF,RF               YES, NEGATE THE AMOUNT                       
*                                                                               
         CLM   RF,15,NBRSCST1                                                   
         BE    *+8                                                              
         OI    LCHGIND,LCST        COST CHANGE                                  
*&&DO                                                                           
         TM    LIND,LSEPLINE       EFFECTIVE COST CHANGES ONLY VALID            
         BO    BCHG10              IF NOT GOING TO SEPARATE BUYLINES            
         CLC   BWDCOST2,BWDTREC2                                                
         BNE   BCHG8                                                            
         CLC   BWDEFDT2,BWDTRED2                                                
         BNE   BCHG8                                                            
         CLC   BWDCOST3,BWDTREC3                                                
         BNE   BCHG8                                                            
         CLC   BWDEFDT3,BWDTRED3                                                
         BE    BCHG10                                                           
*                                                                               
BCHG8    OI    LCHGIND,LEFFCOST    EFFECTIVE DATE/COST CHANGE                   
*&&                                                                             
BCHG10   TM    LIND,LPOL           TEST POL BUY                                 
         BZ    BCHGX                                                            
         CLC   BDMASPRD,LMASPRD    YES-TEST MASTER PRODUCT CHANGE               
         BE    BCHGX                                                            
         OI    LCHGIND,LNEWPRD     YES                                          
*                                                                               
BCHGX    B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* MAKE CHANGES TO BUY SPOT ELEMENT BUY DATES AND RATE OVERRIDES       *         
***********************************************************************         
SPOTCHG  GOTO1 =A(SPOTCHNG),RR=APRELO                                           
         B     EXIT1                                                            
***********************************************************************         
* ADD/DELETE POOL SPOT ELEMENTS                                       *         
* ON ENTRY:    R2                  A(BUY RECORD)                                
*              R5                  A(WEEK TABLE ENTRY)                          
*              R8                  A(WK POSITION IN SCHED TABLE ENTRY)          
*              R9                  A(CAMPAIGN DATES TABLE ENTRY)                
*              LFLAG               LFSTWK FOR THE FIRST WEEK                    
* OUTPUT: FVMSGNO = FVRECOF IF RECORD OVERFLOW                        *         
***********************************************************************         
POOLSPT  GOTO1 =A(POOLSPOT),RR=APRELO                                           
         B     EXIT1                                                            
***********************************************************************         
* ADD/DELETE NON-POOL SPOT ELEMENTS                                             
* ON ENTRY:    R2                  A(BUY RECORD)                                
*              R5                  A(WEEK TABLE ENTRY)                          
*              R8                  A(WK POSITION IN SCHED TABLE ENTRY)          
*              R9                  A(CAMPAIGN DATES TABLE ENTRY)                
*              LFLAG               LFSTWK FOR THE FIRST WEEK                    
*                                                                               
* NOTE: APBYTE GETS CLOBBERED                                                   
***********************************************************************         
         USING BUYRECD,R2                                                       
NPOOLSPT XR    RF,RF               FIND THE SPOT ELEMENT FOR THIS WEEK          
         USING BUYRECD,R2                                                       
         LA    R1,BDELEM                                                        
*                                                                               
NPOL1    CLI   0(R1),0             END OF RECORD?                               
         BNE   NPOL2               NO                                           
         CLI   0(R8),0             YES, STRAIGHT ADD?                           
         BE    NPOL5                    YES                                     
         DC    H'0'                                                             
*                                                                               
NPOL2    CLI   0(R1),X'06'         NON-POOL BUY ELEMENT                         
         BE    NPOL2H                                                           
         CLI   0(R1),X'07'         NON-POOL BUY ELEMENT                         
         BNE   NPOL3                                                            
         USING REGELEM,R1                                                       
         TM    RSTATUS,RSMINUSQ    X'80' - MINUS OTO?                           
         BNZ   NPOL3                                                            
NPOL2H   TM    LFLAG,LFSTWK+LWEEKLY  TEST WEEKLY SKED AND FIRST WEEK            
         BNO   *+18                                                             
         CLC   RDATE,CMPSTMNP      YES-COMP DATE TO CAMPAIGN START MON          
         BL    NPOL3                                                            
         B     *+14                                                             
         CLC   RDATE,0(R9)         TEST BUY DATE IS IN THIS WEEK                
         BL    NPOL3                                                            
         CLC   RDATE,2(R9)                                                      
         BNH   NPOL8                                                            
*                                                                               
NPOL3    IC    RF,1(R1)            NEXT BUY ELEMENT                             
         AR    R1,RF                                                            
         B     NPOL1                                                            
***********************************                                             
* SIMPLE ADD OF NEW SPOT ELEMENT                                                
***********************************                                             
NPOL5    XC    APELEM,APELEM       NO - BUILD NEW SPOT ELEMENT                  
         LA    R1,APELEM                                                        
         USING REGELEM,R1                                                       
         MVI   RCODE,6                                                          
         MVI   RLEN,10                                                          
         MVC   RDATE,2(R5)         BUY DATE                                     
*                                                                               
         L     RE,ATWA                                                          
         AHI   RE,SAVAREA-TWAD                                                  
         USING SAVAREA,RE                                                       
         CLC   RDATE,SVELKSDT                                                   
         BL    NPOL6                                                            
         CLC   RDATE,SVELKNDT                                                   
         BNH   LCKDSPT1                                                         
         DROP  RE                                                               
*                                                                               
NPOL6    MVC   RNUM,1(R8)          NUMBER OF SPOTS                              
         LA    R1,BUYREC           ADD THE ELEMENT                              
         BAS   RE,ADDREG                                                        
         B     NPOLX                                                            
         EJECT                                                                  
***********************************                                             
* NOT SO SIMPLE CHANGE OF NUMBER OF SPOTS                                       
*      NEED TO   ADD/DELETE/MODIFY   THE   SPOT/OTO   ELEMENTS                  
***********************************                                             
NPOL8    L     RE,ATWA                                                          
         AHI   RE,SAVAREA-TWAD                                                  
         USING SAVAREA,RE                                                       
         CLC   RDATE,SVELKSDT                                                   
         BL    NPOL9                                                            
         CLC   RDATE,SVELKNDT                                                   
         BNH   LCKDSPT1                                                         
         DROP  RE                                                               
***************                                                                 
* BUY ELEMENT FOUND                                                             
***************                                                                 
NPOL9    CLC   0(1,R8),1(R8)       ARE WE ADDING OR DELETING SPOTS?             
         BL    NPOLADD             ADDING                                       
*********                                                                       
* DELETING SPOTS                                                                
*********                                                                       
NPOLDEL  XR    RE,RE                                                            
         LR    RF,RE                                                            
         IC    RE,0(R8)                                                         
         IC    RF,1(R8)                                                         
         SR    RE,RF                                                            
         STC   RE,APBYTE           APBYTE CONTAINS # OF SPOTS TO DELETE         
*                                                                               
NPOLDEL0 LR    RF,R1               BUY ELEMENT FOUND, SEE IF ANY -OTO           
         XR    R0,R0                                                            
NPOLDEL1 IC    R0,1(RF)                                                         
         AR    RF,R0                                                            
         CLI   0(RF),0             END OF RECORD?                               
         BE    NPOLDEL8            YES, NO +OTO'S                               
         CLI   0(RF),X'07'                                                      
         BNE   NPOLDEL1                                                         
         TM    RSTATUS-REGELEM(RF),X'80'  +OTO ?                                
         BNZ   NPOLDEL1                                                         
         CLC   RDATE-REGELEM(L'RDATE,RF),RDATE    +OTO FOR SAME DATE?           
         BNE   NPOLDEL1                                                         
         OC    RPAY-REGELEM(L'RPAY,RF),RPAY-REGELEM(RF)   PAID?                 
         BNZ   NPOLDEL1                                                         
***************                                                                 
* WE HAVE AN UNPAID +OTO FOR THIS WEEK                                          
***************                                                                 
         CLC   APBYTE,RNUM-REGELEM(RF)   DELETING MORE THAN +OTO?               
         BNE   *+12                                                             
         MVI   0(RF),FF            NO SAME NUMBER, SO REMOVE THE +OTO           
         B     NPOL99                                                           
*                                                                               
         BH    NPOLDEL2            DELETING MORE THAN +OTO                      
         XR    RE,RE               NO, REDUCE # OF SPOTS IN THE +OTO BY         
         IC    RE,RNUM-REGELEM(RF)        THE # OF SPOTS WE'RE DELETING         
         IC    R0,APBYTE                                                        
         SR    RE,R0                                                            
         STC   RE,RNUM-REGELEM(RF)                                              
         B     NPOLX                                                            
*                                                                               
NPOLDEL2 IC    RE,RNUM-REGELEM(RF) REDUCE # OF SPOTS WE'RE DELETING BY          
         IC    R0,APBYTE              THE # OF SPOTS IN THE +OTO                
         SR    R0,RE                                                            
         STC   R0,APBYTE                                                        
         LR    R0,R1               SAVE A(REGELEM)                              
         MVI   0(RF),FF            DELETE THE +OTO                              
         MVI   APELEM,FF                                                        
         GOTO1 ADELELS,BUYREC                                                   
         MVI   APELEM,X'06'                                                     
         LR    R1,R0                                                            
         B     NPOLDEL0                                                         
*                                                                               
NPOLDEL8 XR    RE,RE               DELETE # OF SPOTS TO ELEMENT FOUND           
         IC    RE,APBYTE                                                        
         XR    RF,RF                                                            
         IC    RF,RNUM                                                          
         SR    RF,RE                                                            
         STC   RF,RNUM                                                          
*                                                                               
         CLI   RNUM,0              DID WE REDUCE THIS NON-POL WEEK DOWN         
         BNE   NPOLX                  TO 0?                                     
         MVI   0(R1),X'FF'         YES!  DELETE IT SO AS NOT TO CONFUSE         
         B     NPOL99                 THE BUY PROGRAM                           
*********                                                                       
* ADDING SPOTS                                                                  
*********                                                                       
NPOLADD  XR    RE,RE                                                            
         LR    RF,RE                                                            
         IC    RE,1(R8)                                                         
         IC    RF,0(R8)                                                         
         SR    RE,RF                                                            
         STC   RE,APBYTE           APBYTE CONTAINS # OF SPOTS TO ADD            
*                                                                               
NPOLADD0 LR    RF,R1               BUY ELEMENT FOUND, SEE IF ANY -OTO           
         XR    R0,R0                                                            
NPOLADD1 IC    R0,1(RF)                                                         
         AR    RF,R0                                                            
         CLI   0(RF),0             END OF RECORD?                               
         BE    NPOLADD8            YES, NO -OTO'S                               
         CLI   0(RF),X'07'                                                      
         BNE   NPOLADD1                                                         
         TM    RSTATUS-REGELEM(RF),X'80'  -OTO ?                                
         BZ    NPOLADD1                                                         
         CLC   RDATE-REGELEM(L'RDATE,RF),RDATE    -OTO FOR SAME DATE?           
         BNE   NPOLADD1                                                         
         OC    RPAY-REGELEM(L'RPAY,RF),RPAY-REGELEM(RF)   PAID?                 
         BNZ   NPOLADD1                                                         
***************                                                                 
* WE HAVE AN UNPAID -OTO FOR THIS WEEK                                          
***************                                                                 
         CLC   APBYTE,RNUM-REGELEM(RF)   ADDING MORE THAN -OTO?                 
         BNE   *+12                                                             
         MVI   0(RF),FF            NO SAME NUMBER, SO REMOVE THE -OTO           
         B     NPOL99                                                           
*                                                                               
         BH    NPOLADD2            ADDING MORE THAN -OTO                        
         XR    RE,RE               NO, REDUCE # OF SPOTS IN THE -OTO BY         
         IC    RE,RNUM-REGELEM(RF)        THE # OF SPOTS WE'RE ADDING           
         IC    R0,APBYTE                                                        
         SR    RE,R0                                                            
         STC   RE,RNUM-REGELEM(RF)                                              
         B     NPOLX                                                            
*                                                                               
NPOLADD2 IC    RE,RNUM-REGELEM(RF) REDUCE # OF SPOTS WE'RE ADDING BY            
         IC    R0,APBYTE              THE # OF SPOTS IN THE -OTO                
         SR    R0,RE                                                            
         STC   R0,APBYTE                                                        
         LR    R0,R1               SAVE A(REGELEM)                              
         MVI   0(RF),FF            DELETE THE -OTO                              
         MVI   APELEM,FF                                                        
         GOTO1 ADELELS,BUYREC                                                   
         MVI   APELEM,X'06'                                                     
         LR    R1,R0                                                            
         B     NPOLADD0                                                         
*                                                                               
NPOLADD8 XR    RE,RE               ADD # OF SPOTS TO ELEMENT FOUND              
         IC    RE,APBYTE                                                        
         XR    RF,RF                                                            
         IC    RF,RNUM                                                          
         AR    RE,RF                                                            
         STC   RE,RNUM                                                          
         B     NPOLX                                                            
*                                                                               
NPOL99   MVI   APELEM,FF                                                        
         GOTO1 ADELELS,BUYREC                                                   
*                                                                               
NPOLX    B     EXIT1                                                            
         DROP  R1                                                               
*                                                                               
LCKDSPT1 L     R5,ATWA             R5 WASN'T POINTING HERE                      
         LA    R2,BWSMSGH                                                       
         XC    8(L'BWSMSG,R2),8(R2)                                             
         MVC   8(L'SPTSLCK1,R2),SPTSLCK1                                        
         OI    6(R2),X'80'                                                      
         LA    R2,BWSKEYH                                                       
         DC    H'0'                                                             
         DC    C'$ABEND'                                                        
SPTSLCK1 DC    C'CANNOT TRANSFER, SPOTS IN LOCKED PERIOD!'                      
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD BUY TURNAROUND REQUEST                               *         
* ON ENTRY, R3 = A(BWS RECORD)                                        *         
*           IOAREA3 CONTAINS BUY RECORD                               *         
***********************************************************************         
         SPACE 1                                                                
         USING BWDRECD,R3                                                       
ADDREQ   L     R2,AIOAREA3                                                      
         USING BUYRECD,R2                                                       
         MVI   APBYTE,0                                                         
         LA    R6,LSTALIST         DO FOR EVERY STATION                         
         LA    R9,MAXSTA                                                        
*                                                                               
ADDR1    OC    0(3,R6),0(R6)       TEST NO MORE STATIONS                        
         BZ    ADDRX                                                            
         XC    APDUB,APDUB                                                      
         MVC   APDUB+2(3),0(R6)                                                 
         GOTO1 VMSUNPK,APPARM,APDUB,APWORK,APWORK+4                             
         XC    APELEM,APELEM                                                    
         MVI   APELEM+14,106                                                    
         LA    R4,APELEM+26                                                     
         MVC   0(80,R4),BLANKS                                                  
         MVC   0(2,R4),=C'61'      NON-POL GET 61'S                             
         CLI   BPRD,FF             TEST POL                                     
         BNE   *+12                                                             
         CLI   CLTPROF,C'2'        YES - TEST BRD POL 61'S                      
         BNE   ADDR2                                                            
         CLI   CLTPROF+2,C'1'      TEST BRD POL 61'S BY STA                     
         BNE   ADDR4                                                            
         MVC   18(5,R4),APWORK+4                                                
         MVI   APBYTE,1                                                         
         B     ADDR4                                                            
*                                                                               
ADDR2    MVC   0(2,R4),=C'81'      PROF+0 = 0 OR 1 SO GET 81'S                  
         CLI   CLTPROF+2,C'2'      TEST U3 BY MKT                               
         BNE   *+14                                                             
         MVC   0(2,R4),=C'U3'                                                   
         B     ADDR4                                                            
*                                                                               
         CLI   CLTPROF+2,C'3'      TEST U3 BY STATION                           
         BNE   *+10                                                             
         MVC   0(2,R4),=C'U3'                                                   
         MVC   18(5,R4),APWORK+4    AND ALL 81'S ARE BY STATION                 
         MVI   APBYTE,1                                                         
*                                                                               
ADDR4    CLC   0(2,R4),=C'U3'                                                   
         BE    ADDR6                                                            
         PACK  APDUB,0(2,R4)                                                    
         CVB   R0,APDUB                                                         
         STC   R0,APELEM+10                                                     
*                                                                               
ADDR6    MVC   2(2,R4),CUAALF                                                   
         MVC   4(1,R4),QMED                                                     
         MVC   5(3,R4),QCLT                                                     
         MVC   8(2,R4),=C'NN'      NO MKT/PRD GRPS                              
         MVC   11(3,R4),QPRD                                                    
         CLI   BPRD,FF             TEST POL                                     
         BNE   ADDR8               NO                                           
         CLI   CLTPROF,C'0'        TEST BRD POL                                 
         BE    ADDR8               NO                                           
         CLI   BDMASPRD,0                                                       
         BE    ADDR8                                                            
         CLI   CMPPRD1,0           TEST FOR CAMPAIGN PIGGYBACKS                 
         BE    *+14                                                             
         MVC   11(3,R4),COMPRD1    YES                                          
         B     ADDR8                                                            
         OC    POLPRD1,POLPRD1     NO-TEST PRODUCT ALLOCATED THIS TIME          
         BZ    ADDR8                                                            
         MVC   11(3,R4),POLPRD1    YES-USE ALLOCATED PRODUCT 1                  
*                                                                               
ADDR8    MVC   14(4,R4),QMKT       MARKET                                       
*                                                                               
         ZIC   R0,BEST             EST                                          
         CLI   ESTREQLO,0          TEST SERIES REQ                              
         BE    *+8                                                              
         IC    R0,ESTREQLO                                                      
         CVD   R0,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  23(3,R4),APDUB                                                   
*                                                                               
         ICM   R0,1,ESTREQHI                                                    
         BZ    *+18                                                             
         CVD   R0,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  26(3,R4),APDUB                                                   
*                                                                               
         MVC   37(12,R4),ESTST     EST START/END DATES                          
*                                                                               
         MVI   59(R4),C'B'         T/A IND                                      
         CLI   APELEM+10,81                                                     
         BNE   *+8                                                              
         MVI   59(R4),C'A'         SET TO GEN U4                                
         CLI   APELEM+10,61                                                     
         BNE   ADDR10                                                           
         MVC   61(5,R4),=C'30000'                                               
         MVI   APELEM+10,0                                                      
         MVC   APELEM+26(2),=C'U3'                                              
         MVC   APELEM+26+49(19),BLANKS CLEAR ALL OPTIONS                        
*                                                                               
ADDR10   MVC   68(12,R4),BYRNM                                                  
         OC    68(12,R4),BLANKS                                                 
*                                                                               
         GOTO1 VDMGR,APPARM,=C'DMADD',=C'REQUEST',APELEM,APELEM                 
         CLI   8(R1),0                                                          
         BNE   ADDR90                                                           
*                                                                               
         LA    R1,SAVAREA                                                       
         USING SAVAREA,R1                                                       
         CLC   SVRFPGRP,=8C' '                                                  
         BNH   ADDR11                                                           
         CLC   =C'G7',CUAALF                                                    
         BNE   ADDR11                                                           
         TM    SVESTFL1,EF1REQ     TEST ESTIMATE REQUESTABLE                    
         BZ    ADDR11                                                           
         MVC   APELEM+128(80),APELEM+26    SAVE ORIGINAL REQUEST                
*****                                                                           
         LA    RF,APELEM                                                        
         USING REQHDRD,RF                                                       
         MVC   REQORIG,SVRFPID                                                  
         DROP  RF                                                               
*****                                                                           
         MVC   APELEM+26(2),=C'RF'                                              
         MVC   APELEM+26+49(8),SVRFPGRP                                         
         DROP  R1                                                               
         GOTO1 VDMGR,APPARM,=C'DMADD',=C'REQUEST',APELEM,APELEM                 
         MVC   APELEM+26(80),APELEM+128    RESTORE ORIGINAL REQUEST             
*****                                                                           
         LA    R1,APELEM           CLEAR PARTS OF REQHDR JUST SET               
         USING REQHDRD,R1                                                       
         XC    REQORIG,REQORIG                                                  
         MVI   REQFLAG,0                                                        
         DROP  R1                                                               
*****                                                                           
*                                                                               
ADDR11   CLI   BDTIME,0            TEST PIGGYBACK                               
         BE    ADDR14              NO                                           
         LA    R8,BDELEM           NEED PARTNER PRODUCT                         
         SR    R0,R0                                                            
*                                                                               
ADDR12   IC    R0,1(R8)                                                         
         AR    R8,R0                                                            
         CLI   0(R8),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R8),4                                                          
         BNE   ADDR12                                                           
         MVC   11(3,R4),6(R8)      PASSIVE PRD                                  
         ZIC   R0,3(R8)            PASSIVE EST                                  
         CVD   R0,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  23(3,R4),APDUB                                                   
         GOTO1 VDMGR,APPARM                                                     
         CLI   8(R1),0                                                          
         BNE   ADDR90                                                           
         B     ADDR20                                                           
*                                                                               
ADDR14   LA    R1,COMPRD2          CHECK FOR P/B MASPRD                         
         CLI   CMPPRD2,0                                                        
         BNE   ADDR16                                                           
         LA    R1,POLPRD2                                                       
         CLI   INOPRD+1,0                                                       
         BE    ADDR20                                                           
*                                                                               
ADDR16   MVC   11(3,R4),0(R1)      ADD REQUEST FOR SECOND BRAND                 
         GOTO1 VDMGR,APPARM                                                     
         CLI   8(R1),0                                                          
         BNE   ADDR90                                                           
*                                                                               
ADDR20   CLI   APBYTE,1            TEST BY STATION                              
         BNE   ADDRX                                                            
         LA    R6,4(R6)            YES-NEXT STATION                             
         BCT   R9,ADDR1                                                         
         B     ADDRX                                                            
*                                                                               
ADDR90   MVC   FVMSGNO,=AL2(FVREQERR) ERROR EXIT                                
         LA    R1,BWSKEYH                                                       
         ST    R1,FVADDR                                                        
*                                                                               
ADDRX    B     EXIT1                                                            
         EJECT                                                                  
***********************************************************************         
* ADD COMMENT ELEMENT TO BUY RECORD                                   *         
***********************************************************************         
         SPACE 1                                                                
CMTADD   L     R2,AIOAREA3                                                      
         USING BUYRECD,R2                                                       
         MVI   LHICOM,0                                                         
         ICM   R4,15,LACMTEL                                                    
         BZ    CMTADDX                                                          
         SR    R0,R0                                                            
*                                                                               
         USING COMEL,R4                                                         
CMTADD2  CLI   0(R4),0                                                          
         BE    CMTADDX                                                          
         CLI   0(R4),NBRCMELQ                                                   
         BNE   CMTADD4                                                          
         XC    APELEM,APELEM                                                    
         LA    R8,APELEM                                                        
         USING COMELEM,R8                                                       
         MVI   CMCODE,X'66'                                                     
         ZIC   RE,COMELLN                                                       
         AHI   RE,-(COMCOM-COMEL)                                               
         LA    RF,COMCOM-1                                                      
         MVI   CMNUM,1                                                          
         CLI   COMNUM,1                                                         
         BL    CMTADD3                                                          
         CLI   COMNUM,5                                                         
         BH    CMTADD3                                                          
         BCTR  RE,0                                                             
         LA    RF,1(RF)                                                         
         MVC   CMNUM,COMNUM        COMMENT NUMBER                               
*                                                                               
CMTADD3  EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   CMDATA(0),0(RF)                                                  
         LA    RE,1+CMDATA-COMELEM(RE)                                          
         STC   RE,CMLEN                                                         
         GOTO1 AADDELS,BUYREC                                                   
*                                                                               
         CLC   CMNUM,LHICOM        HIGHEST COMMENT NUMBER                       
         BNH   CMTADD4                                                          
         MVC   LHICOM,CMNUM                                                     
*                                                                               
CMTADD4  IC    R0,1(R4)            NEXT COMMENT ELEMENT                         
         AR    R4,R0                                                            
         B     CMTADD2                                                          
*                                                                               
CMTADDX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ADD ID ELEMENT TO BUY RECORD                                        *         
***********************************************************************         
         SPACE 1                                                                
IDADD    L     R2,AIOAREA3                                                      
         USING BUYRECD,R2                                                       
         L     R5,ATWA                                                          
         USING TWAD,R5                                                          
*                                                                               
         TM    LIND2,LB0PURP       PURPOSE CODE REQUIRED?                       
         BZ    IDAD0                                                            
         MVC   LBUYID(L'LIDRBYR),LIDRBYR   YES, IDR HAS PURPOSE CODE            
         OC    LBUYID,=12C' '                                                   
         B     IDAD10                                                           
*                                                                               
IDAD0    TM    CLTIND,CLTICC       TEST CCUSA INTERFACE                         
         BO    IDAD2               YES                                          
         OC    INOBUYID,INOBUYID   NO-TEST ID OPTION ENTERED                    
         BZ    IDAD1               NO                                           
         MVC   LBUYID,BLANKS       YES-ADD ID ELEMENT                           
         MVC   LBUYID(L'INOBUYID),INOBUYID                                      
         B     IDAD1A                                                           
*                                                                               
IDAD1    OC    LRECBYID,LRECBYID   TEST RECORD HAS ID ELEMENT                   
         BZ    *+14                                                             
         MVC   LBUYID,LRECBYID     YES-GET ID FROM THERE                        
         B     IDAD1A                                                           
         OC    LMGRPID,LMGRPID     TEST MARKET GROUP ID                         
         BZ    IDADX                                                            
         MVC   LBUYID,BLANKS       YES-USE THAT ID                              
         MVC   LBUYID(L'LMGRPID),LMGRPID                                        
IDAD1A   OC    LIDRBYR,LIDRBYR        DO WE HAVE A IDR BUYER?                   
         BZ    IDAD10                                                           
         MVI   LBUYID+L'LMGRPID,C'-'  YES                                       
         MVC   LBUYID+L'LMGRPID+1(L'LIDRBYR),LIDRBYR                            
         B     IDAD10                                                           
*                                                                               
IDAD2    LA    R3,LACNTAB          SEARCH ACN TABLE FOR STATION                 
         LA    R0,LNACNTAB                                                      
*                                                                               
IDAD4    OC    0(3,R3),0(R3)                                                    
         BZ    IDAD6               ADD NEW ENTRY                                
         CLC   0(3,R3),BUYMSTA+2                                                
         BE    IDAD8               FOUND                                        
         LA    R3,15(R3)                                                        
         BCT   R0,IDAD4                                                         
         DC    H'0'                ACN TABLE NEEDS MORE ENTRIES                 
*                                                                               
IDAD6    MVC   0(3,R3),BUYMSTA+2                                                
         LA    R4,APELEM           SET UP SPACNVAL INTERFACE BLOCK              
         USING SPAVBLKD,R4                                                      
         LA    RF,SPAVBLKL                                                      
         XCEF  SPAVBLK,(RF)                                                     
         MVC   SPAVATWA,ATWA                                                    
         MVC   SPAVACMF,ACOM                                                    
         XC    APWORK(12),APWORK                                                
         MVC   APWORK(5),INOBUYID                                               
         OC    INOBUYID,INOBUYID                                                
         BZ    *+10                                                             
         OC    APWORK(12),BLANKS                                                
         LA    R1,APWORK                                                        
         ST    R1,SPAVAACN                                                      
         MVC   SPAVAGY,CUAALF                                                   
         MVC   SPAVMED,QMED                                                     
         GOTO1 VMSUNPK,APPARM,BUYMSTA,APWORK+12,APWORK+16                       
         MVC   SPAVSTA,APWORK+16                                                
         CLI   SPAVSTA+4,C' '                                                   
         BH    *+8                                                              
         MVI   SPAVSTA+4,C'T'                                                   
         MVC   SPAVCIFC,CLTIFC                                                  
*                                                                               
         XC    BWSMSG,BWSMSG                                                    
         GOTO1 VSPACNVL,APPARM,SPAVBLK    CALL ACN VALIDATION OVERLAY           
         CLI   SPAVERR,0                                                        
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFSET)                                             
         B     IDADX                                                            
         MVC   3(12,R3),SPAVDACN   SAVE DEFAULT ACN NUMBER                      
         DROP  R4                                                               
*                                                                               
IDAD8    MVC   LBUYID,3(R3)        ACN NUMBER FROM TABLE                        
*                                                                               
IDAD10   XC    APELEM,APELEM       ADD ID ELEMENT                               
         MVI   APELEM,X'70'                                                     
         MVI   APELEM+1,15                                                      
         MVI   APELEM+2,0                                                       
         MVC   APELEM+3(12),LBUYID                                              
         GOTO1 AADDELS,BUYREC                                                   
*                                                                               
IDADX    B     EXIT1                                                            
         EJECT                                                                  
***********************************************************************         
* ADD PIGGYBACK ELEMENT TO BUY RECORD                                           
***********************************************************************         
         SPACE 1                                                                
PBKADD   L     R2,AIOAREA3                                                      
         USING BUYRECD,R2                                                       
         XC    APELEM,APELEM                                                    
         LA    R8,APELEM                                                        
         USING PBELEM,R8                                                        
         MVI   PBCODE,4                                                         
         MVI   PBLEN,9                                                          
         MVC   PBPROD,CMPPRD2      PASSIVE PRODUCT CODE                         
         MVC   PBEST,BEST          ESTIMATE                                     
         MVC   PBTIME,CMPLEN2      TIME SHARE                                   
         MVC   PBCOST,CMPLEN2      COST SHARE                                   
         LA    R4,IOKEY            BUILD KEY OF CLIENT RECORD                   
         USING CLTHDRD,R4                                                       
         XC    CKEY,CKEY                                                        
         MVI   CKEYTYPE,0                                                       
         MVC   CKEYAM,BAGYMD                                                    
         MVC   CKEYCLT,BCLT                                                     
         GOTO1 AIO,FILRD1          READ CLIENT RECORD                           
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,AIOAREA1                                                      
         LA    R1,CLIST            LOOK FOR PASSIVE PRODUCT CODE                
*                                                                               
PBAD2    CLI   0(R1),C'A'                                                       
         BNL   *+6                                                              
         DC    H'0'                                                             
         CLC   PBPROD,3(R1)                                                     
         BE    *+12                                                             
         LA    R1,4(R1)                                                         
         B     PBAD2                                                            
         MVC   PBPRD,0(R1)         FOUND - PRODUCT MNEMONIC                     
         GOTO1 AADDELS,BUYREC      ADD THE PIGGYBACK ELEMENT                    
*                                                                               
         MVC   APBYTE,PBPROD                                                    
         GOTO1 =A(GETPIG),RR=APRELO                                             
         MVC   PBPRD,APFULL        3 BYTE PRODUCT MNEMONIC                      
         GOTO1 AADDELS,BUYREC      ADD THE PIGGYBACK ELEMENT                    
*                                                                               
PBADX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ADD ORBIT ELEMENT                                                   *         
* NOTE: THIS ROUTINE ALSO SETS BDDAY                                  *         
***********************************************************************         
         SPACE 1                                                                
ORBADD   L     R2,AIOAREA3                                                      
         USING BUYRECD,R2                                                       
         L     R3,AIOAREA2                                                      
         USING BWDRECD,R3                                                       
         XC    APELEM,APELEM       BUILD ORBIT ELEMENT                          
         LA    R4,APELEM                                                        
         USING ORBELEM,R4                                                       
         MVI   ORBCODE,X'67'                                                    
         LA    R8,ORBDAY                                                        
         USING ORBDAY,R8                                                        
         MVC   IOKEY(13),BWDKEY                                                 
         LA    R3,IOKEY                                                         
         MVI   BWDKELDY,0                                                       
         XC    BWDKELTM,BWDKELTM                                                
         MVI   BWDKELSQ,1                                                       
         MVI   LORBDAY,X'40'       FIRST DAY ALWAYS = MONDAY                    
         CLI   ESTOWSDY,0          IF OUT-OF-WEEK ROTATOR,                      
         BE    *+8                                                              
         MVI   LORBDAY,X'7F'       SET ALL DAYS ON                              
         MVC   LORBTIM1,=X'FFFF'                                                
         XC    LORBTIM2,LORBTIM2                                                
         LA    R1,MINHI1           READ ORBIT LINES                             
         B     ORB2+4                                                           
*                                                                               
ORB2     LA    R1,MINSEQ1                                                       
         GOTO1 AMIN                                                             
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     ORB10                                                            
         CLC   IOKEY(BWDKELSQ-BWDKEY),IOKEYSAV                                  
         BNE   ORB10                                                            
         L     R3,AIOAREA1                                                      
         MVC   ORBDAY,BWDPODAY     SET ORBIT DAYS                               
         MVC   ORBTIME,BWDTIMES              TIMES                              
         MVC   ORBDESC,BWDPROG               DESC                               
         OC    LORBDAY,ORBDAY      DAYS RANGE                                   
         CLC   LORBTIM1,ORBTIME    TIMES RANGE                                  
         BNH   *+10                                                             
         MVC   LORBTIM1,ORBTIME                                                 
         CLC   LORBTIM2,ORBTIME+2                                               
         BNL   *+10                                                             
         MVC   LORBTIM2,ORBTIME+2                                               
         SR    R0,R0                                                            
         LA    R1,BWDEL            FIND DEMO ELEMENT                            
*                                                                               
ORB4     CLI   0(R1),0                                                          
         BE    ORB8                                                             
         CLI   0(R1),DMOELCDQ                                                   
         BE    *+14                                                             
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     ORB4                                                             
         ZIC   RF,1(R1)            FOUND - FIND TARGET RATING                   
         AR    RF,R1                                                            
         LA    R1,DMODEMO-DMOEL(R1)                                             
*                                                                               
ORB6     CLI   1(R1),C'R'                                                       
         BNE   *+14                                                             
         MVC   ORBDEM,6(R1)                                                     
         B     ORB8                                                             
         LA    R1,L'DMODEMO(R1)                                                 
         CR    R1,RF                                                            
         BL    ORB6                                                             
*                                                                               
ORB8     LA    R8,16(R8)           NEXT ORBIT LINE                              
         B     ORB2                                                             
*                                                                               
ORB10    SR    R8,R4                                                            
*** FOLLOWING IS NEW CODE TO CATCH RIDICULOUS AMOUNT OF EXISTING ORBITS         
***                                        MHC 12/09/02                         
         CHI   R8,255              DOES IT TAKE UP MORE THAN 255 BYTES?         
         BNH   ORB11                - NO, WE GOOD (255 BYTE LIMIT)              
         XC    BWSMSG,BWSMSG                                                    
         OI    BWSMSGH+6,X'88'     TRANSMIT AND HIGH INTENSITY                  
         MVC   BWSMSG(33),=C'Too many ORBITS on this station!!'                 
         DC    H'0',C'$ABEND'                                                   
*                                                                               
ORB11    STC   R8,ORBLEN           SET ELEMENT LENGTH                           
         CLI   ORBLEN,4            TEST ANY ORBIT LINES                         
         BNH   ORB90               NO-ERROR                                     
         GOTO1 AADDELS,BUYREC      YES - ADD ORBIT ELEMENT                      
*                                                                               
         MVC   BDTIMST,LORBTIM1    SET BUY TIMES                                
         MVC   BDTIMEND,LORBTIM2                                                
         GOTO1 VDAYUNPK,APPARM,LORBDAY,(7,APDUB)                                
         GOTO1 VDAYPAK,APPARM,(7,APDUB),LORBDAY,APBYTE                          
         CLI   LORBDAY,0                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         ZIC   R1,APBYTE           FILL IN GAPS IN DAYS                         
         SR    R0,R0                                                            
         SLDL  R0,28               R0 = START DAY                               
         SRL   R1,28               R1 = END DAY                                 
         LA    RE,APDUB                                                         
         LA    RF,=C'MTWTFSS'                                                   
         MVI   APFLAG,0                                                         
*                                                                               
ORB12    CLI   0(RE),C'.'                                                       
         BNE   ORB14                                                            
         CLI   APFLAG,1                                                         
         BNE   ORB16                                                            
         MVC   0(1,RE),0(RF)                                                    
*                                                                               
ORB14    MVI   APFLAG,1                                                         
*                                                                               
ORB16    LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R1,ORB12                                                         
*                                                                               
         GOTO1 VDAYPAK,APPARM,(7,APDUB),LORBDAY,APBYTE                          
         CLI   LORBDAY,0                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         TM    LFLAG,LREXFR        TEST RE-TRANSFER                             
         BO    *+14                                                             
         MVC   BDDAY,LORBDAY       NO - SET BUY DAYS FIELD                      
         B     ORBX                                                             
         CLC   BDDAY,LORBDAY       YES - TEST DAYS CHANGE                       
         BE    ORBX                                                             
         GOTO1 VDAYUNPK,APPARM,BDDAY,(7,APDUB)   YES --                         
         MVC   BDDAY,LORBDAY                     SET BUY DAYS FIELD             
         GOTO1 VDAYPAK,APPARM,(7,APDUB),APFLAG,APBYTE                           
         CLI   APFLAG,0                                                         
         BNE   *+6                                                              
         DC    H'0'                                                             
         ZIC   R1,APBYTE                                                        
         SRL   R1,4                R1 = START DAY OF OLD BUY                    
         CR    R0,R1               COMPARE NEW START DAY TO OLD                 
         BE    ORBX                                                             
         OI    LCHGIND,LSTDATE     NOT EQUAL                                    
         B     ORBX                                                             
*                                                                               
ORB90    MVC   FVMSGNO,=AL2(FVORBNL) ERROR EXIT                                 
         LA    R1,BWSKEYH                                                       
         ST    R1,FVADDR                                                        
*                                                                               
ORBX     B     EXIT1                                                            
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* ADD WARNING COMMENT IF THERE ARE BUY DAYS PRIOR TO CAMPAIGN START   *         
***********************************************************************         
         SPACE 1                                                                
WARNADD  L     R2,AIOAREA3                                                      
         USING BUYRECD,R2                                                       
         OC    LWKTAB(2),LWKTAB    TEST ANY SPOTS IN FIRST WEEK                 
         BZ    WARNX                                                            
         CLC   LWKTAB+2(2),LCMPST  TEST START DATE BEFORE CAMPN START           
         BNL   WARNX                                                            
         GOTO1 VDATCON,APPARM,(2,LWKTAB+2),(5,APWORK)                           
         LR    R1,R5                                                            
         AHI   R1,CMPDATSD-TWAD                                                 
         ST    R1,APPARM                                                        
         GOTO1 VADDAY,APPARM,,APDUB,F'-1'                                       
         GOTO1 VDATCON,APPARM,(0,APDUB),(5,APWORK+8)                            
         XC    APELEM,APELEM                                                    
         LA    R8,APELEM                                                        
         USING COMELEM,R8                                                       
         MVI   CMCODE,X'66'                                                     
         ZIC   R1,LHICOM           NEXT COMMENT NUMBER                          
         LA    R1,1(R1)                                                         
         STC   R1,CMNUM                                                         
         MVC   CMDATA(15),=C'NO SPOTS RUN ON'                                   
         MVC   CMDATA+16(8),APWORK                                              
         MVI   CMLEN,27                                                         
         CLC   APWORK(8),APWORK+8                                               
         BE    WARN2                                                            
         MVI   CMLEN,38                                                         
         MVI   CMDATA+25,C'-'                                                   
         MVC   CMDATA+27(8),APWORK+8                                            
*                                                                               
WARN2    GOTO1 AADDELS,BUYREC                                                   
*                                                                               
WARNX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ADD VAT ELEMENT TO BUY RECORD                                       *         
***********************************************************************         
         SPACE 1                                                                
VATADD   L     R2,AIOAREA3                                                      
         USING BUYRECD,R2                                                       
         CLI   CUDMED,C'C'         ONLY FOR CANADIAN AGENCIES                   
         BNE   VATADDX                                                          
         CLI   STAGST,C'X'         ONLY FOR GST CODES X AND Z                   
         BE    *+12                                                             
         CLI   STAGST,C'Z'                                                      
         BNE   VATADDX                                                          
         XC    APELEM,APELEM                                                    
         LA    R8,APELEM                                                        
         USING VATELEM,R8                                                       
         MVI   0(R8),X'6A'                                                      
         MVI   1(R8),3                                                          
         MVC   VATSTA,STAGST       STATION'S GST CODE                           
         LA    R4,BDELEM                                                        
         SR    R0,R0                                                            
         CLI   0(R4),0             ADD TO END OF BUY RECORD                     
         BE    *+14                                                             
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     *-14                                                             
         GOTO1 ARECUPA,APPARM,BUYREC,(R4)                                       
*                                                                               
VATADDX  B     EXIT1                                                            
         EJECT                                                                  
***********************************************************************         
* ADD PST ELEMENT TO BUY RECORD                                       *         
***********************************************************************         
         SPACE 1                                                                
PSTADD   L     R2,AIOAREA3                                                      
         USING BUYRECD,R2                                                       
         CLI   CUDMED,C'C'         ONLY FOR CANADIAN AGENCIES                   
         BNE   PSTADDX                                                          
******** CLC   CUAALF,=C'HD'       AND HDTO (FOR NOW)                           
******** BNE   PSTADDX                                                          
         XC    APELEM,APELEM                                                    
         LA    R8,APELEM                                                        
         USING PSTELEM,R8                                                       
         MVI   0(R8),X'6B'                                                      
         MVI   1(R8),12                                                         
         MVC   PSTVALS,LPST                                                     
         LA    R4,BDELEM                                                        
         SR    R0,R0                                                            
         CLI   0(R4),0             ADD TO END OF BUY RECORD                     
         BE    *+14                                                             
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     *-14                                                             
         GOTO1 ARECUPA,APPARM,BUYREC,(R4)                                       
*                                                                               
PSTADDX  B     EXIT1                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CHECK BUY ID OPTION                                      *         
***********************************************************************         
         SPACE 1                                                                
         USING BWDRECD,R3                                                       
CHKID    XC    LOVRMKT,LOVRMKT                                                  
         XC    LMGRPID,LMGRPID                                                  
         XC    LIDRBYR,LIDRBYR                                                  
         MVC   LSVKEY,IOKEY                                                     
*                                                                               
         XC    IOKEY,IOKEY         READ B0 PROFILE                              
         MVC   IOKEY(4),=C'S0B0'                                                
         MVC   IOKEY+4(2),CUAALF                                                
         MVC   IOKEY+6(1),QMED                                                  
         MVC   IOKEY+7(3),QCLT                                                  
         GOTO1 VGETPROF,APPARM,IOKEY,APWORK,VDMGR                               
         OC    APWORK(16),APWORK   TEST PROFILE FOUND                           
         BZ    CHKID10                                                          
         CLI   APWORK+9,C'Y'       TEST PURPOSE CODE REQUIRED                   
         BNE   CHKID10                                                          
         OI    LIND2,LB0PURP       YES, WE WANT TO REMEMBER THIS                
         GOTO1 =A(CKCAMIDR),APPARM,(RC),RR=APRELO    ANY IDR BUYER?             
*                                                                               
CHKID10  TM    CLTIND,CLTICC       TEST CCUSA INTERFACE                         
         BZ    CHKID20                                                          
         OC    INOBUYID,INOBUYID   YES-TEST ACN NUMBER OVERRIDE                 
         BZ    *+14                                                             
         OC    QSTA,QSTA           YES-TEST STATION FILTER                      
         BZ    ECCU                NO-ERROR                                     
         XC    LACNTAB(LNACNTAB*15),LACNTAB  CLEAR ACN TABLE                    
         B     CHKIDX                                                           
*                                                                               
CHKID20  TM    CLTIND,CLTIBID      TEST BUY ID REQUIRED                         
         BZ    CHKIDX              NO                                           
         OC    INOBUYID,INOBUYID   YES-TEST ID ENTERED                          
         BZ    CHKID60             NO                                           
         TM    CLTIND,CLTIBIMG     YES-TEST ID IS MARKET GROUP                  
         BZ    CHKIDX                                                           
         CLC   CLTMGRID,INOBUYID   YES-SCHEME SHOULD AGREE                      
         BNE   EIBI                                                             
         LA    R2,IOKEY            VALIDATE ID=MARKET GROUP                     
         USING MKGRECD,R2                                                       
         XC    MKGKEY,MKGKEY                                                    
         MVC   MKGKTYP,=X'0D02'                                                 
         MVC   MKGKAGMD,BAGYMD                                                  
         CLI   CLTMGRID,C'F'                                                    
         BH    *+10                                                             
         MVC   MKGKCLT,BCLT                                                     
         MVC   MKGKMID,CLTMGRID                                                 
         PACK  APDUB,INOBUYID+1(5)                                              
         MVC   MKGKMGRP,APDUB+5                                                 
         GOTO1 AIO,DIRHI                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   MKGKEY(11),IOKEYSAV                                              
         BNE   EIBI                INVALID MARKET GROUP                         
         MVC   MKGPTYP,=X'0D82'    SEE IF THERE'S A PASSIVE FOR                 
         MVC   MKGPMKT,BMKT        STATION'S HOME MARKET                        
         GOTO1 AIO,DIRHI                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         USING STERECD,R2                                                       
         XC    STEKEY,STEKEY       LOOKUP STATION EQUIVALENCE                   
         MVC   STEKTYP,=X'0D44'                                                 
         MVC   STEKAGMD,BAGYMD                                                  
         MVC   STEKCLT,BCLT                                                     
         MVC   STEKSTA,BWDSTA                                                   
         CLI   BWDSTA,C'0'         CABLE STATION?                               
         BL    *+8                                                              
         MVI   STEKSTA+4,C' '      YES, DON'T PUT T AFTER THE HEADEND           
         GOTO1 AIO,DIRHI                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   STEKEY,IOKEYSAV                                                  
         BNE   EIBI                                                             
         GOTO1 AIO,FILGET3         GET STATION EQUIV RECORD                     
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIOAREA3                                                      
         SR    R0,R0                                                            
         LA    R1,STEKEY+24                                                     
*                                                                               
CHKID30  CLI   0(R1),0                                                          
         BE    EIBI                                                             
         CLI   0(R1),3                                                          
         BNE   CHKID40                                                          
         USING STEEL03,R1                                                       
         CLC   STEMGID,CLTMGRID    MATCH MKTGRP SCHEME                          
         BNE   CHKID40                                                          
         PACK  APWORK(3),INOBUYID+1(5)                                          
         CLC   STEMGRP,APWORK      MATCH MKTGRP                                 
         BNE   CHKID40                                                          
         MVC   LOVRMKT,STEMGMKT       YES-SET OVERRIDE MARKET                   
         B     CHKID75                                                          
         DROP  R1,R2                                                            
*                                                                               
CHKID40  IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     CHKID30                                                          
*                                                                               
*                                  NO ID ENTERED-                               
CHKID60  TM    CLTIND,CLTIBIMG     TEST REQUIRE MARKET GROUP ID                 
         BZ    CHKIDX                                                           
         XC    IOKEY,IOKEY         YES-READ MARKET ASSIGNMENT RECORD            
         LA    R2,IOKEY                                                         
         USING MKARECD,R2                                                       
         MVC   MKAKTYP,=X'0D03'                                                 
         MVC   MKAKAGMD,BAGYMD                                                  
         CLI   CLTMGRID,C'F'                                                    
         BH    *+10                                                             
         MVC   MKAKCLT,BCLT                                                     
         MVC   MKAKMKT,BMKT                                                     
         GOTO1 AIO,DIRHI                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   MKAKEY(13),IOKEYSAV TEST RECORD FOUND                            
         BNE   EMGR                NO-ERROR                                     
         GOTO1 AIO,FILGET3                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIOAREA3                                                      
         SR    R0,R0                                                            
         LA    R1,MKAEL                                                         
*                                                                               
CHKID70  CLI   0(R1),0             FIND ELEMENT WITH MGRP ID                    
         BE    EMGR                                                             
*                                                                               
         CLI   0(R1),5                                                          
         BNE   CHKID80                                                          
         USING MKAEL05,R1                                                       
         CLC   CLTMGRID,MKAMGRP    COMPARE MARKET GROUP ID'S                    
         BNE   CHKID80                                                          
         MVC   LMGRPID(1),MKAMGRP  SET ID TO MARKET GROUP                       
         UNPK  APDUB,MKAMGRP+1(3)                                               
         MVC   LMGRPID+1(4),APDUB+3                                             
         DROP  R1                                                               
*                                                                               
CHKID75  DS    0H                                                               
         GOTO1 =A(CKCAMIDR),RR=APRELO    ANY IDR BUYER?                         
         B     CHKIDX                                                           
*                                                                               
CHKID80  IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     CHKID70                                                          
*                                                                               
ECCU     MVC   FVMSGNO,=AL2(FVCCST)                                             
         LA    R1,BWSKEYH                                                       
         B     CHKID90                                                          
*                                                                               
EIBI     MVC   FVMSGNO,=AL2(FVIBYID)                                            
         LA    R1,BWSOPTH                                                       
         B     CHKID90                                                          
*                                                                               
EMGR     MVC   FVMSGNO,=AL2(FVNOMGRP)                                           
         LA    R1,BWSOPTH                                                       
*                                                                               
CHKID90  ST    R1,FVADDR                                                        
*                                                                               
CHKIDX   MVC   IOKEY(13),LSVKEY                                                 
         B     EXIT1                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD BUYER NAME BY CALLING SPGETBUBL                      *         
***********************************************************************         
         SPACE 1                                                                
BYRNAME  CLC   BYRNAME,BLANKS                                                   
         BNH   BYRNAMEX                                                         
         LA    R2,APELEM                                                        
         XC    APELEM,APELEM                                                    
         USING GETBUBLD,R2                                                      
         MVC   GBCOMFAC,ACOM                                                    
         MVC   GBIOA,AIOAREA1                                                   
         LA    R1,APWORK                                                        
         ST    R1,GBNAMFLD                                                      
         MVI   GBTYPE,C'B'                                                      
         MVC   GBAGY,CUAALF                                                     
         MVC   GBOFFICE,CLTOFF                                                  
         MVC   GBMEDEBC,QMED                                                    
         MVC   GBCLTEBC,QCLT                                                    
         MVC   GBAGYMD,BAGYMD                                                   
         MVC   GBCLT,BCLT                                                       
         MVC   GBPRD,BPRD                                                       
         TM    LIND,LPOL                                                        
         BZ    *+8                                                              
         MVI   GBPRD,X'FF'                                                      
         MVC   GBEST,BEST                                                       
         MVC   GBMKT,BMKT                                                       
*****    OC    QSTA,QSTA           COMMENTED OUT BECAUSE IT SHOULD GO           
*****    BZ    *+10                AS FAR AS STATION LEVEL- THANKS TIM          
         MVC   GBSTA,BSTA                                                       
         XC    APWORK,APWORK                                                    
         LA    R1,L'BYRNM+1                                                     
         STC   R1,APWORK+5                                                      
         MVI   APWORK+8,C'='                                                    
         MVC   APWORK+9(L'BYRNM),BYRNM                                          
         GOTO1 VGETBUBL,APPARM,(R2)                                             
         CLI   GBERR,0                                                          
         BE    BYRNAMEX                                                         
         DC    H'0'                                                             
*                                                                               
BYRNAMEX B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* INFORM ADDS SYSTEM OF CHANGES TO BUY RECORDS                        *         
***********************************************************************         
         SPACE 1                                                                
ADDS     LA    R4,LSTALIST                                                      
         LA    R0,MAXSTA                                                        
         OC    0(3,R4),0(R4)       TEST ANY BUYLINES ADDED/CHANGED?             
         BZ    ADDSX               NO                                           
         LA    R2,APELEM                                                        
         XC    APELEM,APELEM       BUILD SPADINT BLOCK                          
         USING SPADINTD,R2                                                      
         MVI   ADACTN,ADACHG                                                    
         MVC   ADACOMFC,ACOM                                                    
         MVC   ADQAGYMD,BAGYMD                                                  
         MVC   ADQCLT,BCLT                                                      
         MVC   ADQPRD,BPRD                                                      
         CLI   CMPPRD1,0           TEST PIGGYBACKS                              
         BE    *+16                                                             
         MVC   ADQPRD,CMPPRD1      YES                                          
         MVC   ADQPRD2,CMPPRD2                                                  
         MVC   ADQEST,BEST                                                      
         MVC   ADQMKT,BMKT                                                      
*                                                                               
ADDS2    OC    0(3,R4),0(R4)                                                    
         BZ    ADDSX                                                            
         TM    3(R4),X'80'         TEST STATION READY FOR ADDS SEND             
         BZ    ADDS4                                                            
         MVC   ADQSTA,0(R4)        MOVE STATION TO BLOCK                        
         GOTO1 VSPADINT,APPARM,(R2)     CALL SPADINT                            
         CLI   ADERRS,0            CHECK FOR ERRORS                             
         BNE   ADDS9                                                            
*                                                                               
ADDS4    LA    R4,4(R4)            NEXT STATION                                 
         BCT   R0,ADDS2                                                         
         B     ADDSX                                                            
*                                                                               
ADDS9    MVC   FVMSGNO,=AL2(FVIADDS)   ADDS ERROR                               
         OI    FVERRIND,FVEUNWND       UNWIND THE TRANSACTION                   
*                                                                               
ADDSX    B     EXIT1                                                            
         EJECT                                                                  
***********************************************************************         
* ADD MASTER CLIENT ELEMENT                                           *         
***********************************************************************         
         SPACE 1                                                                
MCLTADD  L     R2,AIOAREA3                                                      
         USING BUYRECD,R2                                                       
         XC    APELEM,APELEM                                                    
         LA    R8,APELEM                                                        
         USING MCLTELEM,R8                                                      
         MVI   0(R8),X'61'                                                      
         MVI   1(R8),6                                                          
         MVC   MCLTCODE,CLTMTCLT   MASTER TRAFFIC CLIENT CODE                   
         MVC   MCLTUNIQ,CLTMTNUM   MASTER TRAFFIC CLIENT SEQUENCE NUM           
         MVC   MCLTPRD,CLTMTPRD    MASTER TRAFFIC CLIENT PRODUCT                
         GOTO1 AADDELS,BUYREC                                                   
         B     XIT                                                              
         DROP  R8                                                               
         EJECT                                                                  
***********************************************************************         
* FIX BWS RECORD TO REFLECT A SINGLE DAY                                        
***********************************************************************         
         SPACE 1                                                                
FIXDAILY DS    0H                                                               
         USING NBRSELD,R3                                                       
         ZIC   R2,LDTRDAY          R2=DAY NUMBER                                
         SR    R9,R9               R9=N'SPOTS FOR THIS DAY                      
         ICM   R6,15,LASPWEL       TEST SPOTS PER WEEK ELEMENT                  
         BZ    FD4                 NO                                           
         CLI   LDTRDAY,1           YES-TEST FIRST PASS                          
         BNE   FD2                                                              
         USING NBRSPEL,R6                                                       
         ZIC   RE,1(R6)            YES-SAVE SPOTS PER DAY ELEMENT               
         BCTR  RE,0                                                             
         MVC   LSVSPWEL(0),0(R6)                                                
         EX    RE,*-6                                                           
*                                                                               
FD2      ZIC   RE,1(R6)            CLEAR SPOTS PER DAY ELEMENT                  
         AHI   RE,-(NBRSPSPW-NBRSPEL)                                           
         BCTR  RE,0                                                             
         XC    NBRSPSPW(0),NBRSPSPW                                             
         EX    RE,*-6                                                           
         ZIC   RE,NBRSPLEN                                                      
         AHI   RE,-(NBRSPSPW-NBRSPEL)                                           
         CR    R2,RE                                                            
         BH    FD4                                                              
         LA    RE,NBRSPSPW-NBRSPEL-1(R2)                                        
         IC    R9,LSVSPWEL(RE)                                                  
         STC   R9,NBRSPEL(RE)      MOVE N'SPOTS FOR CURRENT DAY                 
*                                                                               
FD4      LR    R4,R2                                                            
         BCTR  R4,0                                                             
         LR    R8,R4                                                            
         MHI   R8,6                                                             
         LR    RE,R5               R8=A(DAY'S DATE)                             
         AHI   RE,CMPDATSD-TWAD                                                 
         AR    R8,RE                                                            
*                                                                               
*******  CLI   0(R8),X'FF'         HIT THE END ALREADY?                         
*******  BE    NEQXIT                                                           
*                                                                               
         GOTO1 VGETDAY,APPARM,(R8),APFULL GET DAY OF WEEK                       
         CLC   APFULL(3),BLANKS                                                 
         BH    *+6                                                              
         DC    H'0'                                                             
         ZIC   RF,0(R1)            SET DAYS ROTATION TO SINGLE DAY              
         LA    R1,X'80'                                                         
         SRL   R1,1                                                             
         BCT   RF,*-4                                                           
         STC   R1,NBRSDAYS                                                      
*                                                                               
         SLL   R4,2                                                             
         LR    RE,R5                                                            
         AHI   RE,CMPDATSP-TWAD                                                 
         AR    R4,RE                                                            
         MVC   LDTRDATE,0(R4)      SAVE DAY'S DATE                              
         XC    LABTREL,LABTREL                                                  
         B     EQXIT                                                            
*&&DO                                                                           
         ICM   R1,15,LADTREL       TEST PREVIOUS TRANSFER                       
         BZ    FD10                                                             
         USING DTREL,R1                                                         
         SR    R0,R0                                                            
*                                                                               
FD6      CLC   DTRDAY,LDTRDATE     FIND DAILY TRANSFER ELEMENT FOR              
         BE    FD8                 THIS DAY                                     
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         CLI   0(R1),DTRELCDQ                                                   
         BE    FD6                                                              
         B     FD10                                                             
*                                                                               
FD8      XC    LBTREL,LBTREL       FAKE A REGULAR BUY TRANSFER ELEMENT          
         LA    R8,LBTREL                                                        
         USING BTREL,R8                                                         
         MVI   BTRELCD,BTRELCDQ                                                 
         MVC   BTRLINE,DTRLINE                                                  
         CLI   DTRELLN,6           TEST NEW STYLE ELEM WITH DATE                
         BNH   *+14                                                             
         MVC   BTRDATE,DTRDATE     YES-DATE OF TRANSFER                         
         OI    BTRIND,BTRIDATE     INDICATE DATE IS IN ELEMENT                  
         LA    RE,BTRSPW-1(R2)                                                  
         MVC   0(1,RE),DTRSPOTS                                                 
         SR    RE,R8                                                            
         LA    RE,1(RE)                                                         
         STC   RE,BTRELLN                                                       
         ST    R8,LABTREL          SAVE ELEMENT'S ADDRESS                       
         ST    R1,LACURDTR         SAVE A(CURRENT DAILY TRANSFER ELE)           
*                                                                               
FD10     LTR   R9,R9               IF NO SPOTS                                  
         BNZ   EQXIT                                                            
         OC    LABTREL,LABTREL     AND NO PREVIOUS TRANSFER,                    
         BNZ   EQXIT                                                            
         B     NEQXIT              SKIP THIS DAY                                
         DROP  R1,R3,R8                                                         
*&&                                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* FIX BWS RECORD TO REFLECT A SINGLE EFFECTIVE COST                   *         
***********************************************************************         
FIXCOST  DS    0H                                                               
         L     RF,AIOAREA2                                                      
         USING NBRKEY,RF                                                        
         LA    RF,NBRFSTEL                                                      
         USING NBRSELD,RF                                                       
*                                                                               
         TM    MISCFLG1,MF1UP2OV                                                
         BNZ   FIX20                                                            
         CLI   LBTRIND,0           TEST FIRST PASS                              
         BNE   FIX20                                                            
         MVC   LSVCOST1,NBRSCST1   YES-SAVE EFFECTIVE COSTS AND DATES           
         MVC   LSVCOST2,NBRSCST2                                                
         MVC   LSVCOST3,NBRSCST3                                                
         MVC   LSVDAT2,NBRSEDT2                                                 
         MVC   LSVDAT3,NBRSEDT3                                                 
*                                                                               
         L     R6,AIOAREA2                                                      
         USING NBRKEY,R6                                                        
         OC    NBRKKBUY,NBRKKBUY                                                
         BNZ   FIX10                                                            
         DROP  R6                                                               
*                                                                               
         XC    NBRSCST2,NBRSCST2   CLEAR EFFECTIVE COSTS                        
         XC    NBRSCST3,NBRSCST3                                                
*****    XC    NBRSEDT2,NBRSEDT2   CAN'T CLEAR IF USING EFFECTIVE COSTS         
*****    XC    NBRSEDT3,NBRSEDT3                                                
*                                                                               
FIX10    ICM   R6,15,LASPWEL       SAVE SPOTS PER WEEK ELEMENT                  
         BZ    FIXXNO              NO SPOTS, NO COSTS NEEDED TO FIX             
*****                                                                           
         TM    LIND,LSEPDLY        DON'T EDIT LSVSPWEL IF SEP DAILY!            
         BO    FIX20               ..AND WE HAVE COST OVERRIDES                 
*****                                                                           
         ZIC   RE,1(R6)                                                         
         BCTR  RE,0                                                             
         MVC   LSVSPWEL(0),0(R6)                                                
         EX    RE,*-6                                                           
         B     FIX40                                                            
*                                                                               
FIX20    DS    0H                                                               
         MVC   NBRSCST1,LSVCOST1                                                
         CLI   LBTRIND,0           TEST FIRST PASS                              
         BE    FIX30                                                            
         MVC   NBRSCST1,LSVCOST2                                                
         CLI   LBTRIND,BTRIEC2                                                  
         BE    FIX30                                                            
         MVC   NBRSCST1,LSVCOST3                                                
         DROP  RF                                                               
*                                                                               
FIX30    ICM   R6,15,LASPWEL       DID WE HAVE A SPOTS PER WEEK ELEM?           
         BZ    FIXXNO              NO, NOTHING FOR THIS COST                    
*****                                                                           
         TM    LIND,LSEPDLY        DON'T EDIT LSVSPWEL IF SEP DAILY!            
         BO    FIX40               ..AND WE HAVE COST OVERRIDES                 
*****                                                                           
         ZIC   RE,1(R6)            RESTORE SPOTS PER WEEK ELEMENT               
         BCTR  RE,0                                                             
         MVC   0(0,R6),LSVSPWEL                                                 
         EX    RE,*-6                                                           
*                                                                               
         USING NBRSPEL,R6                                                       
FIX40    LA    R8,1                SET UP FOR THE BXLE                          
         ZIC   R9,1(R6)                                                         
         AR    R9,R6                                                            
         ZIC   RE,CMPNWKS                                                       
         LA    RE,NBRSPSPW(RE)                                                  
         CR    RE,R9                                                            
         BNL   *+6                                                              
         LR    R9,RE                                                            
         BCTR  R9,0                                                             
*                                                                               
         LA    R6,NBRSPSPW         REMOVE SPOTS NOT IN EFF DATE RANGE           
         LR    R1,R5                                                            
         AHI   R1,CMPDATSP-TWAD                                                 
         MVI   APBYTE,0                                                         
*                                                                               
FIX60    TM    MISCFLG1,MF1UP2OV   UPTO OVERRIDE COSTS?                         
         BNZ   FIX90               YES                                          
*                                                                               
         CLI   0(R6),0             NO SPOTS FOR THIS WEEK?                      
         BE    FIX160              NONE, CHECK THE NEXT WEEK                    
         CLI   LBTRIND,0           NO, REGULAR/EFFECTIVE COSTS                  
         BNE   FIX62                                                            
         OC    LSVBDAT2,LSVBDAT2                                                
         BZ    FIX70                                                            
         CLC   LSVBDAT2,2(R1)                                                   
         BH    FIX70                                                            
         B     FIX140                                                           
*                                                                               
FIX62    CLI   LBTRIND,BTRIEC2                                                  
         BNE   FIX64                                                            
         CLC   LSVBDAT2,2(R1)                                                   
         BH    FIX140                                                           
         OC    LSVBDAT3,LSVBDAT3                                                
         BZ    FIX70                                                            
         CLC   LSVBDAT3,2(R1)                                                   
         BH    FIX70                                                            
         B     FIX140                                                           
*                                                                               
FIX64    CLC   LSVBDAT3,2(R1)                                                   
         BH    FIX140                                                           
         B     FIX70                                                            
*                                                                               
*****  ALSO REMOVE SPOTS THAT ARE COST OVERRIDES                                
*                                                                               
FIX70    LR    RE,RF               A(1ST ELEMENT IN BUY REVISION)               
         XR    R0,R0                                                            
FIX72    IC    R0,1(RE)                                                         
         AR    RE,R0                                                            
         CLI   0(RE),0                                                          
         BE    FIX120                                                           
         CLI   0(RE),NBRCOELQ      COST OVERRIDE ELEMENT?                       
         BNE   FIX72                                                            
         USING NBRCOELD,RE                                                      
         CLC   NBRCODAT,0(R1)                                                   
         BL    FIX72                                                            
         CLC   NBRCODAT,2(R1)                                                   
         BH    FIX72                                                            
         CLI   0(R6),0             SKIP IF 0 SPOTS AND OVERRIDE                 
         BE    FIX72                                                            
         IC    R0,0(R6)                                                         
         AHI   R0,-1                                                            
         STC   R0,0(R6)            REDUCE NUMBER OF SPOTS BY 1                  
         B     FIX72                                                            
         DROP  RE                                                               
***************                                                                 
* ONLY COUNT SPOTS FOR OVERRIDES WITH THE CURRCOST                              
***************                                                                 
FIX90    LR    R0,R5                                                            
         AHI   R0,CMPDATSP-TWAD                                                 
         CR    R1,R0               ON THE FIRST WEEK?                           
         BNE   FIX95               NO                                           
*                                                                               
         L     RE,AEXTRAWK                                                      
         USING EXTRAWKD,RE                                                      
         XR    RF,RF                                                            
         IC    RF,CURRCOVR                                                      
         BCTR  RF,0                                                             
         MHI   RF,4                                                             
         LA    RF,COSTABLE(RF)                                                  
         MVC   CURRCOST,0(RF)                                                   
         DROP  RE                                                               
*                                                                               
         ICM   RE,15,LASPWEL       CLEAR REGULAR AND GET OVERRIDES              
         ZIC   RF,1(RE)                AT THE CURRCOST                          
         SHI   RF,NBRSPSPW-NBRSPEL+1                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         XC    0(0,R6),0(R6)                                                    
*                                                                               
FIX95    L     RE,AIOAREA2                                                      
         USING NBRKEY,RE                                                        
         LA    RE,NBRFSTEL         A(1ST ELEMENT IN BUY REVISION)               
         DROP  RE                                                               
         XR    R0,R0                                                            
FIX96    IC    R0,1(RE)                                                         
         AR    RE,R0                                                            
         CLI   0(RE),0                                                          
         BE    FIX120                                                           
         CLI   0(RE),NBRCOELQ      COST OVERRIDE ELEMENT?                       
         BNE   FIX96                                                            
         USING NBRCOELD,RE                                                      
         CLC   NBRCODAT,0(R1)                                                   
         BL    FIX96                                                            
         CLC   NBRCODAT,2(R1)                                                   
         BH    FIX96                                                            
         IC    R0,0(R6)                                                         
         AHI   R0,1                                                             
         STC   R0,0(R6)            REDUCE NUMBER OF SPOTS BY 1                  
         B     FIX96                                                            
         DROP  RE                                                               
*                                                                               
FIX120   MVI   APBYTE,1            INDICATE THERE ARE SPOTS                     
         B     FIX160                                                           
*                                                                               
FIX140   MVI   0(R6),0             REMOVE SPOTS OUTSIDE RANGE                   
*                                                                               
FIX160   LA    R1,4(R1)            PROCESS THE NEXT WEEK                        
         BXLE  R6,R8,FIX60                                                      
*                                                                               
FIXXYES  B     EQXIT                                                            
*                                                                               
FIXXNO   B     NEQXIT              NOTHING FOR THIS COST                        
         EJECT                                                                  
***********************************************************************         
* PUT BUY RECORD                                                      *         
* INPUT  : IOADDR = A(BUY RECORD)                                     *         
*          R1=A(OLD BUY RECORD BEFORE CHANGES)                        *         
***********************************************************************         
         SPACE 1                                                                
PUTBUY   DS    0H                                                               
         GOTO1 =A(PUTZBUY),RR=APRELO                                            
         B     XIT                                                              
***********************************************************************         
* ADD BUY RECORD                                                      *         
* INPUT  : IOADDR = A(RECORD BUFFER)                                  *         
***********************************************************************         
         SPACE 1                                                                
ADDREC   DS    0H                                                               
         L     R2,IOADDR                                                        
         USING BUYRECD,R2                                                       
         TM    LFLAG,LPKG          TEST PACKAGE                                 
         BZ    AREC4               NO - ADD THE RECORD                          
         TM    LFLAG,LPKGREX       YES - TEST RE-TRANSFER                       
         BO    AREC2                                                            
         CLI   LPKGMAST,0                NO - TEST PACKGE MASTER YET            
         BNE   AREC2                                                            
         MVC   LPKGMAST,BUYKBUY               NO - THIS IS THE MASTER           
         L     RE,AIOAREA4                                                      
****     LA    RF,4000                                                          
         LHI   RF,6000                                                          
         LR    R3,RF                                                            
         MVCL  RE,R2                                                            
         B     ARECX                                                            
*                                                                               
AREC2    LA    R1,LPKGSLAV         ADD LINE NUMBER TO SLAVE LIST                
         CLI   0(R1),0                                                          
         BE    *+12                                                             
         LA    R1,1(R1)                                                         
         B     *-12                                                             
         MVC   0(1,R1),BUYKBUY                                                  
*                                                                               
AREC4    OC    CUPASS,CUPASS       TEST PASSWORD PROTECT                        
         BZ    AREC6                                                            
         BAS   RE,ACTIV            ACTIVITY ELEMENT                             
*                                                                               
AREC6    DS    0H                                                               
         GOTO1 VDATCON,APPARM,(5,0),(3,BDCHG)     DATE = TODAY'S DATE           
         DS    0H                                                               
         GOTO1 AIO,FILADD                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,ADDSTA           ADD STATION TO STATION LIST                  
         TM    AFLAG1,X'80'        TEST ADDS USER                               
         BZ    *+8                                                              
         OI    3(R1),X'80'         YES-MARK STATION READY FOR ADDS SEND         
*                                                                               
         CLI   BDSEC,0             MAKE SURE SECONDS LENGTH SET                 
         BNE   ARECX                                                            
*                                                                               
         LA    R2,BWSMSGH                                                       
         XC    8(L'BWSMSG,R2),8(R2)                                             
         MVC   8(L'MSG0SPLN,R2),MSG0SPLN                                        
         OI    6(R2),X'80'                                                      
         LA    R2,BWSRECH                                                       
         DC    H'0'                                                             
         DC    C'$ABEND'                                                        
MSG0SPLN DC    C'CANNOT TRANSFER!  THERE IS A 0 SPOT LENGTH'                    
*                                                                               
ARECX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* NOTE: THIS CODE IS DUPLICATED BELOW IN ROUTINE 'ASTA'                         
*                                                                               
* ADD STATION TO LIST OF STATIONS ADDED TO OR CHANGED IN BUY FILE     *         
* OUTPUT : R1=A(STATION LIST ENTRY)                                   *         
***********************************************************************         
         SPACE 1                                                                
         USING BUYRECD,R2                                                       
ADDSTA   DS    0H                                                               
         LA    R1,LSTALIST                                                      
         LA    RF,MAXSTA                                                        
*                                                                               
ADDSTA2  OC    0(3,R1),0(R1)                                                    
         BNZ   *+14                                                             
         MVC   0(3,R1),BUYMSTA+2                                                
         B     ADDSTAX                                                          
         CLC   BUYMSTA+2(3),0(R1)                                               
         BE    ADDSTAX                                                          
         LA    R1,4(R1)                                                         
         BCT   RF,ADDSTA2                                                       
*                                                                               
         L     R5,ATWA                                                          
         LA    R2,BWSMSGH                                                       
         XC    BWSMSG,BWSMSG                                                    
         MVC   BWSMSG(L'TWOMNYST),TWOMNYST                                      
         OI    BWSMSGH+6,X'80'                                                  
         LA    R2,BWSRECH                                                       
         DC    H'0'                                                             
         DC    C'$ABEND'                                                        
*                                                                               
TWOMNYST DC    C'TOO MANY STATIONS!  TRY BY STATION.'                           
*                                                                               
ADDSTAX  BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* NOTE: THIS CODE IS DUPLICATED BELOW IN ROUTINE 'ACTV'                         
*                                                                               
* ADD OR UPDATE BUY ACTIVITY ELEMENT                                  *         
* INPUT  : R2 = A(BUY RECORD)                                         *         
***********************************************************************         
         SPACE 1                                                                
ACTIV    NTR1  ,                                                                
         USING BUYRECD,R2                                                       
         OC    CUPASS,CUPASS       TEST PASSWORD PROTECT ACTIVE                 
         BZ    ACTIVX                                                           
         TM    CUSTAT,CUSPER       AND IT'S A PERSONAL PASSWORD                 
         BZ    ACTIVX                                                           
         SR    R0,R0               YES - LOOK FOR ACTIVITY ELEMENT              
         LA    R4,BDELEM                                                        
         USING ACTVELEM,R4                                                      
*                                                                               
ACTIV2   IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),X'99'                                                      
         BE    ACTIV4                                                           
         CLI   0(R4),0                                                          
         BNE   ACTIV2                                                           
         LA    R4,APELEM           NOT FOUND - ADD ACTIVITY ELEM                
         MVI   0(R4),X'99'                                                      
         MVI   1(R4),12                                                         
         XC    ACTVADD(10),ACTVADD                                              
         MVC   ACTVADD,CUPASS                                                   
         MVC   ACTVADD+2(3),ASBDAT                                              
         GOTO1 AADDELS,BUYREC                                                   
         B     ACTIVX                                                           
*                                                                               
ACTIV4   MVC   ACTVCHG,CUPASS      FOUND - UPDATE ACTIVITY ELEMENT              
         MVC   ACTVCHG+2(3),ASBDAT                                              
*                                                                               
ACTIVX   B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD A REGULAR BUY ELEMENT                                *         
* R1=A(BUY RECORD)                                                    *         
* APELEM CONTAINS BUY ELEMENT                                         *         
***********************************************************************         
         SPACE 1                                                                
ADDREG   NTR1  ,                                                                
         LR    R2,R1                                                            
         XR    R0,R0                                                            
         XR    R8,R8                                                            
         USING BUYRECD,R2                                                       
         LA    R3,BDELEM                                                        
*                                                                               
         MVI   APHALF,X'06'        NON-POL ELEMENTS                             
         MVI   APHALF+1,X'08'         RANGE FROM X'06'-X'08'                    
         TM    LIND,LPOL                                                        
         BZ    AREG2                                                            
         MVI   APHALF,X'0B'        POL ELEMENTS                                 
         MVI   APHALF+1,X'0D'         RANGE FROM X'0B'-X'0D'                    
*                                                                               
AREG2    IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         CLI   0(R3),0             END OF RECORD?                               
         BE    AREG4                                                            
         CLC   0(1,R3),APHALF                                                   
         BL    AREG2                                                            
         CLC   0(1,R3),APHALF+1                                                 
         BH    AREG2                                                            
         LR    R8,R3                                                            
         USING REGELEM,R3                                                       
         CLC   RDATE,APELEM+RDATE-REGELEM     SAME DATE?                        
         BL    AREG2                                                            
         BH    AREG8               STRAIGHT ADD                                 
***************                                                                 
* WE HAVE THE SAME DATE, MINUS HERE ONLY APPLIES TO POL SPOTS                   
***************                                                                 
         CLI   0(R3),X'0C'         POOL OTO?                                    
         BNE   AREG2                                                            
         TM    RSTATUS,X'80'       -OTO?                                        
         BZ    AREG4               +OTO, ADDED SPOTS SHOULD BE AFTER            
         OC    RPAY,RPAY           IS THIS PAID?                                
         BNZ   AREG2               YES, SKIP THIS ELEMENT                       
*                                                                               
         LA    RF,BDELEM           GO BACK AND LOOK FOR THE MATCHING            
AREG3    XR    R0,R0                   SPOT THAT HAS BEEN MINUSED               
         IC    R0,1(RF)                                                         
         AR    R0,RF                                                            
         CR    R0,R3               NEXT ELEMENT IS THE -OTO?                    
         BE    AREG3B              YES                                          
         LR    RF,R0                                                            
         B     AREG3                                                            
*                                                                               
AREG3B   XR    R0,R0               LEAVE ALL MAKEGOODS AND PREEMPTS             
         B     AREG2                   ALONE                                    
****     TM    RSTATUS-REGELEM(RF),X'02'   -OTO USED FOR MAKEGOOD?              
****     BNZ   AREG2                       YES, LEAVE MAKEGOODS ALONE           
****     NI    RSTATUS-REGELEM(RF),X'FF'-X'40'  TAKE OFF SPOT MINUSED           
****     B     AREG9                            AND DELETE THE -OTO             
*                                                                               
AREG4    CR    R3,R8               ADD SPOT AT THIS ADDR IF BOTH EQUAL          
         BE    AREG8                                                            
*                                                                               
         LTR   R3,R8                                                            
         BZ    AREG10                                                           
*                                                                               
AREG6    IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         CLI   0(R3),0                                                          
         BE    AREG8                                                            
         CLI   0(R3),X'10'                                                      
         BL    AREG8                                                            
         CLI   0(R3),X'19'                                                      
         BNH   AREG6                                                            
** ADDS THE ELEMENT  (NOTE: ARECUPA)                                            
AREG8    GOTO1 ARECUPA,APPARM,BUYREC,(R3)                                       
         B     AREGX                                                            
** DELETES THE ELEMENT                                                          
AREG9    GOTO1 VRECUP,APPARM,(0,BUYREC),(R3)                                    
         B     AREGX                                                            
*                                                                               
AREG10   GOTO1 AADDELS,BUYREC                                                   
*                                                                               
AREGX    XIT1                                                                   
         DROP  R2,R3                                                            
         EJECT                                                                  
         LTORG                                                                  
         SPACE 1                                                                
BLANKS   DC    CL80' '                                                          
         EJECT                                                                  
**********************************************************************          
* READ CLIENT RECORD AND GET 3 CHARACTER PIGGYBACK PRODUCT CODE      *          
**********************************************************************          
GETPIG   NTR1  BASE=*,LABEL=*                                                   
         LA    R4,IOKEY            BUILD KEY OF CLIENT RECORD                   
         USING CLTHDRD,R4                                                       
         XC    CKEY,CKEY                                                        
         MVI   CKEYTYPE,0                                                       
         MVC   CKEYAM,BAGYMD                                                    
         MVC   CKEYCLT,BCLT                                                     
         GOTO1 AIO,FILRD1          READ CLIENT RECORD                           
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,AIOAREA1                                                      
         LA    R1,CLIST            LOOK FOR PASSIVE PRODUCT CODE                
*                                                                               
GETPIG10 CLI   0(R1),C'A'                                                       
         BNL   *+6                                                              
         DC    H'0'                                                             
         CLC   APBYTE,3(R1)                                                     
         BE    *+12                                                             
         LA    R1,4(R1)                                                         
         B     GETPIG10                                                         
         MVC   APFULL,0(R1)         FOUND - PRODUCT MNEMONIC                    
         XIT1                                                                   
***********************************************************************         
* ADD/DELETE POOL SPOT ELEMENTS                                                 
* INPUT : R2 = A(BUY RECORD)                                                    
*         R5 = A(WEEK TABLE ENTRY)                                              
*         R8 = A(WEEK POSITION IN SCHEDULE TABLE ENTRY)                         
*         R9 = A(CAMPAIGN DATES TABLE ENTRY)                                    
*         LFLAG = LFSTWK FOR THE FIRST WEEK                                     
* OUTPUT: FVMSGNO = FVRECOF IF RECORD OVERFLOW                                  
***********************************************************************         
POOLSPOT NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R4,AEXTRAWK                                                      
         USING EXTRAWKD,R4                                                      
         BAS   RE,FINDOVRS                                                      
*                                                                               
         LR    R3,R5               R3 = A(WEEK TABLE ENTRY)                     
*                                                                               
***                                                                             
         TM    LIND,LSEPDLY        WE DOING SEP DAILY?                          
         BNO   PSPT05               - NOPE, CONTINUE NORMALLY                   
         CLC   LDTRDAT2,2(R3)      CURRENT DAY'S DATE                           
         BE    PSPT05               - YUP IT IS, CONTINUE                       
***                                ...CUZ IT'S SEPARATE DAILY!                  
         CR    RB,RB                                                            
         B     PSPTX               EXIT, BUT CONDITION =                        
***                                                                             
PSPT05   NI    MISCFLG1,X'FF'-MF1OVCST                                          
*                                                                               
         TM    MISCFLG1,MF1UP2OV                                                
         BNZ   PSPT40                                                           
*                                                                               
         XC    CURRCOST,CURRCOST   LEAVE COST ALONE, DEFAULT IS BUY'S           
         XR    R5,R5               NUMBER OF CURRENT BUY SPOTS                  
         IC    R5,0(R8)                                                         
         CLI   BUYCSNUM,0                                                       
         BE    PSPT20                                                           
         LA    RE,BUYCSTBL                                                      
         XR    RF,RF                                                            
         IC    RF,BUYCSNUM                                                      
*                                                                               
PSPT10   XR    R0,R0               R0 = # OF SPOTS FOR THIS OVERRIDE            
         IC    R0,L'NBRCOCST(RE)                                                
         SR    R5,R0                                                            
         BNM   *+6                                                              
         DC    H'0'                                                             
         LA    RE,5(RE)                                                         
         BCT   RF,PSPT10                                                        
*                                                                               
PSPT20   XR    R6,R6               NUMBER OF PROPOSED BWS SPOTS                 
         IC    R6,1(R8)                                                         
         CLI   NBRCSNUM,0                                                       
         BE    PSPT35                                                           
         LA    RE,NBRCSTBL                                                      
         XR    RF,RF                                                            
         IC    RF,NBRCSNUM                                                      
*                                                                               
PSPT30   XR    R0,R0               R0 = # OF SPOTS FOR THIS OVERRIDE            
         IC    R0,L'NBRCOCST(RE)                                                
         SR    R6,R0                                                            
         BNM   *+6                                                              
         DC    H'0'                                                             
         LA    RE,5(RE)                                                         
         BCT   RF,PSPT30                                                        
***************                                                                 
* AT THIS POINT R5 AND R6 CONTAIN THE NUMBER OF SPOTS WITHOUT ANY COST          
* OVERRIDES                                                                     
***************                                                                 
PSPT35   CR    R5,R6               ANY CHANGE IN THE NUMBER?                    
         BE    PSPT40              NONE                                         
         STC   R5,0(R8)            MODIFY # OF SPOTS IN WEEK TABLE              
         STC   R6,1(R8)                                                         
         BAS   RE,MDFYBUY          MODIFY THE BUY RECORD                        
***********************************                                             
* NOW WE NEED TO PARSE THROUGH THE NBRCSTBL TO MODIFY OVERRIDES                 
***********************************                                             
PSPT40   OI    MISCFLG1,MF1OVCST   WE'RE DOING OVERRIDE COSTS NOW               
*                                                                               
         CLI   NBRCSNUM,0          ANY PROPOSED COST OVERRIDES?                 
         BE    PSPT100             NONE, CHECK IF COST OVERRIDES IN BUY         
*                                                                               
         MVI   NBRCSPTR,0          START WITH 1ST OVERRIDE                      
         XR    R0,R0                                                            
PSPT50   MHI   R0,5                                                             
         LA    RE,NBRCSTBL                                                      
         AR    RE,R0                                                            
         MVC   CURRCOST,0(RE)      SAVE THE COST OVERRIDE WE'RE                 
         XR    R6,R6                  LOOKING FOR                               
         IC    R6,L'NBRCOCST(RE)   R6 = PROPOSED NUMBER OF SPOTS                
*                                                                               
         XR    R5,R5                                                            
*&&DO                                                                           
         TM    LIND,LSEPDLY                                                     
         BZ    PSPT53                                                           
         CLC   LSPTDATE,2(R3)      IS IT ORIGINAL ON BUY SPOT DATE?             
         BE    PSPT53               - YUP, CONTINUE NORMALLY                    
*                                                                               
****  WE'RE FAKING OUT THE PROGRAM CUZ THIS IS NEW BUY LINE (SEP DAILY)         
         MVC   BUYCSNUM,NBRCSNUM   FAKE OUT PROGRAM CUZ THIS NEW BUY!           
         MVI   BUYCSPTR,0          RESET THE DISP COUNTER                       
         OC    BUYCSTBL,BUYCSTBL   ANYTHING IN HERE?                            
         BNZ   *+10                 - YUP, FORGET IT                            
         MVC   BUYCSTBL,CURRCOST    - NOPE, PUT THE COSTOVRD THERE              
****                                                                            
*                                                                               
         LA    RF,BUYCSTBL         ATTEMPT TO FOLLOW LOGIC B4 PSPT55            
         B     PSPT60                                                           
*&&                                                                             
PSPT53   CLI   BUYCSNUM,0          ANY COST OVERRIDES IN BUY?                   
         BE    PSPT70              NO, ZERO SPOTS AT THIS COST IN BUY           
         XR    RE,RE                                                            
         IC    RE,BUYCSNUM                                                      
         LA    RF,BUYCSTBL         PARSE THROUGH BUY OVERRIDES FOR THIS         
PSPT55   CLC   CURRCOST,0(RF)          SPECIFIC COST                            
         BE    PSPT60                                                           
         LA    RF,5(RF)                                                         
         BCT   RE,PSPT55                                                        
         B     PSPT70              NO MATCH, ZERO SPOTS                         
*                                                                               
PSPT60   IC    R5,L'NBRCOCST(RF)                                                
*                                                                               
PSPT70   CR    R5,R6               ANY CHANGE IN THE NUMBER?                    
         BE    PSPT80              NONE                                         
         STC   R5,0(R8)            MODIFY # OF SPOTS IN WEEK TABLE              
         STC   R6,1(R8)                                                         
         BAS   RE,MDFYBUY          MODIFY THE BUY RECORD                        
*                                                                               
PSPT80   XR    R0,R0                                                            
         IC    R0,NBRCSPTR                                                      
         AHI   R0,1                                                             
         STC   R0,NBRCSPTR                                                      
         CLC   NBRCSPTR,NBRCSNUM                                                
         BL    PSPT50                                                           
***********************************                                             
* NOW WE NEED TO PARSE THROUGH THE BUYCSTBL TO DELETE OVERRIDES                 
***********************************                                             
PSPT100  CLI   BUYCSNUM,0          ANY BUY COST OVERRIDES?                      
         BE    PSPTX               NONE, CHECK IF ANY IN BUY                    
*                                                                               
         MVI   BUYCSPTR,0          START WITH FIRST OVERRIDE                    
         XR    R0,R0                                                            
PSPT110  MHI   R0,5                                                             
         LA    RE,BUYCSTBL                                                      
         AR    RE,R0                                                            
         MVC   CURRCOST,0(RE)      SAVE OVERRIDE COST WE'RE LOOKING FOR         
         XR    R5,R5                                                            
         IC    R5,L'NBRCOCST(RE)   R5 = NUMBER OF SPOTS AT THAT COST            
*                                                                               
         XR    R6,R6                                                            
         CLI   NBRCSNUM,0          ANY PROPOSED OVERRIDES?                      
         BE    PSPT120             NONE, DELETE # OF SPOTS FROM BUY             
         XR    RE,RE                                                            
         IC    RE,NBRCSNUM                                                      
         LA    RF,NBRCSTBL                                                      
PSPT115  CLC   CURRCOST,0(RF)      WE HAVE A MATCH WITH PROPOSED?               
         BE    PSPT130             YES, WE WENT THROUGH THESE ALREADY           
         LA    RF,5(RF)                                                         
         BCT   RE,PSPT115                                                       
*                                                                               
PSPT120  CR    R5,R6               ANY CHANGE IN THE NUMBER?                    
         BE    PSPT130             NONE, NEXT COST                              
         STC   R5,0(R8)                                                         
         STC   R6,1(R8)                                                         
         BAS   RE,MDFYBUY          R6 SHOULD ALWAYS BE 0 HERE                   
*                                                                               
PSPT130  XR    R0,R0                                                            
         IC    R0,BUYCSPTR                                                      
         AHI   R0,1                                                             
         STC   R0,BUYCSPTR                                                      
         CLC   BUYCSPTR,BUYCSNUM   LOOP THROUGH ALL BUY OVERRIDES               
         BL    PSPT110                                                          
         B     PSPTX                                                            
***********************************                                             
* DOING COST OVERRIDE SPOTS                                                     
***********************************                                             
PSPT200  DS    0H                                                               
*                                                                               
PSPTX    XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* ADD/DELETE POOL SPOT ELEMENTS                                                 
*                                                                               
* ON ENTRY:    (R5)                NUMBER OF SPOTS FOR THIS WEEK                
*              (R6)                PROPOSED NUMBER OF SPOTS                     
*              (R4)                A(EXTRA WORK AREA)                           
*              MISCFLG1            MF1OVCST=1, DOING OVERRIDES                  
*              CURRCOST            OVERRIDE COST                                
*                                                                               
* ON EXIT:     FVMSGNO             FVRECOF IF RECORD OVERFLOW                   
***********************************************************************         
MDFYBUY  NTR1                                                                   
*                                                                               
         SR    R5,R6               COMPARE BUY SPTS TO BWS SPTS                 
         BZ    MDFBYX              EQUAL - NONE TO ADD OR DELETE                
         BP    MDFBY40             HIGH - R5 = NUMBER SPTS TO DELETE            
         LPR   R5,R5               LOW  - R5 = NUMBER SPTS TO ADD               
         ICM   R6,1,0(R8)          R6 = NUMBER BUY ELEMS TO READ BEFORE         
         BNZ   MDFBY40                  ADD/DELETE                              
         OI    LFLAG,LNEWEEK       NO BUY SPOTS - INDICATE A NEW WEEK           
         B     MDFBY120                                                         
*                                                                               
         USING BUYRECD,R2               ADD/DELETE                              
MDFBY40  XR    RF,RF                                                            
         LA    R1,BDELEM                                                        
         NI    MISCFLG1,X'FF'-MF1DEL0C  CAN'T DELETE OTO'S YET                  
         XC    FRSTOFWK,FRSTOFWK                                                
*                                                                               
MDFBY50  CLI   0(R1),0             FIND BUY ELEMS FOR THIS WEEK                 
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   0(R1),X'0B'         POOL BUY ELEMENT                             
         BE    *+12                                                             
         CLI   0(R1),X'0C'                                                      
         BNE   MDFBY70                                                          
         USING REGELEM,R1                                                       
         TM    LFLAG,LFSTWK+LWEEKLY  TEST WEEKLY SKED AND FIRST WEEK            
         BNO   *+18                                                             
         CLC   RDATE,CMPSTMNP      YES - COMPARE TO CAMPAIGN START MON          
         BL    MDFBY70                                                          
         B     *+14                                                             
         CLC   RDATE,0(R9)         TEST BUY ELEM IN THIS WEEK                   
         BL    MDFBY70                                                          
         CLC   RDATE,2(R9)                                                      
         BH    MDFBY70                                                          
*                                                                               
         OC    FRSTOFWK,FRSTOFWK   A(1ST ELEM FOR THAT WEEK)                    
         BNZ   *+8                                                              
         ST    R1,FRSTOFWK                                                      
*                                                                               
         TM    RSTATUS,X'C0'       SPOT HAS BEEN MINUSED?                       
         BNZ   MDFBY70             CAN'T INCLUDE THIS AS PART OF (R6)           
*                                                                               
         TM    MISCFLG1,MF1OVCST   DOING COST OVERRIDES?                        
         BZ    MDFBY54             NO                                           
         TM    RSTATUS,X'20'       YES, IS THIS AN OVERRIDE COST?               
         BZ    MDFBY70                  NO                                      
****  THIS CODE IS ONLY FOR SEPARATE DAILY!!                                    
         TM    LIND,LSEPDLY                                                     
         BZ    MDFBY51C             - NOPE DAILY, NORMAL                        
         CLC   LDTRDAT2,RDATE      IS IT THE SAME DATE?                         
         BNE   MDFBY70              - NOPE, GET OUTTA HERE                      
****                                  MHC  04/12/04                             
MDFBY51C XR    RE,RE                    YES                                     
         ICM   RE,7,RPCOST                                                      
         L     RF,AIOAREA3                                                      
         TM    BDCIND2-BUYKEY(RF),X'20'       CANADIAN AGENCY BUY?              
         BNZ   MDFBY52                        YES                               
         TM    BDCIND2-BUYKEY(RF),X'10'       COST IN US DOLLARS                
         BZ    MDFBY52                                                          
         MHI   RE,100                                                           
MDFBY52  CLM   RE,15,CURRCOST      IS IT THE COST WE'RE LOOKING FOR?            
         BE    MDFBY58             YES                                          
         B     MDFBY70                                                          
*********                                                                       
* NOT DOING COST OVERRIDES                                                      
*********                                                                       
MDFBY54  TM    RSTATUS,X'20'       YES                                          
         BNZ   MDFBY70                                                          
*                                                                               
MDFBY58  L     RE,ATWA                                                          
         AHI   RE,SAVAREA-TWAD                                                  
         USING SAVAREA,RE                                                       
         CLC   RDATE,SVELKSDT                                                   
         BL    MDFBY60                                                          
         CLC   RDATE,SVELKNDT                                                   
         BH    MDFBY60                                                          
LCKDSPT2 L     R5,ATWA             R5 WASN'T POINTING HERE                      
         LA    R2,BWSMSGH                                                       
         XC    8(L'BWSMSG,R2),8(R2)                                             
         MVC   8(L'SPTSLCK2,R2),SPTSLCK2                                        
         OI    6(R2),X'80'                                                      
         LA    R2,BWSKEYH                                                       
         DC    H'0'                                                             
         DC    C'$ABEND'                                                        
SPTSLCK2 DC    C'CANNOT TRANSFER, SPOTS IN LOCKED PERIOD!'                      
         DROP  RE                                                               
*                                                                               
MDFBY60  LTR   R6,R6               YES - TEST DELETING ALL                      
         BZ    MDFBY90                   YES                                    
         BCT   R6,*+8                    NO - READ (R6) OF THEM                 
         B     MDFBY80                                                          
MDFBY70  XR    RF,RF               NEXT BUY ELEMENT                             
         IC    RF,1(R1)                                                         
         AR    R1,RF                                                            
         B     MDFBY50                                                          
*                                                                               
MDFBY80  XR    RF,RF               POINT TO NEXT ELEMENT                        
         IC    RF,1(R1)                                                         
         AR    R1,RF               R1 = A(INSERTION/DELETION POINT)             
         CLI   0(R1),X'10'         GO BEYOND ALL ELEMENTS ASSOCIATED            
         BL    MDFBY90             WITH LAST BUY ELEMENT                        
         CLI   0(R1),X'18'                                                      
         BNH   MDFBY80                                                          
*                                                                               
MDFBY90  CLC   0(1,R8),1(R8)       TEST DELETING SPOTS                          
         BNH   MDFBY120                                                         
*                                  YES - DELETE THE NEXT (R5) ELEMS             
MDFBY100 CLI   0(R1),X'0B'         THIS ELEM MUST BELONG TO THIS WEEK           
         BE    MDFBY105                                                         
         CLI   0(R1),X'0C'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    MISCFLG1,MF1DEL0C   OKAY TO DELETE X'0C' ELEMS?                  
         BNZ   MDFBY105            YES                                          
         OI    MISCFLG1,MF1DEL0C   OKAY TO DELETE X'0C' ELEMS?                  
         L     R1,FRSTOFWK                                                      
*                                                                               
MDFBY105 TM    LFLAG,LFSTWK+LWEEKLY  TEST WEEKLY SKED AND FIRST WEEK            
         BNO   *+16                                                             
         CLC   RDATE,CMPSTMNP      YES - COMPARE TO CAMPAIGN START MON          
         BNL   *+18                                                             
         DC    H'0'                                                             
         CLC   RDATE,0(R9)                                                      
         BNL   *+6                                                              
         DC    H'0'                                                             
         CLC   RDATE,2(R9)                                                      
         BNH   MDFBY110                                                         
         DC    H'0'                                                             
*                                                                               
MDFBY110 TM    RSTATUS,X'C0'       SPOT HAS BEEN MINUSED?                       
         BNZ   MDFBY80                                                          
***   WE NEED TO CHECK THAT IT'S THE SAME OVERRIDE COST!!!                      
         XR    RE,RE                                                            
         ICM   RE,7,RPCOST                                                      
         CLM   RE,15,CURRCOST      SAME OVERRIDE COST?                          
         BNE   MDFBY80              - NOPE, GET THE NEXT ELEMENT                
***                                      MHC  04/14/04                          
         OC    RPAY,RPAY           PAID SPOT?                                   
         BZ    MDFBY115            NO, JUST DELETE THAT GROUP OF ELEM           
         BCT   R5,MDFBY80          CAN'T BE DELETED, BUT TREAT AS IF            
         B     MDFBYX              DONE WITH THIS WEEK                          
*                                                                               
MDFBY115 MVI   0(R1),FF            DELETE THE ELEMENT                           
         MVI   APELEM,FF                                                        
         ST    R1,APFULL                                                        
         GOTO1 ADELELS,BUYRECD                                                  
         L     R1,APFULL                                                        
         CLI   0(R1),X'0D'         DELETE ALL ASSOCIATED ELEMENTS               
         BL    *+12                                                             
         CLI   0(R1),X'18'                                                      
         BNH   MDFBY115                                                         
         BCT   R5,MDFBY100         DELETE (R5) TIMES                            
         B     MDFBYX              DONE WITH THIS WEEK                          
         DROP  R1                                                               
*                                                                               
MDFBY120 XC    APELEM,APELEM       ADD SPOTS - BUILD BUY ELEMENT                
         LA    R6,APELEM                                                        
****  THIS CODE IS ONLY FOR SEPARATE DAILY!!                                    
         TM    LIND,LSEPDLY                                                     
         BZ    MDFBY123             - NOPE DAILY, NORMAL                        
         CLC   LDTRDAT2,2(R3)      IS IT THE SAME DATE?                         
         BNE   MDFBYX               - NOPE, GET OUTTA HERE                      
****                                  MHC  04/12/04                             
         USING REGELEM,R6                                                       
MDFBY123 MVI   RCODE,11                                                         
         MVC   RDATE,2(R3)         BUY DATE                                     
*                                                                               
         L     RE,ATWA                                                          
         AHI   RE,SAVAREA-TWAD                                                  
         USING SAVAREA,RE                                                       
         CLC   RDATE,SVELKSDT                                                   
         BL    MDFBY130                                                         
         CLC   RDATE,SVELKNDT                                                   
         BNH   LCKDSPT2                                                         
         DROP  RE                                                               
*                                                                               
MDFBY130 MVC   RPCOST,5(R3)        SPOT COST                                    
         TM    4(R3),X'C0'                                                      
         BZ    *+8                                                              
         OI    RSTATUS,X'20'       RATE OVERRIDE                                
*                                                                               
         TM    MISCFLG1,MF1OVCST   DOING COST OVERRIDES?                        
         BZ    MDFBY138            NO                                           
         OI    RSTATUS,X'20'       YES, RATE OVERRIDE                           
         L     RF,AIOAREA3                                                      
         ICM   RE,15,CURRCOST                                                   
         BZ    MDFBY134                                                         
         TM    BDCIND2-BUYKEY(RF),X'20'       CANADIAN AGENCY BUY?              
         BNZ   MDFBY134                       YES                               
         TM    BDCIND2-BUYKEY(RF),X'10'       COST IN US DOLLARS                
         BZ    MDFBY134                                                         
         CVD   RF,APDUB                                                         
         SRP   APDUB,64-2,5        DIVIDE BY 100 AND ROUND                      
         CVB   RF,APDUB                                                         
*                                                                               
MDFBY134 STCM  RE,7,RPCOST                                                      
         CLC   RPCOST,BDCOST-BUYKEY(RF)                                         
         BNE   MDFBY138                                                         
         XC    RPCOST,RPCOST                                                    
         NI    RSTATUS,X'FF'-X'20'   NOT RATE OVERRIDE ANYMORE                  
*                                                                               
MDFBY138 CLI   CMPPRD1,0           TEST PIGGYBACKS                              
         BNE   MDFBY140                                                         
         CLI   BPRD,FF             NO - TEST FOR CAMPAIGN PRODUCT=POL           
         BE    MDFBY150                                                         
         MVC   RPPRD,BPRD          NO - ALLOCATED PRD IS CAMPAIGN PRD           
         MVC   RPTIME,BDSEC             TIME SHARE IS SPOT LENGTH               
         MVI   RLEN,14                                                          
         B     MDFBY160                                                         
*                                                                               
MDFBY140 MVI   RLEN,18             PIGGYBACKS- SET ACTIVE AND PASSIVE           
         MVC   RPPRD,CMPPRD1                   PRODUCTS AND TIME SHARES         
         MVC   RPTIME,CMPLEN1                                                   
         MVC   RPPRD+L'RPALLOC(1),CMPPRD2                                       
         MVC   RPTIME+L'RPALLOC(1),CMPLEN2                                      
         TM    CMPIND,CMPIFR1      TEST FREE RIDER - BRAND 1 PAYS ALL           
         BZ    *+8                                                              
         OI    RSTATUS,RSB1PALQ    X'08' - BRAND 1 PAYS ALL (PIGGYBACK)         
         B     MDFBY160                                                         
*                                                                               
MDFBY150 MVI   RLEN,10             CAMPAIGN PRODUCT = POOL                      
         CLI   INOPRD,0            TEST FOR PRODUCT OPTION                      
         BE    MDFBY160                                                         
         MVI   RLEN,14                                                          
         MVC   RPPRD,INOPRD        YES - ALLOCATED PRD FROM PRD OPTION          
         MVC   RPTIME,BDSEC              TIME SHARE = SPOT LENGTH               
         CLI   INOPRD+1,0                TEST FOR PIGGYBACK                     
         BE    MDFBY160                                                         
         MVI   RLEN,18                                                          
         MVC   RPTIME,INOPRD+2           YES - SET THE TIME SHARES              
         MVC   RPPRD+L'RPALLOC(1),INOPRD+1     AND SECONDARY PRODUCT            
         MVC   RPTIME+L'RPALLOC(1),INOPRD+3                                     
*&&DO                                                                           
MDFBY160 TM    LFLAG,LNEWEEK       TEST NEW WEEK                                
         BZ    MDFBY180                                                         
*&&                                                                             
MDFBY160 DS    0H                                                               
MDFBY170 LA    R1,BUYREC           YES - ADD THE ELEMENT                        
         BAS   RE,ADDREG                                                        
         BNE   MDFBYX                    EXIT FOR RECORD OVERFLOW               
         BCT   R5,MDFBY170               (R5) TIMES                             
         B     MDFBYX                                                           
*&&DO                              NO -                                         
MDFBY180 ST    R1,APPARM+4         R1 = A(INSERTION POINT)                      
         GOTO1 ARECUPA,APPARM,BUYREC    ADD THE ELEMENT                         
         BNE   MDFBYX              EXIT FOR RECORD OVERFLOW                     
         L     R1,APPARM+4                                                      
         ZIC   RE,RLEN             NEXT INSERTION POINT IS BEYOND               
         AR    R1,RE               ELEMENT JUST ADDED                           
         BCT   R5,MDFBY180         ADD (R5) ELEMENTS                            
*&&                                                                             
MDFBYX   XIT1                                                                   
         DROP  R2,R4,R6                                                         
         EJECT                                                                  
***********************************************************************         
* FINDS THE OVERRIDES IN THE BUY REVISION RECORD AND THE BUY RECORD             
* ON ENTRY:    (R9)                A(CURRENT WEEK - BINARY START/END)           
*              AIOAREA2            A(BUY REVISION RECORD)                       
*              AIOAREA3            A(BUY RECORD)                                
***********************************************************************         
FINDOVRS NTR1                                                                   
         L     R4,AEXTRAWK                                                      
         USING EXTRAWKD,R4                                                      
*                                                                               
         MVI   NBRCSNUM,0          BUY REVISION OVERRIDES                       
         XC    NBRCSTBL,NBRCSTBL                                                
         MVI   BUYCSNUM,0          BUY COST OVERRIDES                           
         XC    BUYCSTBL,BUYCSTBL                                                
*                                                                               
         TM    MISCFLG1,MF1UP2OV                                                
         BZ    FNDOVX                                                           
*                                                                               
         LA    RE,NBRCSTBL                                                      
         L     R2,AIOAREA2                                                      
         USING NBRKEY,R2                                                        
         LA    R2,NBRFSTEL                                                      
FNDOV10  CLI   0(R2),0             DONE WITH THE BUY REVISION RECORD?           
         BE    FNDOV50             YES, NO MORE OVERRIDE ELEMENTS               
         CLI   0(R2),NBRCOELQ                                                   
         BE    FNDOV20                                                          
FNDOV15  XR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     FNDOV10                                                          
*                                                                               
         USING NBRCOELD,R2                                                      
FNDOV20  CLC   NBRCODAT,0(R9)      DATE WITHIN THE TARGET WEEK                  
         BL    FNDOV15                                                          
         CLC   NBRCODAT,2(R9)                                                   
         BH    FNDOV15                                                          
*                                                                               
         LA    R1,NBRCSTBL                                                      
         CLI   NBRCSNUM,0                                                       
         BE    FNDOV30                                                          
         XR    RF,RF                                                            
         IC    RF,NBRCSNUM                                                      
FNDOV25  CLC   NBRCOCST,0(R1)      THIS COST ALREADY IN TABLE?                  
         BE    FNDOV35             YES, INCREMENT THE NUMBER OF SPOTS           
         LA    R1,5(R1)            BUMP TO THE NEXT COST                        
         BCT   RF,FNDOV25          CHECK ALL COSTS IN TBL FOR THIS COST         
*                                                                               
FNDOV30  CLI   NBRCSNUM,MAXCOSTS                                                
         BNH   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   0(L'NBRCOCST,R1),NBRCOCST                                        
         XR    RF,RF                                                            
         IC    RF,NBRCSNUM                                                      
         LA    RF,1(RF)                                                         
         STC   RF,NBRCSNUM                                                      
         DROP  R2                                                               
*                                                                               
FNDOV35  XR    RE,RE                                                            
         IC    RE,L'NBRCOCST(R1)   INCREMENT NUMBER OF SPOTS WITH               
         LA    RE,1(RE)               THIS COST                                 
         STC   RE,L'NBRCOCST(R1)                                                
         B     FNDOV15                                                          
*                                                                               
FNDOV50  LA    RE,BUYCSTBL                                                      
         L     R3,AIOAREA3                                                      
         USING BUYKEY,R3                                                        
         LA    R3,BDELEM                                                        
FNDOV60  CLI   0(R3),0             DONE WITH THE BUY RECORD?                    
         BE    FNDOV100            YES, NO MORE OVERRIDES                       
         CLI   0(R3),X'0B'                                                      
         BL    FNDOV65                                                          
         CLI   0(R3),X'0C'                                                      
         BNH   FNDOV70                                                          
FNDOV65  XR    R0,R0                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     FNDOV60                                                          
*                                                                               
         USING REGELEM,R3                                                       
FNDOV70  TM    RSTATUS,X'C0'       MINUS OR MINUSED?                            
         BNZ   FNDOV65             DON'T CARE ABOUT THESE                       
*                                                                               
         TM    RSTATUS,X'20'       RATE OVERRIDE?                               
         BZ    FNDOV65             ONLY WANT THE OVERRIDES                      
*                                                                               
         CLC   RDATE,0(R9)         DATE WITHIN THE TARGET WEEK                  
         BL    FNDOV65                                                          
         CLC   RDATE,2(R9)                                                      
         BH    FNDOV65                                                          
*                                                                               
         XR    R0,R0                                                            
         ICM   R0,7,RPCOST                                                      
         L     RF,AIOAREA3                                                      
         TM    BDCIND2-BUYKEY(RF),X'20'       CANADIAN AGENCY BUY?              
         BNZ   FNDOV72                        YES                               
         TM    BDCIND2-BUYKEY(RF),X'10'       COST IN US DOLLARS                
         BZ    FNDOV72                                                          
         MHI   R0,100                                                           
FNDOV72  DS    0H                  R0 = ACTUAL COST IN QUESTION                 
*                                                                               
         LA    R1,BUYCSTBL                                                      
         CLI   BUYCSNUM,0                                                       
         BE    FNDOV80                                                          
         XR    RF,RF                                                            
         IC    RF,BUYCSNUM                                                      
FNDOV75  CLM   R0,15,0(R1)         THIS COST ALREADY IN TABLE?                  
         BE    FNDOV85             YES, INCREMENT THE NUMBER OF SPOTS           
         LA    R1,5(R1)            BUMP TO THE NEXT COST                        
         BCT   RF,FNDOV75          CHECK ALL COSTS IN TBL FOR THIS COST         
*                                                                               
FNDOV80  CLI   BUYCSNUM,MAXCOSTS                                                
         BNH   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         STCM  R0,15,0(R1)                                                      
         XR    RF,RF                                                            
         IC    RF,BUYCSNUM                                                      
         LA    RF,1(RF)                                                         
         STC   RF,BUYCSNUM                                                      
*                                                                               
FNDOV85  XR    RE,RE                                                            
         IC    RE,L'NBRCOCST(R1)   INCREMENT NUMBER OF SPOTS WITH               
         LA    RE,1(RE)               THIS COST                                 
         STC   RE,L'NBRCOCST(R1)                                                
         B     FNDOV65                                                          
*                                                                               
FNDOV100 DS    0H                                                               
*                                                                               
FNDOVX   XIT1                                                                   
         DROP  R3,R4                                                            
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* MAKE CHANGES TO BUY SPOT ELEMENT BUY DATES AND RATE OVERRIDES       *         
***********************************************************************         
SPOTCHNG NTR1  BASE=*,LABEL=*                                                   
         L     R2,AIOAREA3                                                      
         USING BUYRECD,R2                                                       
         LA    R4,BDELEM                                                        
         USING REGELEM,R4                                                       
         GOTO1 VDATCON,APPARM,(3,CMPND),(2,APHALF)                              
         SR    RF,RF               APHALF=CAMPAIGN END                          
*                                                                               
SPOT2    CLI   0(R4),0             SCAN ALL THE BUY ELEMENTS                    
         BE    SPOT12                                                           
         CLI   0(R4),6             NON-POOL SPOT ELEMENT                        
         BE    SPOT4                                                            
         CLI   0(R4),11            POOL SPOT ELEMENT                            
         BE    *+12                                                             
         CLI   0(R4),12                                                         
         BNE   SPOT10                                                           
*                                                                               
         TM    LCHGIND,LSTDATE     DATE CHANGES                                 
         BZ    SPOT3                                                            
         TM    RSTATUS,RSMGONLQ    X'02' MAKEGOOD ON A NEW LINE                 
         BZ    SPOT3                                                            
         XC    APWORK,APWORK                                                    
         GOTO1 VMSUNPK,APPARM,(X'80',BUYMSTA),APWORK,APWORK+4                   
         ZIC   R1,BUYKBUY                                                       
         CVD   R1,APDUB                                                         
         UNPK  APWORK+20(3),APDUB                                               
         OI    APWORK+22,X'F0'                                                  
         B     SPOT97              NO CHANGES ALLOWED                           
*                                                                               
SPOT3    TM    LCHGIND,LSLN        TEST SPOT LENGTH CHANGE                      
         BZ    SPOT4                                                            
*                                                                               
         CLI   1(R4),10            YES-TEST ONE ALLOCATED PRODUCT               
         BE    SPOT4               NONE ALLOCATED                               
         CLI   1(R4),14                                                         
         BNE   *+14                                                             
         MVC   RPTIME,BDSEC        ONE ALLOCATED, ALTER THE LENGTH              
         B     SPOT4                                                            
*                                                                               
         CLI   1(R4),18            PIGGYBACK ALLOCATED?                         
         BNE   SPOT4                                                            
         ZIC   RE,RPTIME           YES, CHECK IF WE ADD UP                      
         ZIC   RF,RPTIME+L'RPALLOC                                              
         AR    RE,RF                                                            
         CLM   RE,1,BDSEC                                                       
         BE    SPOT4                    WE DO, THEN WE'RE OKAY                  
*                                                                               
         TM    BDSEC,X'01'         SEE IF WE CAN SPLIT BDSEC EVENLY             
         BNZ   SPOT98                                                           
         CLC   RPTIME,RPTIME+L'RPALLOC                                          
         BNE   SPOT98              WE CAN'T                                     
*                                                                               
         ZIC   R1,BDSEC            HALF-AND-HALF                                
         SRL   R1,1                                                             
         STC   R1,RPTIME                                                        
         STC   R1,RPTIME+L'RPALLOC                                              
*                                                                               
SPOT4    CLI   0(R4),12            OTO'S ONLY AFFECTED BY LEN AND DATEY         
         BNE   SPOT5                   CHANGES                                  
         TM    LCHGIND,LSTDATE                                                  
         BZ    SPOT10                                                           
         MVC   RDATE,2(R6)                                                      
         B     SPOT10                                                           
*                                                                               
*POT5    TM    LCHGIND,LSTDATE+LEFFCOST                                         
SPOT5    TM    LCHGIND,LSTDATE     WE DONT' NEED LEFFCOST ANYMORE!!             
         BZ    SPOT10                                                           
         TM    CMPOPTS,CAMODLY     TEST DAILY SKED                              
         BZ    SPOT5C                                                           
         LR    RF,R5                                                            
         AHI   RF,CMPDATSP-TWAD                                                 
         CLC   RDATE,0(RF)         YES-TEST BUY DATE BEFORE FIRST DAY           
         BL    SPOT10              YES-IGNORE                                   
         B     *+14                                                             
*                                                                               
SPOT5C   CLC   RDATE,CMPSTMNP      TEST BUY DATE BEFORE CAMP START MON          
         BL    SPOT10              YES - IGNORE                                 
*                                                                               
         ZIC   R0,CMPNWKS          FIND WHICH WEEK THIS ELEMENT BELONGS         
         LA    R6,LWKTAB                                                        
         LR    R8,R5                                                            
         AHI   R8,CMPDATSP-TWAD                                                 
         LA    R9,LWKINDS                                                       
*                                                                               
SPOT6    CLC   RDATE,2(R8)         TEST ELEMENT BELONGS TO THIS WEEK            
         BH    SPOT8                                                            
         TM    0(R9),LFRZ          YES - TEST WEEK IS FROZEN                    
         BO    SPOT10                    YES - IGNORE                           
         CLC   2(2,R6),APHALF      MAKE SURE BUY DATE CHANGE NOT BEYOND         
         BH    SPOT10              CAMPAIGN END                                 
         MVC   RDATE,2(R6)         MAKE BUY DATE CHANGE                         
         CLI   RCODE,6             TEST NON-POOL BUY                            
         BE    SPOT10              YES - DONE                                   
         OC    RPAY,RPAY           TEST SPOT PAID                               
         BNZ   SPOT10                                                           
         MVC   RPCOST,5(R6)        NO-MAKE COST CHANGE                          
         NI    RSTATUS,FF-X'20'                                                 
         TM    4(R6),X'C0'         TEST THIS IS RATE OVERRIDE                   
         BZ    *+8                                                              
         OI    RSTATUS,X'20'       YES                                          
         B     SPOT10                                                           
*                                                                               
SPOT8    LA    R6,8(R6)            NEXT WEEK                                    
         LA    R8,4(R8)                                                         
         LA    R9,1(R9)                                                         
         BCT   R0,SPOT6                                                         
*                                                                               
SPOT10   ZIC   RF,1(R4)            NEXT ELEMENT                                 
         AR    R4,RF                                                            
         B     SPOT2                                                            
*                                                                               
SPOT12   TM    LCHGIND,LNEWPRD     TEST MASTER PRODUCT CHANGE                   
         BZ    SPOTYES                                                          
         CLI   BDMASPRD,0          YES-MAKE SURE WE DON'T UNALLOCATE            
         BE    SPOTYES                                                          
         MVI   APBYTE,0                                                         
         OC    INFDATES,INFDATES   TEST DATES FILTER                            
         BZ    SPOT13                                                           
         CLI   CLTPROF,C'0'        AND TRUE POL CLIENT                          
         BNE   SPOT13                                                           
         CLI   BPRD,X'FF'          AND PRD=POL                                  
         BNE   SPOT13                                                           
         CLI   INOPRD,0            AND ALLOCATING A PRODUCT                     
         BE    SPOT13                                                           
         MVI   APBYTE,1            YES-INDICATE TO LEAVE EXCLUDED               
*                                      WEEKS ALONE                              
SPOT13   XC    APHALF,APHALF                                                    
         MVC   APHALF(1),BDSEC     SPOT LENGTH = DESCRIPTION LENGTH             
         CLI   BDMASPRD+1,0        TEST PIGGYBACKS                              
         BE    SPOT14                                                           
         CLI   CMPPRD1,0           YES-LENGTHS FROM CAMPAIGN OR PRODUCT         
         BE    *+20                    OPTION                                   
         MVC   APHALF(1),CMPLEN1                                                
         MVC   APHALF+1(1),CMPLEN2                                              
         B     SPOT14                                                           
*                                                                               
         ZIC   RE,INOPRD+2         CALCULATE TOTAL TIME IN OPTIONS              
         ZIC   RF,INOPRD+3                                                      
         AR    RE,RF                                                            
         CLM   RE,1,BDSEC                                                       
         BE    SPOT13A                                                          
*                                                                               
         TM    BDSEC,X'01'         ODD # OF SECONDS?                            
         BNZ   SPOT98              YES, CAN'T SPLIT EVENLY                      
         CLC   INOPRD+2(1),INOPRD+3   EVEN SPLIT OF SECONDS?                    
         BNE   SPOT98                                                           
         ZIC   R1,BDSEC            SPLIT EVENLY IN HALF                         
         SRL   R1,1                                                             
         STC   R1,APHALF                                                        
         STC   R1,APHALF+1                                                      
         B     SPOT14                                                           
*                                                                               
SPOT13A  MVC   APHALF(1),INOPRD+2                                               
         MVC   APHALF+1(1),INOPRD+3                                             
*                                                                               
SPOT14   LA    R4,BDELEM           SCAN FOR ALL SPOT ELEMENTS                   
*                                                                               
SPOT15   CLI   0(R4),0                                                          
         BE    SPOTYES                                                          
         CLI   0(R4),11            TEST POOL SPOT ELEMENT                       
         BE    *+12                                                             
         CLI   0(R4),12                                                         
         BNE   SPOT24                                                           
         CLI   APBYTE,0            YES-TEST WEEK MAY BE EXCLUDED                
         BE    SPOT18                                                           
         LA    RE,LDTINDS          YES                                          
         LR    RF,R5                                                            
         AHI   RF,CMPDATSP-TWAD                                                 
         LA    R0,53               FIND WEEK THIS SPOT'S IN                     
*                                                                               
SPOT16   CLC   RDATE,0(RF)                                                      
         BL    SPOT17                                                           
         CLC   RDATE,2(RF)                                                      
         BH    SPOT17                                                           
         TM    0(RE),LEXCLD        TEST THIS WEEK IS EXCLUDED                   
         BO    SPOT24              YES-SKIP TO NEXT ELEMENT                     
         B     SPOT18                                                           
*                                                                               
SPOT17   LA    RE,1(RE)                                                         
         LA    RF,4(RF)                                                         
         BCT   R0,SPOT16                                                        
         B     SPOT24              BETTER NOT TO CHANGE ELEMENT                 
*                                                                               
SPOT18   CLI   1(R4),14            TEST WHETHER ELEMENT LENGTH                  
         BNE   *+16                WILL CHANGE                                  
         CLI   BDMASPRD+1,0                                                     
         BE    SPOT19                                                           
         B     SPOT20                                                           
         CLI   1(R4),18                                                         
         BNE   SPOT20                                                           
         CLI   BDMASPRD+1,0                                                     
         BE    SPOT20                                                           
*                                                                               
SPOT19   MVC   RPPRD,BDMASPRD      NO ELEMENT LENGTH CHANGE-                    
         MVC   RPTIME,APHALF       CHANGE PRODUCT(S) AND TIME(S)                
         CLI   1(R4),18                                                         
         BL    SPOT24                                                           
         MVC   RPPRD+L'RPALLOC(1),BDMASPRD+1                                    
         MVC   RPTIME+L'RPALLOC(1),APHALF+1                                     
         B     SPOT24                                                           
*                                                                               
SPOT20   XC    APELEM,APELEM       ELEMENT LENGTH WILL CHANGE-                  
         LA    R8,APELEM                                                        
         ZIC   RE,1(R4)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R8),0(R4)                                                    
         IC    R0,0(R4)                                                         
         MVI   0(R8),X'FF'         DELETE OLD ELEMENT                           
         MVI   0(R4),X'FF'                                                      
         GOTO1 ADELELS,BUYREC                                                   
         STC   R0,0(R8)                                                         
         MVI   1(R8),14                                                         
         MVC   RPPRD-REGELEM(1,R8),BDMASPRD                                     
         MVC   RPTIME-REGELEM(1,R8),APHALF                                      
         CLI   BDMASPRD+1,0                                                     
         BE    SPOT22                                                           
         MVI   1(R8),18                                                         
         MVC   RPTIME-REGELEM(1,R8),APHALF                                      
         MVC   RPPRD+L'RPALLOC-REGELEM(1,R8),BDMASPRD+1                         
         MVC   RPTIME+L'RPALLOC-REGELEM(1,R8),APHALF+1                          
*                                                                               
SPOT22   GOTO1 AADDELS,BUYREC      ADD UPDATED ELEMENT                          
         B     SPOT14              START SCAN FROM THE TOP AGAIN                
*                                                                               
SPOT24   ZIC   RF,1(R4)            NEXT ELEMENT                                 
         AR    R4,RF                                                            
         B     SPOT15                                                           
*                                                                               
SPOT97   MVC   FVMSGNO,=AL2(FVIMKGNC)  MAKEGOOD ON NEW LINE - NO CHANGE         
         XC    FVXTRA,FVXTRA          BACK TO ....                              
         MVC   FVXTRA(8),APWORK+4                                               
         LA    RF,FVXTRA+8-1                                                    
         LA    R0,FVXTRA                                                        
SPOT97A  CLI   0(RF),C' '                                                       
         BH    SPOT97B                                                          
         BCTR  RF,0                                                             
         CR    RF,R0                                                            
         BNL   SPOT97A                                                          
         DC    H'0'                DON'T MESS UP STORAGE, QSTA NOT SET?         
*                                                                               
SPOT97B  MVI   1(RF),C'*'                                                       
         MVC   2(3,RF),APWORK+20                                                
         OI    FVERRIND,FVEUNWND                                                
         B     SPOTNO                                                           
*                                                                               
SPOT98   MVC   FVMSGNO,=AL2(FVIPIG)    INVALID PIGGYBACK SLNS                   
         OI    FVERRIND,FVEUNWND                                                
         B     SPOTNO                                                           
*                                                                               
SPOTYES  SR    RC,RC                                                            
SPOTNO   LTR   RC,RC                                                            
SPOTXIT  XIT1                                                                   
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PUT BUY RECORD                                                      *         
* INPUT  : IOADDR = A(BUY RECORD)                                     *         
*          R1=A(OLD BUY RECORD BEFORE CHANGES)                        *         
***********************************************************************         
PUTZBUY  NTR1  BASE=*                                                           
         B     *+12                                                             
         DC    CL8'**PBUY**'                                                    
*                                                                               
         L     R2,IOADDR                                                        
         USING BUYRECD,R2                                                       
*                                                                               
         BRAS  RE,TSTLOCK          TEST OFFLINE DATA LOCK                       
         BE    PUTB1                                                            
         MVC   FVMSGNO,=AL2(808)                                                
         L     R5,ATWA                                                          
         LA    R1,BWSKEYH                                                       
         ST    R1,FVADDR                                                        
         B     PUTBNO                                                           
*                                                                               
PUTB1    TM    LFLAG,LPKG          TEST PACKAGE                                 
         BZ    PUTB2                                                            
         CLC   LPKGMAST,BUYKBUY    YES - TEST THIS IS THE MASTER                
         BNE   PUTB2                                                            
         L     RE,AIOAREA4                 YES - SAVE THE RECORD                
****     LA    RF,4000                                                          
         LHI   RF,6000                                                          
         LR    R3,RF                                                            
         MVCL  RE,R2                                                            
         B     PUTB9                                                            
*                                                                               
PUTB2    LR    RE,R1               R3=RE=A(OLD BUY RECORD)                      
         LR    R3,R1                                                            
         CLC   0(13,R2),0(RE)      KEYS MUST MATCH                              
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    R8,R2                                                            
         LH    R9,BUYRLEN-BUYKEY(R8)    SEE IF RECORD HAS CHANGED IN            
         LH    RF,BUYRLEN-BUYKEY(RE)    ANY WAY                                 
         CR    R9,RF                                                            
         BNE   *+10                                                             
         CLCL  R8,RE                                                            
         BE    PUTB9               NO-JUST EXIT                                 
*&&DO                                                                           
         OC    BDREP-BUYKEY(L'BDREP,R3),BDREP-BUYKEY(R3)                        
         BZ    PUTB3                                                            
*&&                                                                             
         LA    R1,BDELEM-BUYKEY(R2)   OF WHAT IT WILL BE                        
         XC    APHALF,APHALF       HOLD GREATEST SPOT DATE                      
PUTB2A   CLI   0(R1),0             END OF RECORD?                               
         BE    PUTB3               YES                                          
*                                                                               
         CLI   0(R1),X'06'                                                      
         BE    PUTB2C                                                           
         CLI   0(R1),X'07'                                                      
         BE    PUTB2C                                                           
         CLI   0(R1),X'0B'                                                      
         BE    PUTB2C                                                           
         CLI   0(R1),X'0C'                                                      
         BE    PUTB2C                                                           
*                                                                               
PUTB2B   ZIC   R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     PUTB2A                                                           
*                                                                               
         USING REGELEM,R1                                                       
PUTB2C   CLC   RDATE,APHALF        SPOT DATE > PREVIOUS SPOT DATE?              
         BNH   PUTB2D                                                           
         MVC   APHALF,RDATE        WE GOT A NEW HIGHEST SPOT DATE               
*                                                                               
PUTB2D   OC    RPAY,RPAY           WE GOT A PAID SPOT?                          
         BZ    PUTB2B                                                           
         OC    BDREP-BUYKEY(L'BDREP,R3),BDREP-BUYKEY(R3)                        
         BZ    PUTB2B                                                           
         DROP  R1                                                               
         MVC   BDREP-BUYKEY(L'BDREP,R2),BDREP-BUYKEY(R3)                        
         B     PUTB2B                                                           
*                                                                               
PUTB3    OC    APHALF,APHALF       NO SPOTS?                                    
         BZ    PUTB3B              LEAVE IT ALONE                               
         GOTO1 VDATCON,APPARM,(2,APHALF),(0,APWORK),0,0                         
*                                                                               
         TM    LFLAG,LWEEKLY       WEEKLY?                                      
         BZ    PUTB3A              NO, LEAVE DATE                               
*                                                                               
         GOTO1 VDATCON,APPARM,(2,APHALF),(0,APWORK)                             
         MVC   APHALF(1),BDSEDAY                                                
         MVC   APHALF+1(1),BDSEDAY                                              
         NI    APHALF,X'F0'        GET START DAY OF WEEK                        
         NI    APHALF+1,X'0F'      GET END DAY OF WEEK                          
         SR    RE,RE                                                            
         IC    RE,APHALF                                                        
         SRL   RE,4                                                             
         SR    RF,RF                                                            
         IC    RF,APHALF+1                                                      
         SR    RF,RE                                                            
         BNM   *+8                                                              
         AHI   RF,7                                                             
*                                                                               
         ST    RF,APPARM+8                                                      
PUTB3A   GOTO1 VADDAY,APPARM,APWORK,APWORK+6,,0                                 
         GOTO1 VDATCON,APPARM,(0,APWORK+6),(3,BDEND),0                          
*                                                                               
PUTB3B   GOTO1 VDATCON,APPARM,(5,0),(3,BDCHG)      DATE = TODAY'S DATE          
*                                                                               
         OC    CUPASS,CUPASS       TEST PASSWORD PROTECT                        
         BZ    PUTB4                                                            
         BAS   RE,ACTV             ACTIVITY ELEMENT                             
*                                                                               
PUTB4    TM    LCHGIND,LNEWPRD     TEST NEW MASPRD                              
         BZ    PUTB5                                                            
         XC    APFULL,APFULL       YES-ADD NEW PASSIVE POINTER                  
         MVC   APFULL(2),BDMASPRD                                               
         LA    R1,APFULL                                                        
         ST    R1,IODMP6                                                        
         OI    IOINDS1,IOIDMP6                                                  
*                                                                               
PUTB5    DS    0H                                                               
***  WE'RE CALLING SPBUYVAL HERE  - APWORK GETS WIPED OUT                       
         MVC   APFULL,IOADDR                                                    
         BRAS  RE,VALBUY                                                        
         BNE   PUTBNO                                                           
***  WE'RE CALLING SPBUYVAL HERE                                                
*                                                                               
PUTB5PUT GOTO1 AIO,FILPUT                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         NI    IOINDS1,255-IOIDMP6                                              
         OI    LIND,LPUTREC        INDICATE AT LEAST ONE BUYLINE CHANGE         
         BAS   RE,ASTA             ADD STATION TO STATION LIST                  
*                                                                               
         TM    AFLAG1,X'80'        TEST ADDS USER                               
         BZ    PUTB8                                                            
         TM    3(R1),X'80'         YES-TEST STATION ALREADY READY FOR           
         BO    PUTB8               ADDS SEND                                    
         LR    R4,R1               NO- SAVE A(STATION LIST ENTRY)               
         MVI   BDDAYPT,C'X'            COMPARE OLD/NEW BUY RECORDS,             
         MVI   BDDAYPT-BUYREC(R3),C'X' BUT IGNORING DAYPART, DEMO               
         XC    BDCHG,BDCHG             ELEMENTS AND UPGRADE ELEMENT             
         XC    BDCHG-BUYREC(L'BDCHG,R3),BDCHG-BUYREC(R3)                        
         LR    R1,R2                                                            
         LA    R9,2                                                             
*                                                                               
PUTB6    MVI   APELEM,2                                                         
         GOTO1 ADELELS                                                          
         MVI   APELEM,3                                                         
         BASR  RE,RF                                                            
         MVI   APELEM,X'62'                                                     
         BASR  RE,RF                                                            
         MVI   APELEM,BWSCODEQ     ALSO IGNORE BWS TRANSFER ELEMENT             
         BASR  RE,RF                                                            
         LR    R1,R3                                                            
         BCT   R9,PUTB6                                                         
         LR    RE,R3               COMPARE OLD/NEW BUY RECORDS NOW              
         LR    R8,R2                                                            
         LH    R9,BUYRLEN-BUYKEY(R8)                                            
         LH    RF,BUYRLEN-BUYKEY(RE)                                            
         CR    R9,RF                                                            
         BNE   *+10                                                             
         CLCL  R8,RE                                                            
         BE    PUTB8                                                            
         OI    3(R4),X'80'         UNEQUAL-MARK STATION READY FOR SEND          
*                                                                               
PUTB8    CLI   BDSEC,0                                                          
         BNE   PUTB9                                                            
*                                                                               
         L     R5,ATWA             R5 GOT MESSED UP ABOVE                       
         LA    R2,BWSMSGH                                                       
         XC    8(L'BWSMSG,R2),8(R2)                                             
         MVC   8(L'MSG0SPTL,R2),MSG0SPTL                                        
         OI    6(R2),X'80'                                                      
         LA    R2,BWSRECH                                                       
         DC    H'0'                                                             
         DC    C'$ABEND'                                                        
MSG0SPTL DC    C'CANNOT TRANSFER!  THERE IS A 0 SPOT LENGTH'                    
*                                                                               
PUTB9    DS    0H                                                               
*                                                                               
PUTBYES  SR    RC,RC                                                            
PUTBNO   LTR   RC,RC                                                            
PUTBX    XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* NOTE: THIS CODE IS DUPLICATED ABOVE IN ROUTINE 'ADDSTA'                       
*                                                                               
* ADD STATION TO LIST OF STATIONS ADDED TO OR CHANGED IN BUY FILE               
* OUTPUT : R1=A(STATION LIST ENTRY)                                             
***********************************************************************         
         SPACE 1                                                                
         USING BUYRECD,R2                                                       
ASTA     DS    0H                                                               
         LA    R1,LSTALIST                                                      
         LA    RF,MAXSTA                                                        
*                                                                               
ASTA2    OC    0(3,R1),0(R1)                                                    
         BNZ   *+14                                                             
         MVC   0(3,R1),BUYMSTA+2                                                
         B     ASTAX                                                            
         CLC   BUYMSTA+2(3),0(R1)                                               
         BE    ASTAX                                                            
         LA    R1,4(R1)                                                         
         BCT   RF,ASTA2                                                         
*                                                                               
         L     R5,ATWA                                                          
         LA    R2,BWSMSGH                                                       
         XC    BWSMSG,BWSMSG                                                    
         MVC   BWSMSG(L'TOOMNYST),TOOMNYST                                      
         OI    BWSMSGH+6,X'80'                                                  
         LA    R2,BWSRECH                                                       
         DC    H'0'                                                             
         DC    C'$ABEND'                                                        
*                                                                               
TOOMNYST DC    C'TOO MANY STATIONS!  TRY BY STATION.'                           
*                                                                               
ASTAX    BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* NOTE: THIS CODE IS DUPLICATED ABOVE IN ROUTINE 'ACTIV'                        
*                                                                               
* ADD OR UPDATE BUY ACTIVITY ELEMENT                                            
* INPUT  : R2 = A(BUY RECORD)                                                   
***********************************************************************         
         SPACE 1                                                                
ACTV     NTR1  ,                                                                
         USING BUYRECD,R2                                                       
         OC    CUPASS,CUPASS       TEST PASSWORD PROTECT ACTIVE                 
         BZ    ACTVX                                                            
         TM    CUSTAT,CUSPER       AND IT'S A PERSONAL PASSWORD                 
         BZ    ACTVX                                                            
         SR    R0,R0               YES - LOOK FOR ACTIVITY ELEMENT              
         LA    R4,BDELEM                                                        
         USING ACTVELEM,R4                                                      
*                                                                               
ACTV2    IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),X'99'                                                      
         BE    ACTV4                                                            
         CLI   0(R4),0                                                          
         BNE   ACTV2                                                            
         LA    R4,APELEM           NOT FOUND - ADD ACTIVITY ELEM                
         MVI   0(R4),X'99'                                                      
         MVI   1(R4),12                                                         
         XC    ACTVADD(10),ACTVADD                                              
         MVC   ACTVADD,CUPASS                                                   
         MVC   ACTVADD+2(3),ASBDAT                                              
         GOTO1 AADDELS,BUYREC                                                   
         B     ACTVX                                                            
*                                                                               
ACTV4    MVC   ACTVCHG,CUPASS      FOUND - UPDATE ACTIVITY ELEMENT              
         MVC   ACTVCHG+2(3),ASBDAT                                              
*                                                                               
ACTVX    XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* USE SPBUYVAL TO CHECK FOR ERRORS IN THE BUY RECORD                            
***********************************************************************         
VALBUY   NTR1  BASE=*,LABEL=*                                                   
***      XC    APWORK,APWORK                                                    
**   WE'RE GONNA CHEAT A LITTLE, USING APWORK+60 INSTEAD OF APWORK              
         LA    R1,APWORK+60                                                     
         USING SPBUYVLD,R1                                                      
         MVC   SPBYAREC,APFULL     A(BUY RECORD)                                
         MVC   SPBYAFAC,ACOM       A(COMFACS)                                   
         GOTOR VSPBYVAL,APPARM,APWORK+60,(C'Y',BWSMSGH),VMSUNPK                 
         LA    R1,APWORK+60                                                     
         CLI   SPBYERR,0           ANY ERRORS?                                  
         BE    VALBUYX              - NOPE, WE'RE DONE, CONDITION EQ            
*                                                                               
***  WE'RE GONNA DELETE THE BUY REVISION RECORD HERE                            
         CLI   SPBYERR,X'01'                                                    
         BNH   VALDIE               - NEED TO DIE                               
         XC    IOKEY,IOKEY                                                      
         L     R2,AIOAREA2                                                      
         MVC   IOKEY(13),0(R2)     BUY REVISION KEY                             
         LA    R1,DIRHI+IO2                                                     
         USING NBRKEY,R2                                                        
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   IOKEY(13),IOKEYSAV                                               
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
***      GOTO1 AIO,FILGET2                                                      
***      BE    *+6                                                              
***      DC    H'0'                                                             
*                                                                               
***      OI    NBRCNTL,X'80'       DELETE THE RECORD                            
***      GOTO1 AIO,FILPUT2                                                      
***      BE    *+6                                                              
***      DC    H'0'                                                             
*                                                                               
***      LA    R2,IOKEY            DELETE THE KEY                               
         OI    NBRKCNTL,X'80'                                                   
         GOTO1 AIO,DIRWRT+IO2                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
***  WE'RE GONNA DELETE THE BUY REVISION RECORD HERE                            
*                                                                               
         DROP  R1,R2                                                            
*                                                                               
VALBERRX DS    0H                                                               
         LTR   RB,RB               SET CONDITION TO NE (!=)                     
         B     VALBUYXX                                                         
*                                                                               
VALBUYX  DS    0H                                                               
         CR    RB,RB               SET CONDITION TO EQ (=)                      
VALBUYXX J     EXIT                                                             
*                                                                               
VALDIE   DS    0H                                                               
         DC    H'0'                                                             
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CHECKS TO SEE IF THE CAMPAIGN HAS A IDR BUYER                                 
***********************************************************************         
CKCAMIDR NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         USING CAMRECD,R2                                                       
         MVI   CAMKTYP,CAMKTYPQ                                                 
         MVI   CAMKSUB,CAMKSUBQ                                                 
         MVC   CAMKAGMD,BAGYMD                                                  
         OC    CAMKAGMD,BBYRMASK                                                
         MVC   CAMKBYR,BBYR                                                     
         MVC   CAMKCAM,BCAM                                                     
         GOTO1 AIO,DIRHI                                                        
         CLC   CAMKEY(CAMKREST-CAMKEY),IOKEYSAV                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   IODAOVER,CAMKEY+14    SHOULDN'T HAVE TO DO THIS, BUT..           
         GOTO1 AIO,FILGET3                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIOAREA3                                                      
         LA    R2,CAMFSTEL                                                      
         CLI   0(R2),CAMELCDQ      DESCRIPTION ELEMENT?                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING CAMEL,R2                                                         
         CLI   CAMELLN,CAMELLNQ    DO WE HAVE AN EXPANDED DESC ELEM?            
         BNH   CIDRX                                                            
         MVC   LIDRBYR,CAMIDRNM    YES, SAVE IDR BUYER                          
*                                                                               
CIDRX    XIT1                                                                   
         DROP  R2                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO READ PW STATUS RECORD                                              
***********************************************************************         
GETPW    NTR1  BASE=*                                                           
         B     *+12                                                             
         DC    CL8'**GETPW*'                                                    
*                                                                               
         XC    IOKEY,IOKEY                                                      
         LA    R6,IOKEY                                                         
         USING PWFKEY,R6                                                        
         MVC   PWKTYP,=X'0D7A'                                                  
         MVC   PWKAGMD,BAGYMD                                                   
         MVC   PWKCLT,CMPCLTC                                                   
         MVC   PWKPRD,CMPPRDN                                                   
         MVC   PWKEST,CMPESTN                                                   
         MVC   PWKMKT,BMKT                                                      
         DROP  R6                                                               
*                                                                               
         GOTO1 AIO,DIRHI                                                        
         CLC   IOKEY(13),IOKEYSAV    TEST FOUND                                 
         BNE   GETPWYES              IF NOT, CAN'T BE LOCKED                    
         MVC   IODAOVER,IOKEY+14                                                
         GOTO1 AIO,FILGET1                                                      
*                                                                               
         L     R6,AIOAREA1                                                      
         USING PWRECD,R6                                                        
         TM    PWGNFLG,X'C0'         ANY BUY LOCKED BITS?                       
         BNZ   GETPWNO                                                          
*                                                                               
GETPWYES CR    RB,RB                                                            
         B     GETPWX                                                           
*                                                                               
GETPWNO  LTR   RB,RB                                                            
GETPWX   XIT1                                                                   
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
         DROP  RA                                                               
***********************************************************************         
* CHECKS TO SEE IF THE BUY IS LOCKED BY DARE                                    
*                                                                               
* ON ENTRY:    AIOAREA3            A(BUY RECORD)                                
***********************************************************************         
CHKDARLK NTR1  BASE=*,LABEL=*                                                   
         LA    R6,SAVAREA                                                       
         USING SAVAREA,R6          R6=A(SAVE AREA)                              
*                                                                               
         L     RE,AEXTRAWK         NO DARE ORDER FOR THIS BUY YET               
         NI    BTCHFLG1-EXTRAWKD(RE),X'FF'-BFL1DSNT                             
*                                                                               
         CLI   BPRD,X'FF'          POL PRODUCT?                                 
         BNE   CDLK10              NO                                           
         CLI   INOPRD,0            ANY PRODUCT IN OPTIONS?                      
         BE    CDLKXIT             NONE, DARE WON'T HAVE POL AS PRD1            
*                                                                               
CDLK10   LA    R2,IOKEY            BUILD DARE PASSIVE POINTER BY CLT            
         USING DOKEY,R2                DEFAULT TO CASH FIRST                    
         XC    IOKEY,IOKEY                                                      
         MVI   DCKTYPE,DCKTYPQ                                                  
         MVI   DCKSUBTY,DCKSTYPQ                                                
***                                                                             
         MVC   DCKAGMD,BAGYMD                                                   
         NI    DCKAGMD,X'0F'       ISOLATE THE MEDIA                            
         CLI   DCKAGMD,X'02'       IF WE'RE RADIO, THEN DON'T LOCK              
         BE    CDLKXIT                                                          
***                                                                             
         MVC   DCKAGMD,BAGYMD                                                   
         MVC   DCKCLT,BCLT                                                      
         MVC   DCKPRD,BPRD                                                      
         MVC   DCKEST,BEST                                                      
         MVC   DCKSTA,BSTA                                                      
*                                                                               
         CLI   BPRD,X'FF'          IF PRODUCT OPTIONS                           
         BNE   CDLK15                                                           
         MVC   DCKPRD,INOPRD       THEN USE THOSE PRODUCTS TO CHECK             
         MVC   DCKPRD2,INOPRD+1        AGAINST DARE                             
         B     CDLK20                                                           
*                                                                               
CDLK15   CLI   CMPPRD1,0           ANY PIGGYBACK?                               
         BE    *+10                                                             
         MVC   DCKPRD2,CMPPRD2     YES                                          
*                                                                               
CDLK20   CLI   LFLTSNUM,X'FF'      ANY FLIGHT NUMBER?                           
         BE    CDLK25              NONE, CAMPAIGN DOESN'T INTERSECT ANY         
         MVC   DCKFLTNM,LFLTSNUM                                                
*                                                                               
CDLK25   GOTO1 AIO,DIRHI+IO1                                                    
         B     CDLK25B                                                          
CDLK25A  GOTO1 AIO,DIRSQ+IO1                                                    
*                                                                               
CDLK25B  CLC   DOKEY(DCKFLTNM-DOKEY),IOKEYSAV   SAME UPTO FLIGHT NUM?           
         BNE   CDLKXIT                                                          
         CLI   LFLTSNUM,X'FF'      ANY FLIGHT NUMBER?                           
         BE    CDLK26              NONE, CAMPAIGN DOESN'T INTERSECT ANY         
         CLC   DCKFLTNM,LFLTNNUM   FLIGHT NUMBER BWTN START AND END?            
         BH    CDLKXIT                                                          
*                                                                               
CDLK26   MVC   IODAOVER,DOKEY+14   SHOULDN'T HAVE TO DO THIS, BUT...            
         GOTO1 AIO,FILGET1         READ INTO AIOAREA3                           
*                                                                               
****  NEED SUPPLEMENTARY ID ELEM TO GET TRADE METHOD                            
         L     R2,AIOAREA1                                                      
         LA    R3,DORFRST          A(FIRST ELEMENT)                             
CDLK30   CLI   0(R3),0                                                          
         BE    CDLK35                                                           
         CLI   0(R3),DOSPELQ       TRANSMISSION ELEMENT                         
         BE    CDLK31                                                           
         ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     CDLK30                                                           
*                                                                               
         USING DOSPELD,R3                                                       
CDLK31   L     RF,AIOAREA3                                                      
         USING BUYKEY,RF                                                        
         OC    BDREP,BDREP         ANY REP CODE?                                
         BZ    CDLK35              NONE, CAN'T POSSIBLY BE TRADE                
         DROP  RF                                                               
*                                                                               
         CLI   DOSPLEN,DOSPLNQ     ANY TRADE METHOD?                            
         BNH   CDLK35                                                           
         CLI   DOSPTMTH,0                                                       
         BE    CDLK35              NONE                                         
*                                                                               
         MVC   APBYTE,DOSPTMTH     DON'T CARE ABOUT LOWERCASE YET               
         OI    APBYTE,X'40'                                                     
         CLI   APBYTE,TRDESREP     R - USING SPECIAL REP CODE                   
         BNE   CDLK32              ONLY TRADE METHOD SO FAR                     
**** TRADE - SPECIAL REP CODE                                                   
*        PACK  APDUB,DOSPTDAT(3)   REP IS 3 CHARACTER NUMERIC                   
*        CVB   R1,APDUB                                                         
         L     RF,AIOAREA3                                                      
         USING BUYKEY,RF                                                        
         GOTO1 VRCPACK,APPARM,(C'U',BDREP),APDUB                                
         CLC   BDREP,APDUB         REP IS 3 CHARACTER ALPHANUMERIC!             
*        CLM   R1,3,BDREP                                                       
         BNE   CDLK35                                                           
         DROP  RF                                                               
         TM    DOSPTMTH,X'40'      TRADE ORDER?                                 
         BNZ   CDLK35                                                           
         LA    R2,IOKEY                                                         
         B     CDLK25A             NO, WE NEED THE TRADE ORDERS                 
*                                                                               
CDLK32   DS    0H                                                               
**** NO MORE TRADE TYPES                                                        
*                                                                               
CDLK35   L     R2,AIOAREA1                                                      
         LA    R3,DORFRST          A(FIRST ELEMENT)                             
CDLK36   CLI   0(R3),0                                                          
         BE    CDLK45                                                           
         CLI   0(R3),DOXMTELQ      TRANSMISSION ELEMENT                         
         BE    CDLK40                                                           
         ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     CDLK36                                                           
*                                                                               
         USING DOXMTELD,R3                                                      
CDLK40   L     RE,AEXTRAWK         THIS ORDER WAS SENT ONCE                     
         OI    BTCHFLG1-EXTRAWKD(RE),BFL1DSNT                                   
*                                                                               
         CLC   =X'FFFF',DOXMTDID   FAX REP?                                     
         BE    CDLK45              NOT LCKD, NOTHING BACK FROM FAX              
         CLC   =X'FFFD',DOXMTDID   EMAIL REP?                                   
         BE    CDLK45              NOT LCKD, NOTHING BACK FROM EMAIL            
*                                                                               
         OC    DOXMTSTD,DOXMTSTD   NO RESPONSE FROM REP YET?                    
         BZ    CDLK80              NONE, DARE LOCKS THE BUY                     
*********                                                                       
* CHECK IF ORDER IS IN A RECALL STATUS                                          
*********                                                                       
         CLI   DOXMTSTA,QRECALL                                                 
         BL    CDLK60                                                           
         CLI   DOXMTSTA,QRCLUNKN                                                
         BH    CDLK50                                                           
*********                                                                       
* ORDER IS IN A RECALL STATUS                                                   
*********                                                                       
         CLI   DOXMTSTA,QRCLCONF   RECALL REJECTED, WAS CONFIRMED?              
         BE    CDLK80              YES, LOCKED.  WAIT FOR CONFIRMATION          
         CLI   DOXMTSTA,QRCLREJD   RECALL REJECTED, WAS REJECTED?               
         BE    CDLK80              YES, LOCKED.  WAIT FOR REJECTION             
         CLI   DOXMTSTA,QRCLUNKN   RECALL REJECTED, UNKNOWN STATUS?             
         BE    CDLK80              YES, LOCKED.  WAIT UNTIL STATUS              
         B     CDLK45                                                           
*                                                                               
CDLK45   CLI   LFLTSNUM,X'FF'      THIS DARE ORDER DOESN'T LOCK                 
         BE    CDLKXIT             CHECKING FLIGHTS?  NO                        
         B     CDLK25A                                YES, NEXT RECORD          
*                                                                               
CDLK50   CLI   DOXMTSTA,QRCLTRNS   RECALLED, WAS TRANSMITTED?                   
         BE    CDLK45                                                           
         CLI   DOXMTSTA,QRCLWIP    RECALL, WORK IN PROGRESS?                    
         BE    CDLK45                                                           
*********                                                                       
* ORDER IS NOT IN A RECALL STATUS                                               
*********                                                                       
CDLK60   CLI   DOXMTSTA,QAPP       DARE ORDER IS JUST APPROVED?                 
         BE    CDLK80               - YUP, CAN'T CHANGE!                        
         CLI   DOXMTSTA,QSNTPNDG   DARE ORDER IS SENT PENDING?                  
         BE    CDLK80               - YUP, CAN'T CHANGE!                        
         CLI   DOXMTSTA,QTOBESNT   DARE ORDER TO BE SENT?                       
         BNE   CDLK45              NO, BUY IS NOT LOCKED BY DARE                
*                                                                               
CDLK80   LA    R2,BWSMSGH          TRANSMIT DARE LOCK MESSAGE                   
         XC    BWSMSG,BWSMSG                                                    
         MVC   8(DARLKMLN,R2),DARLKMSG                                          
         L     R4,AIOAREA3                                                      
         USING BUYRECD,R4                                                       
         GOTO1 VMSUNPK,APPARM,BUYKMSTA,APWORK,APWORK+4                          
         DROP  R4                                                               
         MVC   8(5,R2),APWORK+4                                                 
         OI    6(R2),X'80'                                                      
         OI    BWSRECH+6,X'C0'     POSITION CURSOR                              
*                                                                               
         TM    SVNWSOMF,NWSOMPF2   WAITING FOR PF2?                             
         BNZ   CDLK85                                                           
         OI    SVNWSOMF,NWSOMPF2   NO, NOW WE ARE                               
         B     CDLK90                                                           
*                                                                               
CDLK85   L     R1,AINP             A(TIOB)                                      
         USING TIOBD,R1                                                         
         XR    R0,R0                                                            
         ICM   R0,1,TIOBAID        FIGURE OUT THE PFKEY                         
         DROP  R1                                                               
         CHI   R0,12                                                            
         BNH   *+8                                                              
         SHI   R0,12                                                            
         CHI   R0,2                GOT THE OKAY TO RECALL THE ORDER             
         BE    CDLK110             GO FOR IT!                                   
*                                                                               
CDLK90   ICM   RD,15,TWAB4GNL                                                   
         B     CDLKXXX             EXIT ALL THE WAY OUT                         
*                                                                               
CDLK110  NI    SVNWSOMF,X'FF'-NWSOMPF2                                          
         OI    SVNWSOMF,NWSOMCON   PRESS ENTER TO CONTINUE                      
*                                                                               
         LA    R2,BWSMSGH          TRANSMIT DARE LOCK MESSAGE                   
         XC    BWSMSG,BWSMSG                                                    
         MVC   8(DARULMLN,R2),DARULMSG                                          
         MVC   8(5,R2),APWORK+4    FROM MSUNPK ABOVE                            
         OI    6(R2),X'80'                                                      
         OI    BWSRECH+6,X'C0'     POSITION CURSOR                              
*                                                                               
         XC    APELEM,APELEM                                                    
         LA    R1,APELEM                                                        
         USING GLVXFRSY,R1           CALL ORDER MANAGER                         
         MVC   GLVXFRSY,=C'SPO'                                                 
         MVC   GLVXFRPR,=C'NWS'                                                 
         MVC   GLVXTOSY,=C'SPO'                                                 
         MVC   GLVXTOPR,=C'OM '                                                 
         OI    GLVXFLG1,GLV1SEPS                                                
         DROP  R1                                                               
*                                                                               
         L     RF,ACOM                                                          
         ICM   RF,15,CGLOBBER-COMFACSD(RF)                                      
         GOTO1 (RF),APPARM,=C'PUTD',APELEM,24,GLVXCTL   XFER CTRL ELEM          
*                                                                               
         XC    APELEM,APELEM                                                    
         MVC   APELEM(1),BAGYMD                                                 
         MVC   APELEM+1(2),BCLT                                                 
         MVC   APELEM+6(3),BSTA                                                 
         MVC   APELEM+9(1),BEST                                                 
*                                                                               
         CLI   BPRD,X'FF'          IF PRODUCT OPTIONS                           
         BNE   CDLK120                                                          
         MVC   APELEM+10(2),INOPRD                                              
         CLI   INOPRD+1,0                                                       
         BNE   CDLK130                                                          
         MVI   APELEM+10,0                                                      
         MVC   APELEM+11(1),INOPRD                                              
         B     CDLK130                                                          
*                                                                               
CDLK120  CLI   CMPPRD1,0           ANY PIGGYBACK IN CAMPAIGN?                   
         BNE   *+14                YES                                          
         MVC   APELEM+11(1),BPRD                                                
         B     CDLK130                                                          
         MVC   APELEM+10(2),CMPPRD1                                             
*                                                                               
CDLK130  CLI   LFLTSNUM,X'FF'      ANY FLIGHT NUMBER?                           
         BE    *+10                NONE, CAMPAIGN DOESN'T INTERSECT ANY         
         MVC   APELEM+12(1),LFLTSNUM                                            
*                                                                               
         L     RF,ACOM                                                          
         ICM   RF,15,CGLOBBER-COMFACSD(RF)                                      
         GOTO1 (RF),APPARM,=C'PUTD',APELEM,13,GLVBUY1                           
*                                                                               
         XC    APWORK,APWORK                                                    
         MVI   APWORK,C'R'         TO RECALL                                    
         GOTO1 (RF),(R1),=C'PUTD',APWORK,8,GLVSPOPT                             
*                                                                               
         GOTO1 (RF),(R1),=C'PUTD',QMED,1,GLVSPMD                                
         GOTO1 (RF),(R1),=C'PUTD',QCLT,3,GLVSPCLT                               
         XC    APWORK,APWORK                                                    
         CLI   BPRD,X'FF'          POL PRODUCT?                                 
         BNE   CDLK134                                                          
         CLI   INOPRD,0            YES, TEST PRODUCT ALLOCATED?                 
         BE    CDLK132                                                          
         MVC   APWORK(3),POLPRD1                                                
         MVC   APWORK+3(3),POLPRD2                                              
         GOTO1 (RF),(R1),=C'PUTD',APWORK,6,GLVDRPRD                             
         B     CDLK140                                                          
*                                                                               
CDLK132  MVC   APWORK(3),=C'***'                                                
         GOTO1 (RF),(R1),=C'PUTD',APWORK,3,GLVDRPRD                             
         B     CDLK140                                                          
*                                                                               
CDLK134  CLI   CMPPRD1,0           ANY PIGGYBACK PRODUCT?                       
         BNE   CDLK136                                                          
         GOTO1 (RF),(R1),=C'PUTD',QPRD,3,GLVSPPRD                               
         B     CDLK140                                                          
*                                                                               
CDLK136  MVC   APWORK(3),POLPRD1                                                
         MVC   APWORK+3(3),POLPRD2                                              
         GOTO1 (RF),(R1),=C'PUTD',APWORK,6,GLVDRPRD                             
*                                                                               
CDLK140  GOTO1 (RF),(R1),=C'PUTD',QEST,3,GLVSPEST                               
*                                                                               
         L     R4,AIOAREA3                                                      
         USING BUYRECD,R4                                                       
         GOTO1 VMSUNPK,APPARM,BUYKMSTA,APWORK,APWORK+4                          
         DROP  R4                                                               
         L     RF,ACOM                                                          
         ICM   RF,15,CGLOBBER-COMFACSD(RF)                                      
         GOTO1 (RF),APPARM,=C'PUTD',APWORK+4,5,GLVSPSTA                         
*                                                                               
         CLI   LFLTSNUM,X'FF'      ANY FLIGHT NUMBER?                           
         BE    CDLK150             NONE, CAMPAIGN DOESN'T INTERSECT ANY         
         GOTO1 (RF),(R1),=C'PUTD',LFLTSNUM,1,GLVSPEFL   FLIGHT                  
*                                                                               
CDLK150  ICM   RD,15,TWAB4GNL                                                   
         B     CDLKXXX             EXIT ALL THE WAY OUT                         
*                                                                               
CDLKXIT  LTR   RE,RE               BUY IS NOT LOCKED                            
CDLKXXX  J     EXIT                                                             
         DROP  R6                                                               
*                                                                               
DARLKMSG DC    C'DKUIT buy(s) locked by OM.  Press PF2 to unlock!!'             
DARLKMLN EQU   *-DARLKMSG          MESSAGE LENGTH                               
DARULMSG DC    C'DKUIT buy(s) unlocked.  Press ENTER to continue.'              
DARULMLN EQU   *-DARULMSG          MESSAGE LENGTH                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CALCULATES BDSEDAY                                                            
*                                                                               
* ON ENTRY:    (R2)                A(BUY RECORD)                                
*                                                                               
* ON EXIT:     BDSEDAY             WILL BE FILLED IN PROPERLY                   
***********************************************************************         
CALCSEDY NTR1  BASE=*,LABEL=*                                                   
         USING BUYRECD,R2                                                       
         XR    R4,R4               GET BITWISE REP OF DAYS BOUGHT               
         ICM   R4,8,BDDAY                                                       
*                                                                               
         CLI   ESTOWSDY,0          OUT OF WEEK ROTATOR?                         
         BE    CSEDY10             NO                                           
         XR    R1,R1               YES, WILL NEED TO APPEND ANOTHER             
         IC    R1,BDDAY              BDDDAY TO FIRST ONE                        
         SLL   R1,1                                                             
         STC   R1,APBYTE                                                        
         ICM   R4,4,APBYTE                                                      
         XR    R1,R1                                                            
         IC    R1,ESTOWSDY                                                      
         SLL   R4,0(R1)                                                         
         SRL   R4,25               CLEAR EVERYTHING BUT HOB                     
         SLL   R4,24                                                            
*                                                                               
CSEDY10  LA    RE,1                R4 SHOULD HAVE CORRECT BDDAY                 
         CLI   ESTOWSDY,0                                                       
         BE    *+8                                                              
         IC    RE,ESTOWSDY                                                      
         LA    R1,7                                                             
CSEDY15  SLA   R4,1                                                             
         BO    CSEDY20             ON OVERFLOW                                  
         AHI   RE,1                                                             
         BCT   R1,CSEDY15                                                       
         DC    H'0'                                                             
*                                                                               
CSEDY20  CHI   RE,7                WE HAVE OUR START DAY                        
         BNH   *+8                                                              
         SHI   RE,7                                                             
         LR    RF,RE                                                            
         SLL   RE,4                                                             
         STC   RE,APBYTE                                                        
         LR    RE,RF                                                            
         AHI   RE,1                NEXT DAY NUMBER                              
         BCTR  R1,0                ONE LESS DAY TO SHIFT                        
*                                                                               
CSEDY22  SLA   R4,1                                                             
         BZ    CSEDY30             NO MORE BITS                                 
         BO    CSEDY26                                                          
CSEDY24  AHI   RE,1                                                             
         BCT   R1,CSEDY22                                                       
         B     CSEDY30                                                          
*                                                                               
CSEDY26  LR    RF,RE               LAST DAY WITH BIT ON                         
         B     CSEDY24                                                          
*                                                                               
CSEDY30  CHI   RF,7                                                             
         BNH   *+8                                                              
         SHI   RF,7                                                             
         STC   RF,BDSEDAY                                                       
         OC    BDSEDAY,APBYTE                                                   
CSEDYXIT XIT1  ,                                                                
         DROP  R2                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* GET THE FLIGHT RECORD                                                         
*                                                                               
* ON EXIT:     MISCFLG1.MF1FLTRC   1 = FLIGHT EXISTS FOR THE EST                
*                                  0 = NO FLIGHT RECORD EXISTS                  
***********************************************************************         
GETFLTRC NTR1  BASE=*,LABEL=*                                                   
         MVI   LFLTSNUM,X'FF'      NO FLIGHT, 0 MEANS THE NON-FLIGHT            
         MVI   LFLTNNUM,X'FF'        DATES ARE IN THE CAMPAIGN                  
*                                                                               
         XC    IOKEY,IOKEY         SEE IF WE HAVE THE FLIGHT REC USING          
         LA    R2,IOKEY               POL ON THE ESTIMATE                       
         USING DFLRECD,R2                                                       
         MVI   DFLKTYP,DFLKTYPQ                                                 
         MVI   DFLKSUB,DFLKSUBQ                                                 
         MVC   DFLKAGMD,BAGYMD                                                  
         MVC   DFLKCLT,BCLT                                                     
         MVC   DFLKPRD,=C'POL'                                                  
         MVC   DFLKEST,BEST                                                     
         GOTO1 AIO,DIRHI+IO1                                                    
*                                                                               
         CLC   IOKEY(L'DFLKEY),IOKEYSAV   FLT REC EXISTS FOR POL EST?           
         BE    GFLTRC10                   YES                                   
*                                                                               
         XC    IOKEY,IOKEY         SEE IF WE HAVE THE FLIGHT REC USING          
         LA    R2,IOKEY               PRIMARY PRODUCT ON THE ESTIMATE           
         MVI   DFLKTYP,DFLKTYPQ                                                 
         MVI   DFLKSUB,DFLKSUBQ                                                 
         MVC   DFLKAGMD,BAGYMD                                                  
         MVC   DFLKCLT,BCLT                                                     
         MVC   DFLKPRD,QPRD                                                     
         MVC   DFLKEST,BEST                                                     
         GOTO1 AIO,DIRHI+IO1                                                    
*                                                                               
         CLC   IOKEY(L'DFLKEY),IOKEYSAV FLIGHT REC EXISTS FOR PRD/EST?          
         BE    GFLTRC10                 YES                                     
*                                                                               
         XC    IOKEY,IOKEY         SEE IF WE HAVE THE FLIGHT REC FOR            
         LA    R2,IOKEY               'ALL' PRODUCTS ON THIS ESTIMATE           
         MVI   DFLKTYP,DFLKTYPQ                                                 
         MVI   DFLKSUB,DFLKSUBQ                                                 
         MVC   DFLKAGMD,BAGYMD                                                  
         MVC   DFLKCLT,BCLT                                                     
         MVC   DFLKPRD,=C'ALL'                                                  
         MVC   DFLKEST,BEST                                                     
         GOTO1 AIO,DIRHI+IO1                                                    
*                                                                               
         CLC   IOKEY(L'DFLKEY),IOKEYSAV FLIGHT REC EXISTS FOR EST?              
         BNE   GFLTRCX                  NO                                      
*                                                                               
GFLTRC10 GOTO1 AIO,FILGET1              PUT THE FLIGHT RECORD IN AIO            
*                                                                               
         L     R2,AIOAREA1                                                      
         USING DFLKEY,R2                                                        
         SR    R0,R0                                                            
         LA    R1,DFLEL                                                         
GFLTRC20 CLI   0(R1),0                                                          
         BE    GFLTRCX                                                          
         CLI   0(R1),DFINFELQ                                                   
         BE    GFLTRC40                                                         
         CLI   0(R1),DFFLTELQ                                                   
         BE    GFLTRC50                                                         
GFLTRC30 IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     GFLTRC20                                                         
*                                                                               
         USING DFINFEL,R1                                                       
GFLTRC40 CLC   CMPSTDT,DFINFSDT                                                 
         BH    GFLTRC30                                                         
         MVI   LFLTSNUM,0          ZERO FLIGHT IS PART OF CAMPAIGN              
         CLC   CMPND,DFINFSDT                                                   
         BH    GFLTRC30                                                         
         MVI   LFLTNNUM,0          CAMPAIGN LIVES ONLY IN ZERO FLIGHT           
         B     GFLTRCX                                                          
*                                                                               
         USING DFFLTEL,R1                                                       
GFLTRC50 CLI   LFLTSNUM,X'FF'      DETERMINED START FLT INTERSECTION?           
         BNE   GFLTRC80            YES                                          
*                                                                               
         CLC   CMPSTDT,DFFLTSTR                                                 
         BL    GFLTRC60                                                         
         CLC   CMPSTDT,DFFLTEND                                                 
         BH    GFLTRC30                                                         
         B     GFLTRC70                                                         
*                                                                               
GFLTRC60 CLC   CMPND,DFFLTSTR                                                   
         BL    GFLTRCX             BETWEEN FLIGHTS, CAN'T BE LOCKED             
GFLTRC70 MVC   LFLTSNUM,DFFLTNUM                                                
*                                                                               
GFLTRC80 CLC   CMPND,DFFLTEND                                                   
         BH    GFLTRC30                                                         
         CLC   CMPND,DFFLTSTR                                                   
         BL    GFLTRC90                                                         
         MVC   LFLTNNUM,DFFLTNUM                                                
         B     GFLTRCX                                                          
*                                                                               
GFLTRC90 XR    RE,RE               BETWEEN FLIGHTS, SO PREVIOUS FLIGHT          
         IC    RE,DFFLTNUM                                                      
         BCTR  RE,R0                                                            
         STC   RE,LFLTNNUM                                                      
         DROP  R1                                                               
*                                                                               
GFLTRCX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* BUILD BUY DESCRIPTION ELEMENT                                       *         
* INPUT  : R1 = LBADD WHEN BUILDING NEW BUY RECORD                    *         
*               LBCHA WHEN CHANGING EXISTING BUY RECORD               *         
*          LBUYCOST = BUY COST                                        *         
*          LAWKST   = A(FIRST ENTRY IN WEEKS TABLE)                   *         
*          LAWKEN   = A(LAST ENTRY IN WEEKS TABLE)                    *         
*          LNSPWKS  = N'WEEKS WITH SPOTS                              *         
***********************************************************************         
BUYDESC  NTR1  BASE=*,LABEL=*                                                   
*******  L     RC,APALOCAL                                                      
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         STC   R1,LBADDCHA         ADD/CHANGE INDICATOR                         
         L     R2,AIOAREA3                                                      
         USING BUYRECD,R2                                                       
*******  L     R3,AIOAREA2                                                      
         USING NBRSELD,R3                                                       
         MVI   BDCODE,1            BUILD BUY DESCRIPTION ELEMENT                
         LA    R1,NDELEM-BDELEM                                                 
         STC   R1,BDLEN            ELEMENT LENGTH                               
         CLI   LBADDCHA,LBADD      TEST FOR ADD NEW BUY RECORD                  
         BNE   BUYD2                                                            
         XC    BDSTART,BDSTART     YES - CLEAR START/END DATES                  
         XC    BDEND,BDEND                                                      
*                                                                               
BUYD2    TM    LFLAG,LORBIT        TEST ORBIT                                   
         BO    BUYD4               YES-DAYS SET BY ORBADD ROUTINE               
         MVC   BDDAY,NBRSDAYS      BUILD DAYS                                   
*&&DO                                                                           
         CLI   NBRSDAYS,0          TEST PACKAGE                                 
         BNE   BUYD4                                                            
         MVC   BDDAY,BWDPODAY                                                   
*&&                                                                             
**BUYD4    MVI   BDSEDAY,0                                                      
BUYD4    SR    R0,R0                                                            
         ICM   R0,1,ESTOWSDY       TEST OUT-OF-WEEK ROTATOR                     
         BZ    BUYD14                                                           
         LR    R1,R0               YES-                                         
         TM    LFLAG,LORBIT        TEST ORBIT                                   
         BZ    *+10                                                             
         BCTR  R1,0                YES-SET BDSEDAY TO ALL DAYS OF WEEK          
         B     BUYD12                                                           
         SR    RF,RF               GET START DAY                                
         ICM   RF,8,BDDAY                                                       
         SR    R4,R4                                                            
         SR    R8,R8                                                            
         SR    R9,R9                                                            
         SR    RE,RE                                                            
*                                                                               
BUYD6    SLDL  RE,1                                                             
         LTR   RE,RE                                                            
         BZ    BUYD7                                                            
         LR    R8,R4                                                            
         LTR   R9,R9                                                            
         BNZ   BUYD7                                                            
         LR    R9,R4                                                            
*                                                                               
BUYD7    LA    R4,1(R4)                                                         
         SR    RE,RE                                                            
         BCT   R0,BUYD6                                                         
*                                                                               
         LTR   RF,RF                                                            
         BNZ   BUYD8                                                            
         LR    R0,R9                                                            
         LR    R1,R8                                                            
         B     BUYD12                                                           
*                                                                               
BUYD8    SLDL  RE,1                                                             
         LTR   RE,RE                                                            
         BNZ   *+12                                                             
         LA    R1,1(R1)                                                         
         B     BUYD8                                                            
         LR    R0,R1                                                            
         LTR   R8,R8                                                            
         BZ    BUYD10                                                           
         LR    R1,R8                                                            
         B     BUYD12                                                           
*                                                                               
BUYD10   LTR   RF,RF                                                            
         BZ    BUYD12                                                           
         LA    R1,1(R1)                                                         
         SLDL  RE,1                                                             
         B     BUYD10                                                           
*                                                                               
BUYD12   SLL   R0,4                                                             
         OR    R1,R0                                                            
         STC   R1,BDSEDAY                                                       
*                                                                               
BUYD14   BAS   RE,BUYDATE          BUILD DATE-RELATED FIELDS                    
         BNE   BUYDX                                                            
         TM    LFLAG,LORBIT        TEST ORBIT                                   
         BO    BUYD15              YES-TIMES SET BY ORBADD ROUTINE              
         MVC   BDTIMST,NBRSTIMS    START TIME                                   
         MVC   BDTIMEND,NBRSTIME   END TIME                                     
*                                                                               
BUYD15   CLI   LBADDCHA,LBCHA      TEST CHANGING EXISTING RECORD                
         BNE   BUYD16                                                           
         TM    LCHGIND,LCST        AND THERE IS A COST CHANGE                   
         BZ    BUYD16                                                           
         BAS   RE,COSTCHA          YES                                          
         TM    BDCIND2,X'10'       SET TO USE DOLLARS INSTEAD OF CENTS?         
         BNZ   BUYD17A             YES, THEN DON'T RESET BDCIND2                
         B     BUYD17                                                           
*                                                                               
BUYD16   ICM   RF,15,LASKED                                                     
         BZ    *+12                                                             
         TM    1(RF),LPAY          THIS BUY HAS A PAID SPOT?                    
         BNZ   BUYD22              DON'T MODIFY BDCOST,BDCIND & BDCIND2         
*                                                                               
****     MVC   APHALF(1),BDCIND                                                 
****     MVC   APHALF+1(1),BDCIND2                                              
         MVC   BDCOST,LBUYCOST+1   COST                                         
         TM    LBUYCOST,X'80'      NEGATIVE AMOUNT?                             
         BZ    BUYD16F                                                          
         OI    BDCIND,X'01'        MINUS                                        
         ICM   RF,15,LBUYCOST                                                   
         LPR   RF,RF                                                            
         STCM  RF,15,LBUYCOST                                                   
         MVC   BDCOST,LBUYCOST+1   COST                                         
BUYD16F  CLI   LBUYCOST,0          > $167800.00  ?                              
         BE    BUYD17                                                           
         ICM   RF,15,LBUYCOST      MAKE PENNIES INTO DOLLARS                    
         SR    RE,RE                                                            
         D     RE,=F'100'                                                       
         STCM  RF,7,BDCOST         STORE 3 BYTES IN DOLLARS                     
         MVI   BDCIND2,X'10'       SPECIAL FOR  'US BUY IN DOLLARS'             
         B     BUYD17A                                                          
*                                                                               
BUYD17   MVI   BDCIND2,0                                                        
BUYD17A  OI    BDCIND,X'20'        DEFAULT COST TYPE = GROSS                    
         CLI   ESTRATE,C'*'        TEST ESTIMATE SAYS NO RATE TYPE              
         BE    BUYD20              YES-THEN DON'T USE CLIENT'S                  
         MVC   APBYTE,ESTRATE                                                   
         CLI   ESTRATE,0                                                        
         BNE   *+10                                                             
         MVC   APBYTE,CLTPROF+14                                                
         LA    RE,RATETYPS         TEST FOR SPECIAL RATE TYPES                  
*                                                                               
BUYD18   CLI   0(RE),0                                                          
         BE    BUYD20                                                           
         CLC   APBYTE,0(RE)                                                     
         BE    *+12                                                             
         LA    RE,3(RE)                                                         
         B     BUYD18                                                           
         NI    BDCIND,X'FF'-X'20'  NOT GROSS COST TYPE                          
         OC    BDCIND,1(RE)        OVERRIDE COST TYPE 1                         
*                                                                               
         TM    BDCIND2,BDCNBRDQ    (X'10' - BUY IN DOLLARS)?                    
         MVC   BDCIND2,2(RE)                                                    
         BZ    BUYD20                                                           
         OI    BDCIND2,BDCNBRDQ    YES, NEED TO KEEP THIS                       
         B     BUYD20                                                           
*                                                                               
RATETYPS DC    C'1',X'0400'        S                                            
         DC    C'2',X'8000'        F                                            
         DC    C'3',X'1000'        N                                            
         DC    C'4',X'4000'        Q                                            
         DC    C'5',X'0800'        V                                            
         DC    C'6',X'0200'        X                                            
         DC    C'7',X'0000'        P                                            
         DC    C'8',X'2080'        C                                            
         DC    X'00'                                                            
*                                                                               
BUYD20   CLI   CUDMED,C'C'         TEST CANADIAN AGENCY                         
         BNE   BUYD21                                                           
         OI    BDCIND2,X'20'       YES-SET CANADIAN AGENCY BUY                  
         CLI   QMED,C'R'           TEST MEDIA NOT RADIO                         
         BE    BUYD21                                                           
         MVI   BDCANAD,X'80'       YES-GENERATE COMBINED POINTERS               
BUYD21   DS    0H                                                               
*&&DO                                                                           
BUYD21   CLI   LBADDCHA,LBADD      TEST FOR ADD NEW BUY RECORD                  
         BE    BUYD22                                                           
         MVC   BDCIND,APHALF       RESTORE WHAT WAS THERE BEFORE                
         MVC   BDCIND2,APHALF+1                                                 
*&&                                                                             
BUYD22   MVI   BDINPUT,2           METHOD = 2 = END DATE                        
         MVC   BDSEC,NBRSSLN       SPOT LENGTH                                  
         TM    LIND,LPOL                                                        
         BO    *+16                                                             
         MVC   BDTIME,CMPLEN1      ACTIVE P/B TIME PORTION (NON-POL)            
         MVC   BDCOSTP,CMPLEN1     ACTIVE P/B COST PORTION (NON-POL)            
*                                                                               
         MVC   BDDAYPT,NBRSSBDP    SUB-DAYPART CODE                             
         CLI   BDDAYPT,0           DID WE ACTUALLY HAVE A SUB-DAYPART?          
         BNE   *+10                YES                                          
         MVC   BDDAYPT,NBRSDYPT    NO, USE JUST THE DAYPART CODE                
*                                                                               
         MVI   BDPROGRM,C' '       PROGRAM                                      
         MVC   BDPROGRM+1(L'BDPROGRM-1),BDPROGRM                                
         MVC   BDPROGRM(L'NBRSPROG),NBRSPROG                                    
         MVC   BDPROGT,NBRSADJC    PROGRAM ADJACENCY CODE                       
*                                                                               
*        CLI   LBADDCHA,LBADD      ONLY IF ADDING THE BUY                       
*        BNE   BUYD23                                                           
*        OC    ESTREP,ESTREP       ANY SPECIAL REP IN ESTIMATE?                 
*        BZ    BUYD23              NO, NOTHING TO DO THEN                       
*        OC    BDREP,BDREP         HAVE SPECIAL REP FOR THIS BUY?               
*        BNZ   *+10                YES                                          
*        MVC   BDREP,ESTREP        SPECIAL REP CODE                             
*                                                                               
         MVC   BDREP,NBRSREP       ALWAYS USE THIS NOW                          
*                                                                               
BUYD23   CLI   NBRSADJC,0          TEST FOR ANY ADJACENCY CODE                  
         BH    *+10                                                             
         MVC   BDPROGT,CMPADJ      NO-THEN USE THE CAMPAIGN'S                   
         CLI   CLTPROF+9,C'0'      TEST ADJACENCY CODE REQUIRED BY CLT          
         BE    *+12                                                             
         CLI   BDPROGT,0           YES-MAKE SURE IT'S THERE                     
         BE    BUYD99                                                           
*                                                                               
         LA    R0,L'NBRSPROG-2                                                  
         LA    R1,NBRSPROG+L'NBRSPROG-2                                         
*                                                                               
BUYD24   CLC   0(2,R1),=C'-S'      TEST PROGRAM IS A SPECIAL                    
         BNE   *+12                                                             
         MVI   BDPROGRM+17,0                                                    
         B     BUYD26                                                           
         CLI   1(R1),C' '                                                       
         BH    BUYD26                                                           
         BCTR  R1,0                                                             
         BCT   R0,BUYD24                                                        
*                                                                               
BUYD26   OC    QSTA,QSTA                                                        
         BNZ   BUYD28                                                           
         L     R1,AIOAREA2                                                      
         USING NBRKEY,R1                                                        
         XC    APDUB,APDUB                                                      
         MVC   APDUB+2(L'NBRKSTA),NBRKSTA                                       
         DROP  R1                                                               
         GOTO1 VMSUNPK,APPARM,(X'80',APDUB),APWORK,APWORK+4                     
         CLI   APWORK+4+4,C' '                                                  
         BNE   *+16                                                             
         CLI   QMED,C'T'                                                        
         BNE   *+8                                                              
         MVI   APWORK+4+4,C'T'                                                  
         CLC   LSTASAV,APWORK+4                                                 
         BE    BUYD28                                                           
         MVC   LSTASAV,APWORK+4                                                 
         GOTO1 AGETSTA,APWORK+4    GET STATION DETAILS                          
         BE    BUYD28                                                           
         DC    H'0'                                                             
*                                                                               
BUYD28   CLI   LBADDCHA,LBCHA      TEST BUYLINE CHANGE                          
         BNE   *+16                                                             
         L     R1,LASKED           YES-IF BUY HAS ANY PAID OR MATCHED           
         TM    1(R1),LPAY+LAFD         SPOTS, DON'T CHANGE THE TAX RATE         
         BNZ   *+10                                                             
         MVC   BDNTAX,STANTAX      STATION TAX RATE                             
         OI    BDSTAT,X'04'        BWS TRANSFER INDICATOR                       
         TM    CMPOPTS,CAMODLY     DAILY SKED INDICATOR                         
         BZ    BUYD29                                                           
**                                                                              
         TM    NBRSINDS,NBRSDALY   CAMPAIGN WAS DAILY?                          
         BNZ   BUYD28X              - YUP, OK                                   
         OC    NBRSCRDT,NBRSCRDT   ANY CREATION DATE?                           
         BZ    *+6                  - NOPE, NEED TO CHECK CAMPAIGN REC          
         DC    H'0'                 - YUP, DIE, CMPOPTS IS WRONG                
*                                                                               
         MVC   APWORK(L'IOKEY),IOKEY   SAVE OFF CURRENT IOKEY IN CASE           
         USING CAMRECD,R1                                                       
         XC    IOKEY,IOKEY                                                      
         LA    R1,IOKEY                                                         
         MVI   CAMKTYP,CAMKTYPQ      X'0D'                                      
         MVI   CAMKSUB,CAMKSUBQ      X'66'                                      
         MVC   CAMKAGMD,BAGYMD                                                  
         OC    CAMKAGMD,BBYRMASK   ALWAYS APPLY THE BUYER MASK                  
         MVC   CAMKBYR,BBYR                                                     
         MVC   CAMKCAM,BCAM                                                     
         GOTO1 AIO,DIRHI           READ HIGH FOR CAMPAIGN KEY                   
         BE    *+6                                                              
         DC    H'0'                NO REASON WHY IT'S NOT FOUND                 
         CLC   IOKEY(CAMKREST-CAMKEY),IOKEYSAV                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   IODAOVER,IOKEY+14   SHOULDN'T HAVE TO DO THIS, BUT..             
         GOTO1 AIO,FILGET1                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,AIOAREA1                                                      
         TM    CAMOPT,CAMODLY      IS IT REALLY DAILY?                          
         BNZ   BUYD28S              - YUP                                       
         DC    H'0'                SOMETHING WEIRD HERE                         
         DROP  R1                                                               
*                                                                               
BUYD28S  MVC   IOKEY,APWORK        MOVE BACK THE PREVIOUS IOKEY                 
BUYD28X  OI    BDSTAT2,X'80'       OK FOR DAILY                                 
***********************************                                             
*********  DIY TRADE                                                            
BUYD29   TM    CLTIND3,CLTIDIYT    DIY TRADE?                                   
         BZ    *+8                                                              
         OI    BDSTAT2,X'10'       YES                                          
*********  DIY TRADE                                                            
***********************************                                             
         TM    LIND,LPOL           TEST POOL BUY                                
         BZ    *+10                                                             
         MVC   BDMASPRD,LMASPRD    YES-SET MASTER PRODUCT                       
         TM    CMPOPTS2,CAMOF94A   TEST F94=ARB OPTION ON FOR CAMPAIGN          
         BZ    BUYD30                                                           
         TM    STAIND,STAIF94A     AND IT'S ON FOR THE STATION                  
         BZ    BUYD30                                                           
         OI    BDSTAT2,X'04'       YES-SET OPTION ON IN THE BUY                 
*                                                                               
BUYD30   MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     BUYDX                                                            
*                                                                               
BUYD99   MVC   FVMSGNO,=AL2(FVNOADJ)  ADJACENCY CODE MISSING                    
         OI    FVERRIND,FVEUNWND                                                
*                                                                               
BUYDX    CLC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
EXIT2    XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CHANGE THE COST OF EXISTING BUY RECORD                   *         
***********************************************************************         
         SPACE 1                                                                
COSTCHA  NTR1  ,                                                                
         MVI   APBYTE,0                                                         
*                                                                               
COST1    LA    R4,BDELEM                                                        
         SR    R0,R0                                                            
*                                                                               
COST2    CLI   0(R4),0                                                          
         BE    COST10                                                           
         CLI   BUYKPRD,FF          TEST POL BUY                                 
         BE    COST4                                                            
         CLI   0(R4),6             NO-TEST BUY ELEMENT                          
         BL    COST8                                                            
         CLI   0(R4),8                                                          
         BH    COST8                                                            
         USING REGELEM,R4                                                       
         OC    RPAY,RPAY           YES-TEST PAID                                
         BNZ   COSTX               YES-EXIT WITHOUT COST CHANGE                 
         B     COST8                                                            
*                                                                               
COST4    CLI   0(R4),11            TEST POL BUY ELEMENT                         
         BL    COST8                                                            
         CLI   0(R4),13                                                         
         BH    COST8                                                            
         TM    RSTATUS,X'20'       YES-TEST ALREADY HAS COST OVERRIDE           
         BO    COST8                                                            
         CLI   APBYTE,0            NO-TEST LOOKING FOR PAID SPOTS               
         BNE   COST6                                                            
         OC    RPAY,RPAY           YES-TEST PAID                                
         BZ    COST8                                                            
         MVI   APBYTE,1            YES-NOW PUT COST OVERRIDES ON UNPAID         
         B     COST1                   SPOTS                                    
*                                                                               
COST6    OC    RPAY,RPAY           TEST PAID                                    
         BNZ   COST8                                                            
         MVC   RPCOST,LBUYCOST+1   NO-PUT COST OVERRIDE                         
         OI    RSTATUS,X'20'                                                    
*                                                                               
COST8    IC    R0,1(R4)            NEXT ELEMENT                                 
         AR    R4,R0                                                            
         B     COST2                                                            
*                                                                               
COST10   CLI   BUYKPRD,FF          TEST POL BUY                                 
         BNE   *+12                                                             
         CLI   APBYTE,0            YES-TEST FOUND PAID SPOTS                    
         BNE   COSTX                                                            
         MVC   BDCOST,LBUYCOST+1   NO-CHANGE THE COST                           
         TM    LBUYCOST,X'80'      NEGATIVE AMOUNT?                             
         BZ    COST20                                                           
         OI    BDCIND,X'01'        MINUS                                        
         ICM   RF,15,LBUYCOST                                                   
         LPR   RF,RF                                                            
         STCM  RF,15,LBUYCOST                                                   
         MVC   BDCOST,LBUYCOST+1   NO-CHANGE THE COST                           
COST20   CLI   LBUYCOST,0          > $167800.00  ?                              
         BE    COSTX                                                            
         ICM   RF,15,LBUYCOST      MAKE PENNIES INTO DOLLARS                    
         SR    RE,RE                                                            
         D     RE,=F'100'                                                       
         STCM  RF,7,BDCOST         STORE 3 BYTES IN DOLLARS                     
         MVI   BDCIND2,X'10'       SPECIAL FOR  'US BUY IN DOLLARS'             
*                                                                               
COSTX    B     EXIT2                                                            
         EJECT                                                                  
***********************************************************************         
* BUILD DATE RELATED BUY RECORD FIELDS                                *         
* INPUT  : LAWKST   = A(FIRST ENTRY IN WEEKS TABLE)                   *         
*          LAWKEN   = A(LAST ENTRY IN WEEKS TABLE)                    *         
*          LNSPWKS  = N'WEEKS WITH SPOTS                              *         
***********************************************************************         
         SPACE 1                                                                
BUYDATE  NTR1  ,                                                                
         L     R2,AIOAREA3                                                      
         USING BUYRECD,R2                                                       
******   L     R3,AIOAREA2                                                      
         USING NBRSELD,R3                                                       
*                                                                               
         OC    LAWKST,LAWKST       PROTECT AGAINST NO SPOTS                     
         BZ    BDAT36              (COULD HAPPEN ON RE-TRANSFER)                
         L     R8,LAWKST                                                        
         GOTO1 VDATCON,APPARM,(2,2(R8)),(3,APWORK)   BUY START DATE             
*                                                                               
         CLI   LBADDCHA,LBADD      TEST FOR ADD NEW BUY RECORD                  
*&&DO                                                                           
         BE    BDAT0M                                                           
         TM    MISCFLG1,MF1UP2OV   UPTO OVERRIDE COSTS?                         
         BNZ   BDAT0F                                                           
         GOTO1 VDATCON,APPARM,(2,2(R8)),(0,APWORK)                              
         B     BDAT0Z                                                           
*                                                                               
BDAT0F   CLC   BDSTART,APWORK      YES, OLD DATE IS LESS                        
         BNH   BDAT0X                   YES, LEAVE IT ALONE                     
*                                                                               
BDAT0M   MVC   BDSTART,APWORK          YES, LEAVE IT ALONE                      
BDAT0X   GOTO1 (RF),(R1),(3,BDSTART),APWORK          APWORK=START DATE          
*                                                                               
BDAT0Z   TM    CMPOPTS,CAMODLY     TEST DAILY SKED                              
*&&                                                                             
         BE    BDAT0                                                            
         TM    MISCFLG1,MF1UP2OV   UPTO OVERRIDE COSTS?                         
         BZ    BDAT0                                                            
         CLC   BDSTART,APWORK      YES, OLD DATE IS LESS                        
         BNH   BDAT0X                   YES, LEAVE IT ALONE                     
*                                                                               
BDAT0    MVC   BDSTART,APWORK          YES, LEAVE IT ALONE                      
BDAT0X   GOTO1 (RF),(R1),(3,BDSTART),APWORK          APWORK=START DATE          
*                                                                               
         TM    CMPOPTS,CAMODLY     TEST DAILY SKED                              
         BZ    BDAT4                                                            
         GOTO1 VGETDAY,(R1),APWORK,APWORK+6  YES-GET DAY-OF-WEEK                
         CLC   APWORK+6(3),FILLER                OF START DATE                  
         BH    *+6                                                              
         DC    H'0'                                                             
         ZIC   R8,APPARM                                                        
*                                                                               
         CLI   ESTOWSDY,0          YES-TEST OUT-OF-WEEK-ROTATOR                 
         BE    BDAT1                                                            
         ZIC   R1,BDSEDAY          YES-                                         
         SRL   R1,4                R1=START DAY                                 
         CR    R1,R8               COMPARE DAYS                                 
         BE    BDAT4                                                            
         BL    BDAT3               NOT EQUAL-BRING START DATE BACK              
         LA    R8,7(R8)                                                         
         B     BDAT3                                                            
*                                                                               
BDAT1    SR    R1,R1               GET DAY-OF-WEEK OF FIRST DAY OF DAYS         
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RF,8,BDDAY                                                       
         BNZ   BDAT2                                                            
         TM    LFLAG,LORBIT        TEST ORBIT                                   
         BO    *+6                                                              
         DC    H'0'                                                             
         LA    R1,1                YES-FIRST DAY ALWAYS = MONDAY                
         B     BDAT3                                                            
*                                                                               
BDAT2    SLDL  RE,1                                                             
         LTR   RE,RE                                                            
         BNZ   BDAT3                                                            
         LA    R1,1(R1)                                                         
         B     BDAT2                                                            
*                                                                               
BDAT3    SR    R1,R8               TEST IT'S THE SAME DAY                       
         BNM   BDAT4                                                            
         ST    R1,APPARM+8         NO-BRING START DATE BACK                     
         GOTO1 VADDAY,APPARM,APWORK,APWORK+6                                    
         GOTO1 VDATCON,(R1),APWORK+6,(3,BDSTART)                                
         MVC   APWORK(6),APWORK+6                                               
         CLC   APWORK(6),ESTST     TEST BEFORE ESTIMATE START                   
         BL    BDAT99              YES-ABORT                                    
*                                                                               
*                                  FIND BUY END DATE                            
BDAT4    LA    R9,CMPSTMON         R9=A(CAMPAIGN START MONDAY)                  
         TM    CMPOPTS,CAMOWKS     TEST NON-CONTIGUOUS FLIGHTS WEEKS            
         BZ    *+8                                                              
         LA    R9,CMPFLSTM         YES-R9=A(FLIGHT START MONDAY)                
         TM    CMPOPTS,CAMODLY     TEST DAILY SCHEDULE                          
         BZ    BDAT4A                                                           
         LR    R9,R5                                                            
         AHI   R9,CMPDATSD-TWAD    YES-R9=A(FIRST DAY)                          
BDAT4A   L     R8,LAWKEN                                                        
         LA    RE,LWKTAB                                                        
         SR    R8,RE                                                            
         BZ    BDAT4B                                                           
         SRL   R8,3                                                             
         MHI   R8,6                                                             
         LR    RE,R5                                                            
         AHI   RE,CMPDATSD-TWAD                                                 
         LA    R9,0(RE,R8)                                                      
*                                                                               
BDAT4B   TM    CMPOPTS,CAMODLY     TEST DAILY SKED                              
         BZ    BDAT5                                                            
         GOTO1 VGETDAY,APPARM,(R9),APWORK+6  YES-GET DAY-OF-WEEK                
         CLC   APWORK+6(3),FILLER                OF END DATE                    
         BH    *+6                                                              
         DC    H'0'                                                             
         LA    R1,7                GET DAY-OF-WEEK OF LAST DAY OF DAYS          
         SR    RF,RF                                                            
         ZIC   RE,BDDAY                                                         
         SRDL  RE,1                                                             
         LTR   RF,RF                                                            
         BNZ   *+8                                                              
         BCT   R1,*-10                                                          
         ZIC   RE,APPARM                                                        
         SR    R1,RE               TEST IT'S THE SAME DAY                       
         BP    *+14                                                             
         MVC   APWORK+6(6),0(R9)   YES-END DATE STAYS THE SAME                  
         B     BDAT8                                                            
         ST    R1,APPARM+8         NO-BRING END DATE FORWARD                    
         GOTO1 VADDAY,APPARM,(R9),APWORK+6                                      
         B     BDAT8                                                            
*                                                                               
BDAT5    LA    R1,6                                                             
         TM    LFLAG,LORBIT        TEST ORBIT                                   
         BO    BDAT6               YES - LAST DAY = SUNDAY                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RE,1,NBRSDAYS                                                    
*****    BNZ   *+8                 TEST PACKAGE (DAYS=0)                        
*****    IC    RE,BWDPODAY         YES                                          
*****                                                                           
         SR    R0,R0                                                            
         ICM   R0,1,ESTOWSDY                                                    
         BZ    BDAT6                                                            
         AHI   R0,-8                                                            
         LPR   R0,R0                                                            
         SRDL  RE,1                                                             
         BCT   R0,*-4                                                           
         LR    R0,RF                                                            
         SR    RF,RF                                                            
         SRDL  RE,7                                                             
         OR    RF,R0                                                            
         SLDL  RE,7                                                             
         SR    RF,RF                                                            
*                                                                               
BDAT6    SRDL  RE,1                CALCULATE END DATE                           
         LTR   RF,RF                                                            
         BNZ   BDAT7                                                            
         BCT   R1,*-10                                                          
         MVC   APWORK+6(6),0(R9)                                                
         B     BDAT8                                                            
*                                                                               
BDAT7    ST    R1,APPARM+8                                                      
         GOTO1 VADDAY,APPARM,(R9),APWORK+6                                      
*                                                                               
BDAT8    GOTO1 VDATCON,APPARM,APWORK+6,(3,APWORK+12)  END DATE                  
         CLI   LBADDCHA,LBADD      TEST FOR ADD NEW BUY RECORD                  
         BE    BDAT8A                                                           
         TM    MISCFLG1,MF1UP2OV   UPTO OVERRIDE COSTS?                         
         BZ    BDAT8A                                                           
         CLC   BDEND,APWORK+12   NO, OLD DATE IS LESS                           
         BNL   BDAT8B                  NO, WE HAVE A NEW LOWER DATE             
*                                                                               
BDAT8A   MVC   BDEND,APWORK+12         YES, LEAVE IT ALONE                      
*                                                                               
BDAT8B   TM    CMPOPTS,CAMODLY     TEST DAILY SKED                              
         BO    BDAT22                                                           
         CLC   BDEND,CMPND         NO-TEST END AFTER CAMPAIGN END               
         BNH   *+10                                                             
         MVC   BDEND,CMPND         YES-SET END TO CAMPAIGN END                  
*                                                                               
         L     R4,LAWKST                                                        
         MVI   APBYTE,0                                                         
         MVI   APFLAG,0                                                         
*                                                                               
BDAT9    CLI   1(R4),0                                                          
         BE    BDAT12                                                           
         CLI   APBYTE,0            APBYTE = SPOTS/WEEK                          
         BNE   *+14                                                             
         MVC   APBYTE,1(R4)                                                     
         B     BDAT11                                                           
         CLC   APBYTE,1(R4)                                                     
         BNE   BDAT14                                                           
         LR    R0,R4                                                            
         S     R0,APFULL                                                        
         SRL   R0,3                                                             
         CLI   APFLAG,0            APFLAG = FREQUENCY                           
         BNE   BDAT10                                                           
         STC   R0,APFLAG                                                        
         B     BDAT11                                                           
*                                                                               
BDAT10   CLM   R0,1,APFLAG                                                      
         BNE   BDAT14                                                           
*                                                                               
BDAT11   ST    R4,APFULL                                                        
*                                                                               
BDAT12   LA    R4,8(R4)                                                         
         C     R4,LAWKEN                                                        
         BNH   BDAT9                                                            
*                                                                               
         CLI   APFLAG,2                                                         
         BL    BDAT14                                                           
         CLI   APFLAG,4                                                         
         BH    BDAT14                                                           
         ZIC   RF,APFLAG                                                        
         BCTR  RF,0                                                             
         BCTR  RF,0                                                             
         LA    RE,=C'ATF'                                                       
         AR    RE,RF                                                            
         MVC   BDWKIND,0(RE)       WEEK INDICATOR - A/T/F                       
*****    MVC   BDWKS,LNSPWKS       ACTUAL NUMBER OF WEEKS                       
         ZIC   RE,LNSPWKS          ACTUAL NUMBER OF WEEKS                       
         B     BDAT15                                                           
*                                                                               
BDAT14   MVI   BDWKIND,C'O'        NO A/T/F PATTERN -                           
         ICM   RE,15,LAWKEN        SET BDWKS TO OVERALL NUMBER OF WEEKS         
         BZ    BDAT16                                                           
         S     RE,LAWKST                                                        
         SRL   RE,3                                                             
         LA    RE,1(RE)                                                         
BDAT15   CLM   RE,1,BDWKS          IF IT IS LESS THAN WHAT IS IN BUY            
         BL    BDAT16                 THEN DON'T CHANGE                         
         STC   RE,BDWKS                                                         
*                                                                               
BDAT16   L     R4,LAWKST           FIND MOST FREQUENT NUMBER OF                 
         SR    R8,R8               SPOTS PER WEEK                               
         SR    R9,R9                                                            
         XC    APELEM,APELEM                                                    
*                                                                               
BDAT18   IC    R8,1(R4)                                                         
         IC    R9,APELEM(R8)                                                    
         LA    R9,1(R9)                                                         
         STC   R9,APELEM(R8)                                                    
         LA    R4,8(R4)                                                         
         C     R4,LAWKEN                                                        
         BNH   BDAT18                                                           
*                                                                               
         LA    R1,APELEM                                                        
         LA    RE,APELEM+1                                                      
         LA    RF,99                                                            
         MVI   APBYTE,0                                                         
*                                                                               
BDAT20   CLC   APBYTE,0(RE)                                                     
         BNL   *+12                                                             
         MVC   APBYTE,0(RE)                                                     
         LR    R1,RE                                                            
         LA    RE,1(RE)                                                         
         BCT   RF,BDAT20                                                        
         LA    RE,APELEM                                                        
         SR    R1,RE                                                            
         TM    MISCFLG1,MF1UP2OV   UPTO OVERRIDE COSTS?                         
         BNZ   BDAT36                                                           
         STC   R1,BDNOWK           NUMBER OF SPOTS PER WEEK                     
         B     BDAT36                                                           
*                                                                               
BDAT22   MVI   BDWKIND,C'O'        DAILY SKED --------------                    
         CLI   BDSEDAY,0           TEST S/E DAYS ALREADY SET                    
         BNE   BDAT24                                                           
         SR    RE,RE               NO--                                         
         SR    RF,RF                                                            
         ICM   RF,1,BDDAY          TEST DAYS SET YET                            
         BNZ   BDAT23                                                           
         TM    LFLAG,LORBIT        NO-IF IT'S AN ORBIT,                         
         BZ    BDAT24                                                           
         LA    RF,X'7F'            ASSUME M-SU                                  
*                                                                               
BDAT23   LR    R4,RF                                                            
         SLL   RF,24                                                            
         SR    R1,R1                                                            
         SLDL  RE,1                                                             
         LTR   RE,RE                                                            
         BNZ   *+12                                                             
         LA    R1,16(R1)                                                        
         B     *-14                                                             
         LR    R0,R1                                                            
         LR    RE,R4                                                            
         SR    RF,RF                                                            
         LA    R1,7                                                             
         SRDL  RE,1                                                             
         LTR   RF,RF                                                            
         BNZ   *+10                                                             
         BCTR  R1,0                                                             
         B     *-12                                                             
         OR    R1,R0                                                            
         STC   R1,BDSEDAY                                                       
*                                  SET N'WEEKS                                  
BDAT24   GOTO1 VPERVERT,APPARM,APWORK,APWORK+6                                  
         LH    RE,12(R1)                                                        
         OC    10(2,R1),10(R1)                                                  
         BZ    *+8                                                              
         LA    RE,1(RE)                                                         
         STC   RE,BDWKS                                                         
*                                                                               
         TM    CMPOPTS,CAMOWKS     DETERMINE N'SPOTS PER WEEK                   
         BZ    *+18                                                             
         MVC   APWORK+12(8),CMPWKDLY   IF FLIGHT WEEKS,                         
         LA    R0,2                    USE THOSE                                
         B     BDAT25                                                           
         L     R4,LAWKST                                                        
         GOTO1 VDATCON,APPARM,(2,2(R4)),APWORK                                  
         L     R4,LAWKEN                                                        
         GOTO1 (RF),(R1),(2,2(R4)),APWORK+6                                     
         XC    APELEM,APELEM                                                    
         MVC   APELEM(4),VGTBROAD                                               
         MVC   APELEM+4(4),VADDAY                                               
         MVC   APELEM+8(4),VGETDAY                                              
         MVC   APELEM+12(4),VDATCON                                             
         CLI   ESTOWSDY,0                                                       
         BE    *+10                                                             
         MVC   APELEM+24(1),ESTOWSDY                                            
*                                                                               
         LA    RF,APWORK           SETUP 1ST PARAMETER IN APPARM                
         ST    RF,0(R1)                                                         
         LA    RF,3                                                             
         CLI   CMPNWKS,14                                                       
         BNH   *+8                                                              
         LA    RF,8                                                             
         STC   RF,0(R1)                                                         
*                                                                               
         GOTO1 VMOBILE,(R1),,(4,APWORK+12),APELEM,APELEM+16                     
*                                                                               
         LA    R0,3                3 WEEKS WILL COVER 14 DAYS                   
         CLI   CMPNWKS,14                                                       
         BNH   *+8                                                              
         LA    R0,8                8 WEEKS WILL COVER 53 DAYS                   
*                                                                               
BDAT25   LA    R4,APWORK+12                                                     
         L     R8,LAWKST                                                        
         LA    R9,APFULL                                                        
         XC    APFULL,APFULL                                                    
         SR    RF,RF                                                            
*                                                                               
BDAT26   CLC   2(2,R8),2(R4)                                                    
         BNH   BDAT28                                                           
         LA    R4,4(R4)                                                         
         STC   RF,0(R9)                                                         
         LA    R9,1(R9)                                                         
         SR    RF,RF                                                            
         BCT   R0,*+6                                                           
         DC    H'0'                                                             
         CLI   0(R4),X'FF'                                                      
         BNE   BDAT26                                                           
         DC    H'0'                                                             
*                                                                               
BDAT28   ZIC   RE,1(R8)                                                         
         AR    RF,RE                                                            
         LA    R8,8(R8)                                                         
         C     R8,LAWKEN                                                        
         BNH   BDAT26                                                           
         STC   RF,0(R9)                                                         
         LA    R4,APFULL                                                        
         SR    R8,R8                                                            
         CLI   0(R4),0                                                          
         BE    BDAT30                                                           
         LR    R8,R4                                                            
         CLC   0(1,R4),1(R4)                                                    
         BE    BDAT34                                                           
         CLC   0(1,R4),2(R4)                                                    
         BE    BDAT34                                                           
*                                                                               
BDAT30   LA    R4,APFULL+1                                                      
         CLI   0(R4),0                                                          
         BE    BDAT32                                                           
         LTR   R8,R8                                                            
         BNZ   *+6                                                              
         LR    R8,R4                                                            
         CLC   0(1,R4),1(R4)                                                    
         BE    BDAT34                                                           
*                                                                               
BDAT32   LTR   R4,R8                                                            
         BNZ   *+8                                                              
         LA    R4,APFULL+2                                                      
         CLI   0(R4),0                                                          
         BNE   BDAT34                                                           
         DC    H'0'                                                             
*                                                                               
BDAT34   TM    MISCFLG1,MF1UP2OV   UPTO OVERRIDE COSTS?                         
         BNZ   BDAT36                                                           
         MVC   BDNOWK,0(R8)                                                     
*                                                                               
BDAT36   MVC   FVMSGNO,=AL2(FVFOK)    NORMAL EXIT                               
         B     BDATX                                                            
*                                                                               
BDAT99   MVC   FVMSGNO,=AL2(FVESTST)  EST START DATE MUST BE BROUGHT            
         XC    FVXTRA,FVXTRA          BACK TO ....                              
         GOTO1 VDATCON,APPARM,APWORK,(5,FVXTRA)                                 
         OI    FVERRIND,FVEUNWND                                                
*                                                                               
BDATX    CLC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT2                                                            
         DROP  R2,R3                                                            
         EJECT                                                                  
         LTORG                                                                  
         SPACE 1                                                                
FILLER   DC    CL80' '                                                          
         EJECT                                                                  
***********************************************************************         
* ADD DEMOGRAPHIC AND UPGRADE ELEMENTS TO BUY RECORD                  *         
***********************************************************************         
DEMADD   NTR1  BASE=*,LABEL=*                                                   
*****    L     RC,APALOCAL                                                      
         L     R2,AIOAREA3                                                      
         USING BUYRECD,R2                                                       
*****    L     R3,AIOAREA2                                                      
         USING NBRSELD,R3                                                       
         LA    R5,LDMUPBLK         INITIALIZE SPDEMUP BLOCK                     
         USING SPDEMUPD,R5                                                      
         XC    SPDEMUPD(SPDEMUP2),SPDEMUPD                                      
         MVI   LUPPUT,0                                                         
         MVI   LUPSHR,0                                                         
         LA    R8,APELEM                                                        
*                                                                               
         XC    APELEM,APELEM       BUILD UPGRADE DESCRIPTION ELEMENT            
         USING UPELEM,R8                                                        
*                                                                               
         ICM   R4,15,LAUPGEL       TEST FOR UPGRADE                             
         BZ    DEMA2                                                            
         USING NBRUPELD,R4         YES -                                        
         OC    NBRUPEXP,NBRUPEXP                                                
         BZ    DEMA2                                                            
         MVC   UPFILE,NBRUPFIL     UPGRADE FILE                                 
         MVC   UPSRC,CLTSRC        CLIENT'S RATING SERVICE                      
         MVC   UPFBK,NBRUPOBK      SHARE BOOK                                   
*** ???                                                                         
         CLI   NBRUPLEN,51                                                      
         BL    *+10                                                             
         MVC   UPFBKLST,NBRUPBKL   OVERRIDE SHARE BOOK LIST                     
         MVC   UPTYPE,NBRUPEXP     UPGRADE EXPRESSION                           
*****                                                                           
**  UPINPUT IS TOO SHORT TO HANDLE ALL OF NBRUPINP                              
*****    MVC   UPINPUT,NBRUPINP    UPGRADE ACTUAL INPUT DATA                    
*                                                                               
******   MVC   LUPPUT,BWDUPUT      PUT AVERAGING                                
******   MVC   LUPSHR,BWDUSHR      SHR AVERAGING                                
****     MVC   LUPPUT,CMPUPUT <===== USE THESE FOR NOW                          
****     MVC   LUPSHR,CMPUSHR                                                   
         MVC   LUPPUT,NBRSUPUT                                                  
         MVC   LUPSHR,NBRSUSHR                                                  
         B     DEMA3                                                            
*                                                                               
DEMA2    OC    CMPUP,CMPUP         NO - TEST DEFAULT CAMPAIGN UPGRADE           
         BZ    DEMA3                                                            
         MVC   UPFILE,CMPUF             YES                                     
         MVC   UPFBK,CMPFB                                                      
**** ???                                                                        
         MVC   UPFBKLST,CMPFBLST                                                
         MVC   UPTYPE,CMPUP                                                     
         MVC   UPINPUT,X40                                                      
         MVC   LUPPUT,CMPUPUT                                                   
         MVC   LUPSHR,CMPUSHR                                                   
*                                                                               
******  WE NEED TO SETUP QBOOKTYP FOR SPECIAL BOOKS!!                           
DEMA3    TM    UPFBK+1,X'F0'       ANY SPECIAL TYPE BOOKS?                      
         BZ    DEMA4                - NOPE                                      
**                                                                              
         CLI   APWORK,0            ONLY NEED THE FIRST BYTE                     
         MVC   APWORK(1),UPFBK+1                                                
         NI    APWORK,X'F0'        ONLY CARE ABOUT 1ST NIBBLE                   
*                                                                               
*****  THIS IS COPIED FROM "VALUPB4A" IN SPNWS00                                
         LA    RE,BKTYPTB1         (CAN'T ADDRESS BKTYPTAB)                     
DEMA3E   CLI   0(RE),0                                                          
         BNE   *+6                 NOT IN TABLE!                                
         DC    H'0'                DIE FOR NOW!!                                
         CLC   APWORK(1),0(RE)                                                  
         BE    *+12                                                             
         LA    RE,L'BKTYPTB1(RE)                                                
         B     DEMA3E                                                           
         CLI   1(RE),X'FE'         DO WE HAVE SPECIAL BOOKTYPE?                 
         BE    DEMA3Z               - YES, QBOOKTYP SHOULD BE SET               
         MVC   QBOOKTYP,1(RE)                                                   
*                                                                               
*&&DO                                                                           
         L     RF,ACOM                                                          
         ICM   RF,15,CDEMTABS-COMFACSD(RF)                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTO1 (RF),APPARM,SPBOOKTB   GET A(BOOK TABLE)                         
         ICM   RF,15,0(R1)         A(TABLE) RETURNED IN P1                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
****     L     R4,4(R1)            L'TABLE ENTRY RETURNED IN P2                 
         USING SPBKTYPD,RF                                                      
*                                                                               
DEMA3K   DS    0H                                                               
         CLI   0(RF),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   SPBKTYPA,NBRORK     DO WE HAVE THE RIGHT BINARY?                 
         BE    DEMA3Z                                                           
****     AR    RF,R4                                                            
         LA    RF,4(R1,RF)                                                      
         B     DEMA3K                                                           
*&&                                                                             
*****                               MHC  06/24/04                               
******        QBOOKTYP IS NOW SET UP CORRECTLY!!                                
*                                                                               
DEMA3Z   NI    UPFBK+1,X'0F'       TURN OFF SPECIAL BOOK TYPE (FOR BUY)         
*                                                                               
DEMA4    ICM   R4,15,LAODTEL                                                    
         BZ    DEMA6                                                            
         USING NBRODELD,R4                                                      
         MVC   UPSTA,NBRODOST          OVERRIDE STATION                         
         MVC   UPDAYTIM(1),NBRODODY    OVERRIDE DAY                             
         MVC   UPDAYTIM+1(4),NBRODOTM  OVERRIDE TIME                            
*                                                                               
DEMA6    OC    APELEM,APELEM       TEST ANY UPGRADE VALUES                      
         BZ    DEMA8                                                            
         CLI   LUPPUT,C'1'         YES-FIX THE PUT/SHR AVERAGING VALUES         
         BNE   *+8                                                              
         MVI   UP2YRP,C'N'                                                      
         CLI   LUPPUT,C'2'                                                      
         BNE   *+8                                                              
         MVI   UP2YRP,C'Y'                                                      
         CLI   LUPSHR,C'1'                                                      
         BNE   *+8                                                              
         MVI   UP2YRS,C'N'                                                      
         CLI   LUPSHR,C'2'                                                      
         BNE   *+8                                                              
         MVI   UP2YRS,C'Y'                                                      
         MVI   UPELEM,X'62'        ADD THE UPGRADE ELEMENT                      
         MVI   UPELEM+1,UPELEMLN                                                
         GOTO1 AADDELS,BUYREC                                                   
         MVC   SPUPFIL,UPFILE            SAVE UPGRADE VALUES IN                 
         MVC   SPUPUDAY,UPDAYTIM         SPDEMUP BLOCK                          
         MVC   SPUPUTIM,UPDAYTIM+1                                              
         MVC   SPUPTYPE(L'UPTYPE),UPTYPE                                        
         MVC   SPUPFBK,UPFBK                                                    
         MVC   SPUPFBKL,UPFBKLST                                                
         MVC   SPUP2YRP,UP2YRP                                                  
         MVC   SPUP2YRR,UP2YRS                                                  
*                                                                               
DEMA8    ICM   R4,15,LADEMEL       TEST FOR DEMO ELEMENT                        
         BZ    DEMA26                                                           
         USING NBRDMELD,R4                                                      
         XC    LDEMS,LDEMS         YES - BUILD LIST OF DEMOS IN POOL            
         LA    R6,LDEMS            EST HDR BUT NOT IN BWS DEMO ELEMENT          
         MVI   0(R6),FF                                                         
         LA    R8,L'DMODEMO                                                     
         LA    RE,LPOLDEMS                                                      
*                                                                               
DEMA10   OC    0(3,RE),0(RE)       TEST END OF POOL DEMO LIST                   
         BZ    DEMA14                                                           
         ZIC   R9,NBRDMLEN         LOOK FOR DEMO IN ELEMENT                     
         AR    R9,R4                                                            
         BCTR  R9,0                                                             
         LA    R1,NBRDMDMO                                                      
         CLC   1(2,R1),1(RE)                                                    
         BE    DEMA12              FOUND                                        
         BXLE  R1,R8,*-10                                                       
         MVC   0(3,R6),0(RE)       NOT FOUND - ADD TO LIST                      
         LA    R6,3(R6)                                                         
         MVI   0(R6),FF                                                         
*                                                                               
DEMA12   LA    RE,3(RE)            NEXT POOL DEMO                               
         B     DEMA10                                                           
*                                                                               
DEMA14   L     RE,ATIA             SET UP REST OF SPDEMUP BLOCK                 
         ST    RE,SPUPAREC         (USE TIA FOR IUN RECORD)                     
         MVC   SPUPAFAC,ACOM                                                    
         MVC   SPUPAGY,CUAALF                                                   
         MVC   SPUPMED,CUDMED                                                   
         MVC   SPUPCLI,QCLT                                                     
         MVC   SPUPMKT,MKTLKUP                                                  
*                                                                               
         L     RE,AIOAREA2                                                      
         USING NBRKEY,RE                                                        
         XC    APWORK,APWORK                                                    
         MVC   APWORK+2(L'NBRKSTA),NBRKSTA                                      
         DROP  RE                                                               
         GOTO1 VMSUNPK,APPARM,(X'80',APWORK),APWORK+5,APWORK+9                  
*****  CABLE/FUSION DATA LOOKUP         MHC  04/01/05                           
         CLI   APWORK+9,C'0'       IS IT A NUMBER?                              
         BL    DEMA14E              - NOPE, WE DON'T HAVE CABLE NETWORK         
         XC    SPUPSTA,SPUPSTA                                                  
         MVC   SPUPSTA(3),APWORK+14   MOVE THE NETWORK IN                       
         MVC   SPUPSYSE,APWORK+9                                                
         B     DEMA14G                                                          
*****  CABLE/FUSION DATE LOOKUP                                                 
DEMA14E  CLI   QMED,C'T'                                                        
         BNE   *+16                                                             
         CLI   APWORK+9+4,C' '                                                  
         BNE   *+8                                                              
         MVI   APWORK+9+4,C'T'                                                  
         MVC   SPUPSTA,APWORK+9                                                 
*                                                                               
DEMA14G  MVC   SPUPDAY,NBRSDAYS                                                 
******   CLI   NBRSDAYS,0          TEST PACKAGE/ORBIT                           
******   BNE   *+10                                                             
******   MVC   SPUPDAY,BWDPODAY                                                 
         MVC   SPUPTIM,NBRSTIMS                                                 
         MVC   SPUPSRC,CLTSRC                                                   
         OC    SPUPFBK,SPUPFBK                                                  
         BNZ   *+16                                                             
*****    MVC   SPUPFBK,BWDBOOK                                                  
         MVC   SPUPFBK,CMPFB   <====  USE THIS FOR NOW                          
         XC    SPUPFBKL,SPUPFBKL                                                
         MVC   SPUPBTYP,STABKTYP                                                
*                                                                               
         CLI   QBOOKTYP,0                                                       
         BE    *+10                                                             
         MVC   SPUPBTYP,QBOOKTYP                                                
         TM    SPUPFBK+1,BTYBITSQ  SPECIAL BOOK?                                
         BZ    DEMA15                                                           
         TM    SPUPFBK+1,BTY2CHAR  2 CHARACTER BOOKTYPE?                        
         BNO   DEMA14R              - NOPE                                      
         CLI   CMPFBTP,0                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   SPUPBTYP,CMPFBTP     - YUP                                       
         B     DEMA14T                                                          
*                                                                               
DEMA14R  GOTO1 AGETBKTY,APPARM,(C'B',SPUPFBK+1),SPUPBTYP                        
DEMA14T  NI    SPUPFBK+1,X'FF'-BTYBITSQ                                         
*                                                                               
DEMA15   CLI   CUDMED,C'C'         TEST CANADA                                  
         BNE   *+16                                                             
         TM    CLTIND2,CLTIANFR    AND 1W ANGLO/FRANCO OPTION ON                
         BZ    *+8                                                              
         OI    SPUPOPTS,SPOANGFR   YES                                          
*                                                                               
         CLI   G1WPROF+5,C'I'      AS PER ZEN                                   
         BNE   *+8                                                              
         OI    SPUPOPTS,SPOPDMAI                                                
*                                                                               
         CLI   G1WPROF+7,C'Y'      AS PER ZEN                                   
         BNE   *+8                                                              
         OI    SPUPOPTS,SPOPNORM                                                
*                                                                               
         CLI   LDEMS,FF            TEST ANY EXTRA POOL DEMOS                    
         BE    DEMA18                                                           
         XC    LDEMVALS,LDEMVALS                                                
         TM    LFLAG,LORBIT        YES - TEST ORBIT                             
         BO    DEMA18                    YES - EXTRA DEMOS = ZERO               
         OC    SPUPTYPE,SPUPTYPE   IS THERE AN UPGRADE FORMULA                  
         BNZ   DEMA16                                                           
         CLI   QMED,C'R'                                                        
         BE    DEMA18                                                           
         L     RE,ATWA             NO - ERROR EXIT                              
         USING TWAD,RE                                                          
         MVC   FVMSGNO,=AL2(FVNOCNUP)                                           
         LA    R1,BWSKEYH                                                       
         ST    R1,FVADDR                                                        
         B     DEMAX                                                            
         DROP  RE                                                               
*                                  DO UPGRADE FOR EXTRA POOL DEMOS              
***  2 DECIMAL  ***                                                             
DEMA16   TM    APROFBTS,A00TWODC   ARE WE DOING 2 DECIMALS?                     
         BNO   DEMA16E                                                          
         OI    SPUPOPTS,SPOP2DEC    - YUP, SPECIAL 2 DECIMAL LOOKUP             
***  2 DECIMAL  ***                                                             
DEMA16E  GOTO1 ASPDEMUP,APPARM,LDMUPBLK,LDEMS,LDEMVALS                          
*                                                                               
DEMA18   XC    APELEM,APELEM       BUILD DEMOGRAPHIC ELEMENT                    
         LA    R8,APELEM                                                        
         USING NDELEM,R8                                                        
         MVI   NDCODE,2                                                         
         MVC   NDBOOK,NBRSBOOK     SHARE BOOK                                   
         NI    NDBOOK+1,X'0F'                                                   
****   DON'T USE THE FOLLOWING, IT IS ONLY A PATCH FOR SPECIAL BOOKTYPE         
****     MVC   NDBOOK,CMPFB   <====  USE THIS FOR NOW                           
         MVC   NDPROG,NBRSPROG     PROGRAMMING                                  
         ZIC   RF,NBRDMLEN                                                      
         AHI   RF,-(NBRDMDMO-NBRDMELD)                                          
         BCTR  RF,0                                                             
         EX    RF,*+8              MOVE DEMOS FROM BWS RECORD                   
         B     *+10                                                             
         MVC   NDEMNO(0),NBRDMDMO                                               
         LA    RF,NDEMNO           POINT TO THE 1ST DEMO                        
         LA    R6,LDEMS                                                         
         LA    R9,LDEMVALS                                                      
*                                                                               
         LA    R1,8                MAX OF 8 DEMOS                               
DEMA18A  OC    0(8,RF),0(RF)       ANYTHING HERE?                               
         BZ    DEMA19              NO                                           
***  CHECK IF THE DEMO VALUE IS MESSED UP                                       
         MVC   APWORK(4),4(RF)                                                  
         NI    APWORK,X'FF'-X'80'-X'40'   TAKE OF OVERRIDE/2 DECIMAL            
*                                                                               
         CLI   1(RF),C'R'          IS IT RATING?                                
         BE    DEMA18C                                                          
         CLI   1(RF),C'E'          IS IT EXTRA RATING?                          
         BNE   DEMA18G              - NOPE, GOTTA BE IMPRESSION, NO CHG         
*                                                                               
***  2 DECIMAL                                                                  
DEMA18C  TM    APROFBTS,A00TWODC   DOING 2 DECIMAL?                             
         BZ    DEMA18K              - NOPE, CHECK FOR 9999                      
DEMA18G  CLC   APWORK(4),=F'99999'   IS IT BIGGER THAN 999.99 RATING?           
         BNH   DEMA18X                                                          
**       DC    H'0'                                                             
         B     DEMA18ER                                                         
*                                                                               
DEMA18K  CLC   APWORK(4),=F'9999'   IS IT BIGGER THAN 999.9 FOR RATING?         
         BNH   DEMA18X                                                          
**       DC    H'0'                 - YUP, NEEDS TO DIE                         
***                                MHC  01/18/05                                
*****                                                                           
DEMA18ER L     RE,ATWA                                                          
         USING TWAD,RE                                                          
         LA    R1,BWSMSGH                                                       
         DROP  RE                                                               
         XC    8(L'BWSMSG,R1),8(R1)                                             
         MVC   8(13,R1),=C'CANNOT XFR!  '                                       
         MVC   21(8,R1),NBRSSTA                                                 
         MVI   29(R1),C'/'                                                      
         LR    R0,R1                                                            
         LR    R8,RF                                                            
         GOTO1 AGETTIM,NBRSTIMS                                                 
         LR    RF,R8                                                            
         LR    R1,R0                                                            
         MVC   30(11,R1),QTIMES                                                 
         MVI   41(R1),C'/'                                                      
         MVC   42(12,R1),NBRSPROG                                               
         MVI   54(R1),C'-'                                                      
         XC    DBLOCK,DBLOCK       GET DEMO NAME                                
         MVC   DBCOMFCS,ACOM                                                    
         MVC   DBFILE,=C'TP '                                                   
         MVC   DBSELMED,CUDMED                                                  
         XR    R8,R8                                                            
         AHI   R8,1                                                             
         ST    R8,APPARM                                                        
         XC    APWORK(7),APWORK    DEMO NAME ONLY 7 BYTES LONG                  
         LR    R8,RF                                                            
         LR    R0,R1               SAVE OFF R1                                  
**  THE BINARY DEMO IS ALREADY AT THE ADDRESS IN RE                             
         GOTO1 VDEMOCON,APPARM,(R8),(2,APWORK),DBLOCK                           
         LR    R1,R0                                                            
         LR    RF,R8                                                            
         MVC   55(7,R1),APWORK                                                  
         CLI   1(RE),C'R'          RATING?                                      
         BE    DEMA18R1             - YUP                                       
         CLI   1(RE),C'E'          E-RATING?                                    
         BE    DEMA18R1             - YUP                                       
         MVC   62(5,R1),=C'>9999'                                               
         B     *+10                                                             
DEMA18R1 MVC   62(6,R1),=C'>999.9'                                              
         MVC   FVMSGNO,=AL2(FVFSET)                                             
         OI    4(R1),X'20'         VALIDATED                                    
         OI    6(R1),X'80'                                                      
         DC    H'0'                                                             
         DC    C'$ABEND'                                                        
*****                                                                           
DEMA18X  LA    RF,8(RF)                                                         
         BCT   R1,DEMA18A                                                       
         B     DEMA22              MAX'ED OUT                                   
*                                                                               
DEMA19   DS    0H                                                               
*&&DO                                                                           
DEMA19   LA    R6,LDEMS                                                         
         LA    R9,LDEMVALS                                                      
*                                                                               
DEMA20   CLI   0(R6),FF            MOVE EXTRA POOL DEMOS, IF ANY                
         BE    DEMA22                                                           
         MVC   0(3,RF),0(R6)                                                    
         MVC   4(4,RF),0(R9)                                                    
         LA    R6,3(R6)                                                         
         LA    R9,4(R9)                                                         
         LA    RF,8(RF)                                                         
         B     DEMA20                                                           
*&&                                                                             
DEMA22   SR    RF,R8                                                            
         STC   RF,NDLEN            ELEMENT LENGTH                               
         AR    RF,R8                                                            
         BCTR  RF,0                                                             
         LA    RE,8                                                             
         LA    R1,NDEMNO                                                        
         USING NDEMNO,R1                                                        
         MVI   NDSVI,100           ALL HUT ADJUSTMENTS = 100%                   
         CLI   CLTBWPRO+8,C'Y'     TEST ALL DEMOS SHOULD BE OVERRIDES           
         BNE   *+8                                                              
         OI    NDEMRAW,X'80'       YES                                          
         BXLE  R1,RE,*-16                                                       
         DROP  R1                                                               
         GOTO1 AADDELS,BUYREC      ADD THE BUY DEMO ELEMENT                     
*                                                                               
         CLI   CUDMED,C'C'         CANADA ALWAYS GETS SPILL                     
         BE    *+12                                                             
         TM    CLTIND,CLTINOSP     TEST US SPILL ALLOWED                        
         BO    DEMA24                                                           
         BAS   RE,SPILLADD         YES-ADD SPILL ELEMENTS                       
         BNE   DEMAX                                                            
*                                                                               
DEMA24   XC    APELEM,APELEM       DELETE EXISTING DEMO LOOKUP                  
         LA    R8,APELEM           OVERRIDE ELEMENT, IF ANY                     
         USING DLUELEM,R8                                                       
         MVI   DLUCODE,X'24'                                                    
         GOTO1 ADELELS,BUYREC                                                   
         CLI   CUDMED,C'C'         IS IT CANADA?                                
         BE    DEMA24G              - YUP, NO NEED TO TEST BOOKTYPE             
         CLI   QBOOKTYP,0                                                       
         BNE   *+12                                                             
         CLI   STABKTYP,0          TEST SPECIAL BOOK TYPE                       
         BE    DEMA26                                                           
*        MVI   DLULEN,6            YES-ADD ELEMENT WITH BOOK TYPE               
DEMA24G  MVI   DLULEN,DLULENQ      YES-ADD ELEMENT WITH BOOK TYPE               
         MVC   DLUBKTYP,STABKTYP                                                
         L     RF,ATWA                                                          
         AHI   RF,SVMALPHA-TWAD                                                 
         USING SVMALPHA,RF                                                      
         MVC   DLUBAMKT,SVMALPHA                                                
         CLI   SVMRTGSV,C'0'       NSI?                                         
         BNE   *+12                 - NOPE                                      
         OI    DLUBFLGS,X'02'                                                   
         MVI   APBYTE,C'0'                                                      
         CLI   SVMRTGSV,C'1'       BBM?                                         
         BNE   *+12                 - NOPE                                      
         OI    DLUBFLGS,X'01'                                                   
         MVI   APBYTE,C'1'                                                      
****  APBYTE USED TO DETERMINE IF IT'S NSI OR BBM FOR STAMASRD                  
         BRAS  RE,STAMASRD                                                      
****  APFULL WILL BE FILLED WITH STATION OVERRIDE CALL LETTERS                  
         CLI   APWORK+77,0         ARE WE SUPPRESSING IMPRESSION?               
         BE    *+8                  - NOPE                                      
         OI    DLUBFLGS,X'80'       - YUP, SUPPRESS IMPRESSIONS                 
         MVC   DLUBSTOV,APFULL                                                  
*                                                                               
         CLI   QBOOKTYP,0                                                       
         BE    *+10                                                             
         MVC   DLUBKTYP,QBOOKTYP                                                
*                                                                               
         ICM   R4,15,LAUPGEL       TEST FOR UPGRADE                             
         BZ    DEMA25T                                                          
         USING NBRUPELD,R4         YES -                                        
         CLI   NBRUPOBK+1,BTY2CHAR  IS IT 2 CHARACTERS?                         
         BNO   DEMA25T                                                          
         CLI   NBRUPLEN,NBRUPLNQ   EXTENDED LENGTH?                             
         BH    *+6                  - YUP                                       
         DC    H'0'                                                             
         CLI   NBRUPOBT,0                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   DLUBKTYP,NBRUPOBT    - YUP                                       
*                                                                               
DEMA25T  GOTO1 AADDELS,BUYRECD                                                  
*                                                                               
DEMA26   MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
DEMAX    CLC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
XIT2     XIT1  ,                                                                
*                                                                               
* SPECIAL TYPES TABLE - NOTE: MASKS ARE BIT COMBINATIONS                        
* NOTE: COPY OF BKTYPTAB WHICH WE CANNOT ACCESS                                 
BKTYPTB1 DS    0XL2                 SEE BOOKTYPD FOR SETTINGS                   
         DC    AL1(BTY2CHAR),X'FE'  - 2 CHARACTER BOOK TYPES                    
         DC    AL1(BTYZR4Q),C'4'    - ZERO 4                                    
         DC    AL1(BTYZR1Q),C'1'    - ZERO 1                                    
         DC    AL1(BTYHPEOQ),C'I'   - HISPANIC PEOPLE METER                     
         DC    AL1(BTYOLYMQ),C'O'   - OLYMPICS                                  
         DC    AL1(BTYPRNTQ),C'A'   - PARENT ONLY DATA                          
         DC    AL1(BTYHISPQ),C'H'   - HISPANIC                                  
         DC    AL1(BTYBLAKQ),C'B'   - BLACK                                     
         DC    AL1(BTYPEOPQ),C'P'   - PEOPLE METER                              
         DC    AL1(BTYTRADQ),C'T'   - TRADE                                     
         DC    AL1(BTYMTROQ),C'M'   - METRO                                     
         DC    AL1(BTYDMAQ),C'D'    - DMA                                       
         DC    AL1(BTYOTHRQ),C'E'   - OTHER                                     
         DC    AL1(0)                                                           
         EJECT                                                                  
***********************************************************************         
* ADD SPILL ELEMENTS                                                  *         
* INPUT : APELEM CONTAINS THE BUY DEMO ELEMENT                        *         
*         R5 = A(SPDEMUP BLOCK)                                       *         
***********************************************************************         
         SPACE 1                                                                
SPILLADD NTR1  ,                                                                
         USING SPDEMUPD,R5                                                      
         LA    R1,LDEMOVR                                                       
         ST    R1,SPUPAOVR         SET A(OVERRIDE ELEMENTS)                     
         MVC   APBYTE,APELEM       DELETE EXISTING SPILL ELEMS                  
         MVI   APELEM,3                                                         
         GOTO1 ADELELS,BUYREC                                                   
         MVC   APELEM(1),APBYTE                                                 
*                                                                               
         MVC   APWORK(L'IOKEY),IOKEY     SAVE IOKEY                             
         LA    R6,IOKEY            BUILD SPILL RECORD KEY                       
         USING SDEFRECD,R6                                                      
         XC    SDEFKEY,SDEFKEY                                                  
         MVC   SDEFKTYP,=X'0D13'                                                
         MVC   SDEFKAGY,CUAALF                                                  
         MVI   SDEFKRSV,C'0'                                                    
         TM    APROFBTS,A00CANAD   CANADIAN?                                    
         BO    SPAD0A               - YUP YUP                                   
         CLI   CLTSRC,C'A'         0=NSI,1=ARB                                  
         BNE   *+8                                                              
         MVI   SDEFKRSV,C'1'                                                    
         B     SPAD0C                                                           
*                                                                               
SPAD0A   CLI   CLTSRCDF,C'A'       0=NSI,1=ARB                                  
         BNE   *+8                                                              
         MVI   SDEFKRSV,C'1'                                                    
         B     SPAD1                                                            
*                                                                               
SPAD0C   CLI   QMED,C'T'           TEST MEDIA=T                                 
         BNE   SPAD1                                                            
         CLI   SDEFKRSV,C'1'       AND RATING SERVICE = ARB                     
         BNE   SPAD1                                                            
         CLC   CMPND,=X'5D0C1A'    AND CAMPAIGN RUNS PAST 1993                  
         BNH   SPAD1                                                            
         CLI   CMPRSVC,0           AND NO RATING SERVICE OVERRIDE               
         BNE   SPAD1                                                            
         MVI   SDEFKRSV,C'0'       YES-FORCE SERVICE TO NSI                     
*                                                                               
SPAD1    DS    0H                                                               
         L     RE,AIOAREA2                                                      
         USING NBRKEY,RE                                                        
         XC    APWORK,APWORK                                                    
         MVC   APWORK+2(L'NBRKSTA),NBRKSTA                                      
         DROP  RE                                                               
         GOTO1 VMSUNPK,APPARM,(X'80',APWORK),APWORK+5,APWORK+9                  
*                                                                               
         MVC   SDEFKSTA(5),APWORK+9                                             
*****                                                                           
         CLI   QMED,C'R'                                                        
         BE    *+8                                                              
         MVI   IOKEY+9,0           MEDIA T/N HAVE 0 IN KEY+9                    
*****                                                                           
         MVC   SDEFKCLT,BCLT       TRY CLIENT SPECIFIC FIRST                    
*                                                                               
SPAD2    GOTO1 AIO,DIRHI+IO1       READ SPILL RECORD POINTER                    
         BNE   *+14                                                             
         CLC   SDEFKEY(13),IOKEYSAV                                             
         BE    SPAD4                                                            
         MVC   IOKEY,IOKEYSAV                                                   
         OC    SDEFKCLT,SDEFKCLT                                                
         BZ    SPAD20                                                           
         XC    SDEFKCLT,SDEFKCLT   TRY NOT CLIENT SPECIFIC                      
         B     SPAD2                                                            
*                                                                               
SPAD4    GOTO1 AIO,FILGET1         GET SPILL RECORD                             
         BNE   SPAD20                                                           
         L     R6,AIOAREA1                                                      
         CLC   SDEFLEN,=H'256'     RECORD LENGTH MUST BE LE 256                 
         BNH   *+6                                                              
         DC    H'0'                                                             
*********                                                                       
* SHRINK DEMO ELEMENT TO GET RID OF IMPRESSIONS FOR THE SPILL ELEMENT           
*********                                                                       
         LA    R8,APELEM                                                        
         USING NDELEM,R8                                                        
         MVI   NDCODE,3            SET SPILL ELEMENT CODE                       
         XC    NDPROG,NDPROG       ERASE PROGRAMMING                            
         ZIC   RF,NDLEN                                                         
         AHI   RF,-(NDEMNO-NDELEM)                                              
         BNP   SPAD8                                                            
         SRL   RF,3                RF = NUMBER OF DEMOS                         
         LA    R4,NDEMNO           R4 = A(FIRST DEMO)                           
*                                                                               
SPAD4D   CLI   QMED,C'R'           RADIO SAVES SPILL FOR ALL DEMOS              
         BE    *+12                                                             
         CLI   1(R4),C'I'          IMPRESSION?                                  
         BE    SPAD4S              YES, HAVE TO SHRINK THE ELEMENT              
         LA    R4,8(R4)            CHECK NEXT DEMO TYPE                         
SPAD4G   BCT   RF,SPAD4D                                                        
*                                                                               
SPAD4M   LR    R1,R4                                                            
         SR    R1,R8                                                            
         STC   R1,NDLEN            NEW L(ELEMENT)                               
         LA    RE,256                                                           
         SR    RE,R1                                                            
         BCTR  RE,0                                                             
         XC    0(0,R4),0(R4)       CLEAR REST OF THE ELEMENT                    
         B     SPAD4X                                                           
*                                                                               
SPAD4S   ZIC   RE,NDLEN                                                         
         LR    R1,R4               R1 = L(ELEMENT) SO FAR TO THIS DEMO          
         SR    R1,R8                                                            
         SR    RE,R1                                                            
         AHI   RE,-9               LESS THIS DEMO + 1 FOR EX INSTR              
         BNP   SPAD4M                                                           
         EX    RE,*+8                                                           
         B     SPAD4G                                                           
         MVC   0(0,R4),8(R4)       TIME TO SHRINK THE ELEMENT                   
*                                                                               
SPAD4X   DS    0H                                                               
*********                                                                       
SPAD5    LA    R8,APELEM           ALTER DEMO ELEM TO SPILL DEMO ELEM           
         USING NDELEM,R8                                                        
         MVI   NDCODE,3            SET SPILL ELEMENT CODE                       
         XC    NDPROG,NDPROG       ERASE PROGRAMMING                            
         ZIC   RF,NDLEN                                                         
         AHI   RF,-(NDEMNO-NDELEM)                                              
         BNP   SPAD8                                                            
         LA    R4,NDEMNO                                                        
         SRL   RF,3                                                             
         LA    R9,LDEMS                                                         
         XC    LDEMS,LDEMS                                                      
*                                                                               
SPAD6    XC    4(4,R4),4(R4)       CLEAR OVERRIDE AND DEMO VALUE                
         TM    INOXFI,INOXFINS     TEST NO SPILL OPTION                         
         BZ    *+8                                                              
         OI    4(R4),X'80'         YES - OVERRIDE DEMO VALUE = 0                
         MVC   0(3,R9),0(R4)       SAVE THE DEMO CODES                          
         LA    R4,8(R4)                                                         
         LA    R9,3(R9)                                                         
         BCT   RF,SPAD6                                                         
         MVI   0(R9),FF            MARK END OF SAVED DEMO LIST                  
*                                                                               
SPAD8    SR    R0,R0               LOOK FOR SPILL MARKET ELEMENTS               
         LA    R4,SDEFEL                                                        
         USING SDEFEL05,R4                                                      
*                                                                               
SPAD10   CLI   0(R4),0             TEST END OF SPILL RECORD                     
         BE    SPAD18                                                           
         CLI   0(R4),5             TEST SPILL MARKET ELEMENT                    
         BNE   SPAD16                                                           
         CLC   SDEFAMKT,BMKT       TEST SPILL MKT EQUAL ACTUAL MKT              
         BE    SPAD16              YES - SKIP                                   
         TM    SDEFCEX,X'80'       TEST '*' FEATURE                             
         BO    SPAD16              YES - SKIP                                   
**   NO MORE HARD CODE!!         MHC  11/10/05                                  
**       MVC   NDPROG(4),SDEFAMKT  SET AGY AND RTG SVC MARKETS                  
**       MVC   NDPROG+4(1),SDEFBKTY    SET SPECIAL BOOK TYPE                    
**       MVC   NDPROG+6(2),SDEFOSET    SET TIME DSPL                            
         MVC   NDAGYMKT,SDEFAMKT   SET AGY MARKET                               
         MVC   NDRSMKT,SDEFRMKT    SET RATING SERVICE MARKET                    
         MVC   NDBKTYPE,SDEFBKTY   SET SPECIAL BOOKTYPE                         
         MVC   NDRTGSVC,SDEFRSVC   RATING SERVICE (0=NSI,1=BBM)                 
         MVC   NDMKTALF,SDEFALPH   SET ALPHA MARKET CODE                        
         MVC   APBYTE,SDEFRSVC     FOR STAMASRD                                 
****  APBYTE USED TO DETERMINE IF IT'S NSI OR BBM FOR STAMASRD                  
         BRAS  RE,STAMASRD                                                      
****  APFULL WILL BE FILLED WITH STATION OVERRIDE CALL LETTERS                  
         CLI   APWORK+77,0         ARE WE SUPPRESSING IMPRESSION?               
         BE    *+8                  - NOPE                                      
         OI    NDFLGS,X'80'         - YUP, SUPPRESS IMPRESSIONS                 
         MVC   NDSTA,APFULL                                                     
*                                                                               
         TM    INOXFI,INOXFINS     TEST NO SPILL OPTION                         
         BO    SPAD14              YES - SKIP DOING UPGRADES                    
         XC    LDEMOVR,LDEMOVR                                                  
         TM    LIND,LSPILLEL       TEST SPILL DEMO ELEMENT                      
         BZ    *+8                                                              
         BAS   RE,GETOVR           YES-GET THE OVERRIDES                        
         OC    SPUPTYPE,SPUPTYPE   IS THERE AN UPGRADE FORMULA                  
         BNZ   SPAD11                                                           
         L     RE,ATWA             NO - ERROR EXIT                              
         USING TWAD,RE                                                          
         MVC   FVMSGNO,=AL2(FVNOCNUP)                                           
         LA    R1,BWSKEYH                                                       
         ST    R1,FVADDR                                                        
         B     SPADX                                                            
         DROP  RE                                                               
*                                                                               
SPAD11   MVC   SPUPSPL,SDEFRMKT    SET SPILL MKT IN SPDEMUP BLOCK               
         MVC   SPUPBTYP,SDEFBKTY                                                
         XC    LDEMVALS,LDEMVALS                                                
***  2 DECIMAL  ***                                                             
         TM    APROFBTS,A00TWODC   ARE WE DOING 2 DECIMALS?                     
         BNO   *+8                                                              
         OI    SPUPOPTS,SPOP2DEC    - YUP, SPECIAL 2 DECIMAL LOOKUP             
***  2 DECIMAL  ***                                                             
         GOTO1 ASPDEMUP,APPARM,LDMUPBLK,LDEMS,LDEMVALS   DO UPGRADES            
*                                                                               
         CLI   0(R1),X'FF'         NO DEMOS FOUND ERROR?                        
         BNE   SPAD11X             NO                                           
***********************************                                             
* NO DEMOS FOUND BY SPDEMUP, SET DEMO OVERRIDES MYSELF                          
***********************************                                             
         LA    R1,LDEMOVR          R1 = A(DEMO OVERRIDE LIST)                   
SPAD11A  CLI   0(R1),0                                                          
         BE    SPAD11X                                                          
         CLI   0(R1),OVERELEM      TEST OVERRIDE ELEMENT                        
         BE    SPAD11C                                                          
SPAD11B  ZIC   R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     SPAD11A                                                          
*                                                                               
SPAD11C  LA    R9,LDEMS                                                         
         LA    RE,LDEMVALS                                                      
*                                                                               
SPAD11D  CLI   0(R9),X'FF'         END OF DEMO LIST?                            
         BE    SPAD11B             YES, CHECK NEXT OVERRIDE ELEM                
*                                                                               
         CLC   2(1,R9),3(R1)                                                    
         BE    *+16                                                             
         LA    R9,3(R9)                                                         
         LA    RE,4(RE)                                                         
         B     SPAD11D                                                          
*                                                                               
         MVC   2(2,RE),4(R1)       COPY THE OVERRIDE                            
         MVI   0(R9),OVERELEM                                                   
         B     SPAD11B                                                          
*                                                                               
SPAD11X  LA    R1,NDEMNO                                                        
         LA    R9,LDEMS                                                         
         LA    RE,LDEMVALS         MOVE DEMO VALUES TO ELEMENT                  
*                                                                               
SPAD12   CLI   0(R9),FF                                                         
         BE    SPAD14                                                           
***  CHECK IF THE DEMO VALUE IS MESSED UP                                       
         CLI   1(R9),C'R'          IS IT RATING?                                
         BE    SPAD12C                                                          
         CLI   1(R9),C'E'          IS IT EXTRA RATING?                          
         BNE   SPAD12E              - NOPE, GOTTA BE IMPRESSION, NO CHG         
*                                                                               
SPAD12C  MVC   APWORK(4),0(RE)                                                  
         NI    APWORK,X'FF'-X'80'-X'40'   TAKE OF OVERRIDE/2 DECIMAL            
***  2 DECIMAL                                                                  
         TM    APROFBTS,A00TWODC   DOING 2 DECIMAL?                             
         BZ    SPAD12D              - NOPE, CHECK FOR 999                       
         CLC   APWORK(4),=F'9999'   IS IT BIGGER THAN 99.99 FOR RATING?         
         BNH   SPAD12E                                                          
         DC    H'0'                 - YUP, NEEDS TO DIE                         
***  2 DECIMAL                                                                  
*                                                                               
SPAD12D  CLC   APWORK(4),=F'999'   IS IT BIGGER THAN 99.9 FOR RATING?           
         BNH   SPAD12E                                                          
         DC    H'0'                 - YUP, NEEDS TO DIE                         
***                                MHC  01/18/05                                
SPAD12E  MVC   4(4,R1),0(RE)                                                    
         CLI   0(R9),OVERELEM      TEST OVERRIDE                                
         BNE   *+12                                                             
         MVI   0(R9),0                                                          
         OI    4(R1),X'80'                                                      
         LA    R1,8(R1)                                                         
         LA    R9,3(R9)                                                         
         LA    RE,4(RE)                                                         
         B     SPAD12                                                           
*                                                                               
SPAD14   GOTO1 AADDELS,BUYREC      ADD BUY SPILL DEMO ELEMENT                   
*                                                                               
SPAD16   IC    R0,1(R4)            NEXT SPILL RECORD ELEMENT                    
         AR    R4,R0                                                            
         B     SPAD10                                                           
*                                                                               
SPAD18   DS    0H                  NO OP!!                                      
******** MVI   BDSPILL,C'S'        INDICATE SPILL DEMOS PRESENT                 
*                                                                               
SPAD20   MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
SPADX    MVC   IOKEY,APWORK        RESTORE IOKEY                                
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         B     XIT2                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET SPILL OVERRIDES                                                
*                                                                               
* WARNING: THIS ROUTINE DOESN'T DO ANYTHING RIGHT NOW BECAUSE WE DON'T          
*          DO SPILL FOR REVISIONS YET                                           
***********************************************************************         
         SPACE 1                                                                
GETOVR   NTR1  ,                                                                
*&&DO                                                                           
         USING NDELEM,R8                                                        
         LA    RE,BWDEL            SEARCH FOR SPILL DEMO ELEMNETS               
         SR    R0,R0                                                            
*                                                                               
OVR2     CLI   0(RE),0                                                          
         BE    OVRX                                                             
         CLI   0(RE),SPIELCDQ                                                   
         BNE   OVR4                                                             
         USING SPIEL,RE                                                         
         CLC   SPIRMKT,NDPROG+2    FIND ELEMENT FOR THIS MKT                    
         BE    OVR6                                                             
*                                                                               
OVR4     IC    R0,1(RE)                                                         
         AR    RE,R0                                                            
         B     OVR2                                                             
*                                                                               
OVR6     ZIC   RF,1(RE)            FOUND-LOOK FOR DEMO OVERRIDES                
         AR    RF,RE                     AND BUILD DEMO OVERRIDE                
         BCTR  RF,0                      ELEMENTS                               
         LA    R1,SPIDEMO                                                       
         USING SPIDEMO,R1                                                       
         DROP  RE                                                               
         LA    RE,L'SPIDEMO                                                     
         LA    R4,LDEMOVR                                                       
*                                                                               
OVR8     TM    SPIDEMO+4,SPIDEMOV     TEST FOR OVERRIDE                         
         BZ    OVR10                                                            
         MVI   0(R4),OVERELEM         YES-                                      
         MVI   1(R4),6                                                          
         MVC   2(2,R4),SPIDEMO+1                                                
         MVC   4(2,R4),SPIDEMO+6                                                
         LA    R4,6(R4)                                                         
*                                                                               
OVR10    BXLE  R1,RE,OVR8                                                       
*                                                                               
*&&                                                                             
OVRX     B     XIT2                                                             
         EJECT                                                                  
OVERELEM EQU   X'DE'                                                            
NDEMOS   EQU   14                                                               
*                                                                               
X40      DC    CL16' '                                                          
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* TEST DATA LOCKED BY OFFLINE APPLICATION                                       
* THIS CODE SHOULD BE CHANGED TO CALL LOCKUP WHEN ALL CONVENTIONS               
* ARE AGREED. LOCKUP/LOCKET DSECTS ARE IDENTICAL                                
*                                                                               
* ON ENTRY:    (R2)                A(BUY RECORD)                                
***********************************************************************         
         DS    0D                                                               
TSTLOCK  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R5,AEXTRAWK                                                      
         USING EXTRAWKD,R5                                                      
         XC    LKUPKEY,LKUPKEY                                                  
L        USING LKKEYD,LKUPKEY                                                   
*                                                                               
         MVC   L.LOCKAGY,CUAALF                                                 
         MVC   L.LOCKRTY,=C'BU'    LOCK BUYS                                    
         L     RE,IOADDR                                                        
         MVC   L.LOCKMED,QMED                                                   
         MVC   L.LOCKCLT,QCLT                                                   
* QSTA IS NOT RELIABLE AS GETTING IT FROM BUY RECORD                            
******   MVC   L.LOCKSTA,QSTA                                                   
         GOTO1 VMSUNPK,APPARM,(X'80',BUYMSTA-BUYKEY(R2)),APFULL,APDUB           
         MVC   L.LOCKSTA,APDUB     CABLE, WE WANT '/', BUT NOT NTWK             
*                                                                               
         CLI   L.LOCKMED,C'X'                                                   
         BNE   *+8                                                              
         MVI   L.LOCKSTA+4,C'X'                                                 
         CLI   L.LOCKSTA+4,C' '                                                 
         BH    *+8                                                              
         MVI   L.LOCKSTA+4,C'T'                                                 
         CLI   L.LOCKSTA,C'0'                                                   
         BL    *+8                                                              
         MVI   L.LOCKSTA+4,C'/'                                                 
         BAS   RE,TSTIT                                                         
*                                                                               
         XC    LKUPKEY,LKUPKEY                                                  
         MVC   L.LOCKAGY,CUAALF                                                 
         MVC   L.LOCKRTY,=C'BA'    LOCK BUYS                                    
         MVC   L.LOCKMED,QMED                                                   
         MVC   L.LOCKCLT,QCLT                                                   
         SR    R0,R0                                                            
         IC    R0,BEST                                                          
         CVD   R0,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         MVC   L.LOCKEST,APDUB                                                  
         MVC   APDUB(3),L.LOCKEST                                               
         BAS   RE,TSTIT                                                         
*                                                                               
         MVC   L.LOCKEST,=3C' '                                                 
         BAS   RE,TSTIT                                                         
*                                                                               
*****    CLI   APROF7,C'C'                                                      
*****    BNE   TSTLKEQ                                                          
         TM    APROFBTS,A00CANAD   FOR CAN, MAKE SURE MED C NOT LOCKED          
         BZ    TSTLKEQ                                                          
*                                                                               
         CLI   QMED,C'T'                                                        
         BE    *+12                                                             
         CLI   QMED,C'N'                                                        
         BNE   TSTLKEQ                                                          
         MVI   L.LOCKMED,C'C'                                                   
         MVC   L.LOCKEST,APDUB                                                  
         BAS   RE,TSTIT                                                         
*                                                                               
         MVC   L.LOCKEST,=3C' '                                                 
         BAS   RE,TSTIT                                                         
         B     TSTLKEQ                                                          
         DROP  L                                                                
*                                                                               
TSTIT    LR    R0,RE                                                            
TSTIT2   L     RF,ACPARMA                                                       
         L     RF,16(RF)                                                        
         ST    RF,APPARM+4                                                      
         L     RF,(CLOCKET-COMFACSD)(RF)                                        
         GOTO1 (RF),APPARM,('LKTESTQ',LKUPKEY),,0                               
         CLI   4(R1),1             TEST LOCKED                                  
         BE    TSTLKNEQ                                                         
         CLI   4(R1),2             TEST TABLE BUSY                              
         BE    TSTIT2                                                           
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
TSTLKEQ  CR    RB,RB                                                            
         B     *+6                                                              
TSTLKNEQ LTR   RB,RB                                                            
         XIT1                                                                   
         DROP  R5                                                               
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE FALOCKUPD                                                      
LKKEYD   DSECT                                                                  
         ORG   LOCKKEY                                                          
LOCKMED  DS    XL1                                                              
LOCKCLT  DS    XL3                                                              
LOCKSTA  DS    XL5                                                              
         ORG   LOCKSTA                                                          
LOCKEST  DS    CL3                                                              
         ORG                                                                    
* SPNWSWRK                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPNWSWRK                                                       
         PRINT ON                                                               
         EJECT                                                                  
LOCALD   DSECT                                                                  
*                                                                               
COMWRK   DS    0C                  COMMON BETWEEN BWS35 AND BWS37               
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE SPNWS05WRK                                                     
         PRINT ON                                                               
         SPACE 2                                                                
LOCALD   DSECT                                                                  
*                                                                               
******** ORG   LOCALD+2048                                                      
         ORG   LOCAL05X            BWS37 WORK AREA                              
*                                                                               
*  CAUTION : MAKE SURE W/S DOES NOT EXTEND BEYOND 4096 BYTES !!                 
*                                                                               
AXTRA    DS    0F               ** EXTENTION ROUTINE ADDRESSES **               
AVALDT   DS    A                                                                
ABLDWKS  DS    A                                                                
ANXTBYLN DS    A                                                                
ABUYCHG  DS    A                                                                
ASPOTCHG DS    A                                                                
APOOLSPT DS    A                                                                
ANPOLSPT DS    A                                                                
AADDREQ  DS    A                                                                
ACMTADD  DS    A                                                                
AIDADD   DS    A                                                                
APBKADD  DS    A                                                                
AORBADD  DS    A                                                                
AWARNADD DS    A                                                                
AVATADD  DS    A                                                                
APSTADD  DS    A                                                                
ACHKID   DS    A                                                                
ABYRNAME DS    A                                                                
AADDS    DS    A                                                                
AMCLTADD DS    A                                                                
AFIXDLY  DS    A                                                                
AFIXCOST DS    A                                                                
APUTBUY  DS    A                                                                
AADDREC  DS    A                                                                
AXTRAN   EQU   (*-AXTRA)/L'AXTRA                                                
*                                                                               
ABUYDESC DS    A                                                                
ADEMADD  DS    A                                                                
*                                                                               
AEXTRAWK DS    A                                                                
*                                                                               
LASPWEL  DS    A                                                                
LADEMEL  DS    A                                                                
LAUPGEL  DS    A                                                                
LAODTEL  DS    A                                                                
LAIDEL   DS    A                                                                
*LACS2EL  DS    A                                                               
LABTREL  DS    A                                                                
LABTREL1 DS    A                                                                
LABTREL2 DS    A                                                                
LABTREL3 DS    A                                                                
LADTREL  DS    A                                                                
LACURDTR DS    A                                                                
LACMTEL  DS    A                                                                
LAWKST   DS    A                                                                
LAWKEN   DS    A                                                                
LASTABYE DS    A                                                                
LASKED   DS    A                                                                
LARECTAB DS    A                                                                
LANXTREC DS    A                                                                
LARECTBX DS    A                                                                
LPKGMADA DS    F                                                                
LSPTOT   DS    F                                                                
LMAXSPTS DS    F                                                                
LBUYCOST DS    F                                                                
*                                                                               
LTOTBYLN DS    PL2                                                              
LBYLNLO  DS    PL2                                                              
LBYLNHI  DS    PL2                                                              
*                                                                               
********************                                                            
* REFERENCES TO NMAXWKS HAVE BEEN CHANGED TO 53                                 
********************                                                            
LDTINDS  DS    XL53                DATE INDICATORS TABLE                        
LEXCLD   EQU   X'80'               THIS WEEK IS EXCLUDED FROM TRANSFER          
LWKINDS  DS    XL53                WEEK INDICATORS TABLE                        
LFRZ     EQU   X'80'               THIS WEEK IS FROZEN                          
LXFRTOT  DS    XL53                ORIGINAL TRANSFER TOTALS SPTS/WEEK           
LBUYTOT  DS    XL53                CURRENT BUYLINES TOTALS SPTS/WEEK            
LWKTAB   DS    XL((53+1)*8)        WEEKS TABLE (SEE BLDWKS FOR FORMAT)          
LSTALIST DS    XL(MAXSTA*4)        LIST OF STATIONS ADDED/CHANGED IN            
MAXSTA   EQU   50                  BUY FILE                                     
*                                                                               
LNSPTS   DS    X                                                                
LMAXSPW  DS    X                                                                
LNSPWKS  DS    X                                                                
LDAYDSPL DS    X                                                                
*                                                                               
LBADDCHA DS    X                                                                
LBADD    EQU   1                                                                
LBCHA    EQU   2                                                                
*                                                                               
LFLAG    DS    X                                                                
LFREEZE  EQU   X'80'                                                            
LAFFDVT  EQU   X'40'                                                            
LFSTWK   EQU   X'20'                                                            
LNEWEEK  EQU   X'10'                                                            
LPKG     EQU   X'08'                                                            
LPKGREX  EQU   X'04'                                                            
LORBIT   EQU   X'02'                                                            
LWEEKLY  EQU   X'01'                                                            
*                                                                               
LIND     DS    X                                                                
LPOL     EQU   X'80'                                                            
LREXFR   EQU   X'40'                                                            
LFSTREC  EQU   X'20'                                                            
LSEPLINE EQU   X'10'                                                            
LPUTREC  EQU   X'08'                                                            
LSEPDLY  EQU   X'04'                                                            
LSPILLEL EQU   X'02'                                                            
LXFR1STP EQU   X'01'                                                            
*                                                                               
LIND2    DS    X                                                                
LB0PURP  EQU   X'80'               B0 - REQUIRES PURPOSE CODE                   
LOVRDONE EQU   X'40'               COST OVERRIDE DONE, LSEPDLY ONLY!!           
LCOS2    EQU   X'20'               WE NEED BRV RECORD COS2                      
*                                                                               
LCHGIND  DS    X                   RECORD CHANGE INDICATOR                      
LSLN     EQU   X'01'                                                            
LDAYS    EQU   X'02'                                                            
LSTDATE  EQU   X'04'                                                            
LTIMES   EQU   X'08'                                                            
LTIMNOTX EQU   X'10'                                                            
LCST     EQU   X'20'                                                            
LEFFCOST EQU   X'40'               USED FOR COST OVERRIDE FROM BUY!!            
LNEWPRD  EQU   X'80'                                                            
*                                                                               
LPKGMAST DS    XL1                                                              
LPKGSLAV DS    XL256                                                            
*                                                                               
LCMPST   DS    XL2                                                              
LEFDT2   DS    XL2                                                              
LEFDT3   DS    XL2                                                              
LUPPUT   DS    CL1                                                              
LUPSHR   DS    CL1                                                              
LSTASAV  DS    CL8                                                              
LPST     DS    CL10                                                             
LBUYID   DS    CL12                                                             
LRECBYID DS    CL12                                                             
LMGRPID  DS    CL5                 MARKET GROUP ID                              
LIDRBYR  DS    CL6                 IDR BUYER OR PURPOSE CODE                    
LSPWEL   DS    XL71                                                             
LSVSPWEL DS    XL71                                                             
LBTREL   DS    XL71                                                             
LPOLDEMS DS    XL60                                                             
LDEMS    DS    XL60                                                             
LDEMVALS DS    XL80                                                             
LDMUPBLK DS    (SPDEMUP2)X                                                      
LDEMOVR  DS    XL(6*NDEMOS+1)                                                   
LBUYKSAV DS    XL(L'IOKEY)                                                      
LOVRMKT  DS    XL2                                                              
LKEYCOMP DS    XL1                                                              
LBTRIND  DS    XL1                                                              
LSVKEY   DS    XL13                                                             
LDETKEY  DS    XL13                                                             
LPKGMAKY DS    XL13                                                             
LHICOM   DS    XL1                                                              
LMASPRD  DS    XL2                                                              
LESTST   DS    XL2                                                              
LESTND   DS    XL2                                                              
LDTRDATE DS    XL2                                                              
LDTRDAY  DS    XL1                                                              
LSVDAT2  DS    XL3                                                              
LSVDAT3  DS    XL3                                                              
LSVBDAT2 DS    XL2                                                              
LSVBDAT3 DS    XL2                                                              
LSVCOST1 DS    XL4                                                              
LSVCOST2 DS    XL4                                                              
LSVCOST3 DS    XL4                                                              
LSVCS2   DS    XL4                                                              
LSVSTA   DS    CL3                                                              
LSVDAYS  DS    XL1                                                              
*                                                                               
LORBDAY  DS    XL1                                                              
LORBTIM1 DS    XL2                                                              
LORBTIM2 DS    XL2                                                              
*                                                                               
LFLTSNUM DS    XL1                 INTERSECTING STARTING FLIGHT NUMBER          
LFLTNNUM DS    XL1                 INTERSECTING ENDING   FLIGHT NUMBER          
*                                                                               
MISCFLG1 DS    XL1                 MISCELLANEOUS FLAGS                          
MF1UP2OV EQU   X'80'                - UPTO OVERRIDE COSTS NOW                   
MF1OVCST EQU   X'40'                - DOING OVERRIDE COSTS NOW                  
MF1DEL0C EQU   X'20'                - CAN DELETE X'0C' ELEM NOW                 
CURRCOST DS    XL4                 CURRENT COST                                 
*                                                                               
LACNTAB  DS    (LNACNTAB)CL15                                                   
LNACNTAB EQU   12                                                               
         EJECT                                                                  
         SPACE 1                                                                
*                                                                               
*  CAUTION : MAKE SURE W/S DOES NOT EXTEND BEYOND 4096 BYTES !!                 
*                                                                               
         DS    0XL(LOCALD+4096-*)  TELLS US HOW MUCH SPACE LEFT                 
         ORG                                                                    
         ORG   LOCALD+4096                                                      
LOCALX   EQU   *                                                                
         EJECT                                                                  
***********************************************************************         
* EXTRA WORKING STORAGE FOR THIS OVERLAY                              *         
***********************************************************************         
         SPACE 1                                                                
EXTRAWKD DSECT                                                                  
FRSTOFWK DS    A                   A(1ST ELEM FOR THE WEEK)                     
OVRDELEM DS    A                   ADDRESS OF NEXT X'31' (LSEPDLY ONLY)         
*                                                                               
XTRAFLG1 DS    XL1                 EXTRA FLAGS                                  
XF1NXESL EQU   X'80'                - SOME LINES NOT XFR CAUSE OF ESLN          
XAUTH    EQU   X'40'               IF ON, ALREADY WENT THROUGH CODE             
XSDE     EQU   X'20'               IF ON, SDESK AUTH OPEN FOR PRD OPTN          
*                                                                               
BTCHFLG1 DS    XL1                 BATCH FLAGS                                  
BFL1DSNT EQU   X'80'               - WE HAVE A DARE ORDER THAT WAS SENT         
*                                                                               
MAXCOSTS EQU   8                                                                
***************                                                                 
* FOR THE CURRENT WEEK                                                          
***************                                                                 
NBRCSNUM DS    XL1                 NUMBER OF COST OVERRIDES                     
NBRCSPTR DS    XL1                 DISP COUNTER OF CURRENT OVERRIDE             
NBRCSTBL DS    0XL(MAXCOSTS*(4+1))    BYTES 0-3 = OVERRIDE COST                 
         DS    (MAXCOSTS)XL(4+1)      BYTE  4   = # OF SPOTS @ COST             
*                                                                               
BUYCSNUM DS    XL1                 NUMBER OF COST OVERRIDES                     
BUYCSPTR DS    XL1                 DISP COUNTER OF CURRENT OVERRIDE             
BUYCSTBL DS    0XL(MAXCOSTS*(4+1))    BYTES 0-3 = OVERRIDE COST                 
         DS    (MAXCOSTS)XL(4+1)      BYTE  4   = # OF SPOTS @ COST             
***************                                                                 
* FOR THE ENTIRE BUY REVISION RECORD                                            
***************                                                                 
COSTNUMB DS    XL1                 NUMBER OF COST OVERRIDES                     
CURRCOVR DS    XL1                 CURRENT COST OVERRIDE                        
COSTABLE DS    0XL(MAXCOSTS*4)     BYTES 0-3 = OVERRIDE COST                    
         DS    (MAXCOSTS)XL4                                                    
*                                                                               
LSTABYTB DS    XL((MAXSTA+1)*STABYL)   STATION/BUYLINE TABLE                    
LKUPKEY  DS    XL16                LOCKUP KEY                                   
LDTRDAT2 DS    XL2                 CURRENT BUYLINE DATE (SEP DAILY)             
LDTRDAT3 DS    XL2                 BUYLINE DATE LAST ITERATION                  
LSPTDATE DS    XL2                 BDSTART FROM BUY IF LSEPDLY IS ON            
***********************************************************************         
* SCHEDULES TABLE                                                     *         
* TABLE ENTRY HAS THE FOLLOWING FORMAT :                              *         
* +0(1) BUYLINE NUMBER                                                *         
* +1(1) BUYLINE INDICATOR (LCHG = THIS BUYLINE WILL CHANGE)           *         
* +2(1) WEEK 1 CURRENT BUY RECORD NUMBER OF SPOTS                     *         
* +3(1) WEEK 1 PROPOSED NUMBER OF SPOTS ACCORDING TO BWS SCHEDULE     *         
* +4(2) WEEK 2                                                        *         
* +6(2) WEEK 3                                                        *         
* ..... ETC                                                           *         
***********************************************************************         
LSKEDENL EQU   2+2*53              ENTRY LENGTH                                 
LSKEDS   DS    21XL(LSKEDENL)      TABLE                                        
LCHG     EQU   X'80'               BUYLINE CHANGE INDICATOR                     
LADD     EQU   X'40'               ADD BUY ELEMENT(S) TO BUYLINE                
LDEL     EQU   X'20'               DELETE ALL SPOTS FROM BUYLINE                
LAFD     EQU   X'10'               BUY HAS MATCHED SPOTS                        
LPAY     EQU   X'08'               BUY HAS PAID SPOTS                           
LDONTCHG EQU   X'04'               DON'T CHANGE THE BUY                         
LSKEDSL  EQU   *-LSKEDS            LENGTH OF TABLE                              
LSKEDSX  EQU   *                   END OF TABLE                                 
EXTRAWKL EQU   *-EXTRAWKD                                                       
         EJECT                                                                  
STABYD   DSECT                     DSECT FOR STATION/BUYLINE TABLE              
SBSTA    DS    CL8                 STATION                                      
SBLNLO   DS    PL2                 LOW BUY LINE                                 
SBLNHI   DS    PL2                 HIGH BUY LINE                                
STABYL   EQU   *-STABYD                                                         
         SPACE 2                                                                
RTABD    DSECT                     DSECT FOR RECORD TABLE                       
RTSORT   DS    0CL14                                                            
RTSTA    DS    XL3                                                              
RTKBUYS  DS    XL3                                                              
RTSEQ    DS    XL1                 FOR THE NEW BUYS W/O BUYKBUY                 
RTDAYCD  DS    XL1                                                              
RTTIMES  DS    XL4                                                              
RTKDAYS  DS    XL2                                                              
RTKTIMES DS    XL2                                                              
RTABL    EQU   *-RTABD                                                          
         EJECT                                                                  
TWAD     DSECT                                                                  
         ORG   BWSTABH                                                          
* SPNWSFBD                                                                      
* SPNWSHDR                                                                      
* SPNWSCAM                                                                      
* SPNWSBRV                                                                      
* SPGENBUY                                                                      
* SPGENEST                                                                      
* SPDEMUPD                                                                      
* SPGENSDEF                                                                     
* SPGENCLT                                                                      
* SPACVNALD                                                                     
* SPGENMKG                                                                      
* SPGENMKA                                                                      
* SPGENSTEQ                                                                     
* SPGETBUBLD                                                                    
* SPADINTD                                                                      
* SPGENDRORD                                                                    
* SPGENDRFLT                                                                    
* SPGENDRBTC                                                                    
* SPGENWIPW                                                                     
* FATIOB                                                                        
* SPGENAUTH                                                                     
* SPAUTHD                                                                       
         PRINT OFF                                                              
       ++INCLUDE SPNWSFBD                                                       
         EJECT                                                                  
       ++INCLUDE SPNWSHDR                                                       
         EJECT                                                                  
       ++INCLUDE SPNWSCAM                                                       
         EJECT                                                                  
       ++INCLUDE SPNWSBRV                                                       
         EJECT                                                                  
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
         EJECT                                                                  
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
         EJECT                                                                  
       ++INCLUDE SPDEMUPD                                                       
         EJECT                                                                  
       ++INCLUDE SPGENSDEF                                                      
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
SPAVBLKD DSECT                                                                  
       ++INCLUDE SPACNVALD                                                      
         EJECT                                                                  
       ++INCLUDE SPGENMKG                                                       
         EJECT                                                                  
       ++INCLUDE SPGENMKA                                                       
         EJECT                                                                  
       ++INCLUDE SPGENSTEQ                                                      
         EJECT                                                                  
       ++INCLUDE SPGETBUBLD                                                     
         EJECT                                                                  
       ++INCLUDE SPADINTD                                                       
         EJECT                                                                  
       ++INCLUDE SPGENDRORD                                                     
         EJECT                                                                  
       ++INCLUDE SPGENDRFLT                                                     
         EJECT                                                                  
       ++INCLUDE SPGENDRBTC                                                     
         EJECT                                                                  
       ++INCLUDE SPGENWIPW                                                      
         EJECT                                                                  
       ++INCLUDE FATIOB                                                         
         EJECT                                                                  
       ++INCLUDE SPGENAUTH                                                      
         EJECT                                                                  
       ++INCLUDE SPAUTHD                                                        
         EJECT                                                                  
       ++INCLUDE DDGLVXCTLD                                                     
         EJECT                                                                  
       ++INCLUDE DDGLOBEQUS                                                     
         EJECT                                                                  
       ++INCLUDE SPBUYVALD                                                      
         EJECT                                                                  
REQHDRD  DSECT                                                                  
       ++INCLUDE DDREQHDR                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'219SPNWS37   06/19/12'                                      
         END                                                                    
