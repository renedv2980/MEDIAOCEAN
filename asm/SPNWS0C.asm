*          DATA SET SPNWS0C    AT LEVEL 009 AS OF 02/26/07                      
*PHASE T2070CC,*                                                                
T2070C   TITLE 'BWS0C - BUYERS WORK SHEET - BUY ADD PROGRAM'                    
T2070C   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 EXTRAWKL,T2070C**,RA,RR=R8                                       
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
         MVC   APPARM,COMPARM                                                   
         LA    R3,APRECKEY                                                      
         USING BWDRECD,R3                                                       
*                                                                               
         CLI   APMODE,APMVALP                                                   
         BE    VALPAR                                                           
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE SELECT PARAMETERS                                          *         
* OUTPUT : FVMSGNO NE FVOK IF KEY IS INVALID                          *         
*          BWS RECORDS TRANSFERRED TO BUY PROGRAM                     *         
***********************************************************************         
VALPAR   DS    0H                                                               
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
*                                                                               
VALP1A05 GOTO1 AFVAL,BWSOPTH       VALIDATE OPTIONS BECAUSE STUPID PRD          
         XC    INOPTI,INOPTI       CLEAR ALL OPTIONS AND GO AGAIN               
         L     R1,ACOPTTAB         OPTION DOESN'T GO IF KEY CHANGED             
         GOTO1 AVALOPTS                                                         
         BE    VALP1A06                                                         
         LA    R1,BWSOPTH                                                       
         ST    R1,FVADDR                                                        
         B     EXIT                                                             
*                                                                               
VALP1A06 MVI   LIND,0              INITIALIZE                                   
         ZAP   LBYLNLO,=P'0'                                                    
         ZAP   LBYLNHI,=P'0'                                                    
         ZAP   LTOTBYLN,=P'0'                                                   
*                                                                               
         L     RE,AEXTRAWK                                                      
         LA    RF,EXTRAWKL                                                      
         XCEFL ,                                                                
*                                                                               
         L     RE,LARECTAB         CLEAR THE RECORD TABLE                       
         ST    RE,LANXTREC                                                      
         LHI   RF,14*1024          14K NOW, WAS 8K AND 6K B4 THAT               
         LA    R1,0(RE,RF)         WE'VE COME A LONG WAY                        
         ST    R1,LARECTBX                                                      
         XCEFL ,                                                                
*                                                                               
         XC    LSTASAV,LSTASAV                                                  
         XC    LSTALIST,LSTALIST                                                
         GOTO1 VDATCON,APPARM,(3,CMPST),(2,LCMPST)  CAMPAIGN START DATE         
*                                                                               
*****                                                                           
         MVI   SVESTFL1,0                                                       
*****                                                                           
*                                                                               
VALP2A   SR    R8,R8               R8=RECORD COUNT                              
*                                                                               
         LA    R4,BWDKELST-BWDKEY  SET EXECUTE LENGTH FOR KEY COMPARE           
         OC    QSTA,QSTA                                                        
         BZ    *+8                                                              
         LA    R4,L'BWDKELST(R4)                                                
         BCTR  R4,0                                                             
         STC   R4,LKEYCOMP                                                      
         MVC   IOKEY(13),APRECKEY                                               
*                                                                               
VALP3    LA    R1,MINHI2           READ THE DETAIL RECORDS                      
         B     VALP4+4                                                          
*                                                                               
VALP4    LA    R1,MINSEQ2                                                       
         GOTO1 AMIN                                                             
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     VALP6                                                            
         ZIC   R4,LKEYCOMP                                                      
         EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   IOKEY(0),IOKEYSAV                                                
         BNE   VALP6                                                            
         L     R3,AIOAREA2                                                      
         OC    QCABLE,QCABLE       TEST CABLE SYSTEM FILTER                     
         BZ    *+14                                                             
         CLC   QCABLE,BWDSTA       YES-MATCH CABLE SYSTEM                       
         BNE   VALP4                                                            
         CLI   BDPT,0              CHECK DAYPART/LENGTH FILTERS                 
         BE    *+14                                                             
         CLC   BDPT,BWDDPT                                                      
         BNE   VALP4                                                            
         CLI   BSLN,0                                                           
         BE    VALP5                                                            
         CLC   BSLN,BWDSLN                                                      
         BNE   VALP4                                                            
*                                                                               
VALP5    CLI   BWDKELPO,0          IGNORE FUNNY PACKAGE/ORBIT RECORDS           
         BNE   *+12                                                             
         TM    BWDINDS,BWDIPKG+BWDIORB                                          
         BNZ   VALP4                                                            
*                                                                               
         TM    BWDINDS2,BWDINBR    X'40' - ALREADY BEEN > ?                     
         BO    VALP4                - YUP, SKIP THIS RECORD                     
*                                                                               
         LA    RF,BWDEL            LOOK FOR SPOTS PER WEEK ELEMENT              
         SR    R0,R0               OR PREVIOUS TRANSFER ELEMENT                 
*                                                                               
VALP5A   CLI   0(RF),0                                                          
         BE    VALP5B                                                           
         CLI   0(RF),BTRELCDQ      DOES IT HAVE A TRANSFER ELEMENT?             
         BE    VALP4                - YUP, WE DON'T NEED TO BUYADD              
         IC    R0,1(RF)            ...CAN'T DO BUYADD MORE THAN ONCE            
         AR    RF,R0                                                            
         B     VALP5A                                                           
*                                                                               
VALP5B   LA    R8,1(R8)            AUGMENT RECORD COUNT                         
         BAS   RE,SAVEREC          SAVE RECORD DETAILS IN TABLE                 
         LA    R3,IOKEY            NEXT DAYS/TIMES                              
         MVI   BWDKELSQ,FF                                                      
         B     VALP3                                                            
*                                                                               
VALP6    LTR   R8,R8               TEST ANY RECORDS                             
         BZ    VALP97                                                           
         LA    R9,RTABL            SORT THEM                                    
         LA    R0,L'RTSORT                                                      
*                                                                               
         GOTO1 VXSORT,APPARM,(0,LARECTAB),(R8),(R9),(R0),0                      
*                                                                               
         L     R4,LARECTAB                                                      
         USING RTABD,R4                                                         
         MVC   IOKEY(13),APRECKEY                                               
*                                                                               
         XR    R8,R8               RESETTING R8 TO USE AS COUNTER               
**                                 ...FOR NUMBER OF RECORDS VIA BUYADD          
VALP7    ST    R4,LANXTREC                                                      
         OC    RTSORT,RTSORT       TEST END OF RECORDS                          
         BZ    VALP15                                                           
******** OC    QSTA,QSTA           TEST STATION FILTER                          
******** BNZ   VALP7A                                                           
         CLC   RTSTA,LSVSTA        NO-TEST NEW STATION                          
         BE    VALP7A                                                           
         MVC   LSVSTA,RTSTA        YES-GET STATION DETAILS                      
         GOTO1 AGETSTA,LSVSTA                                                   
         MVC   LPST,ACWORK+24                                                   
         USING EXTRAWKD,RF                                                      
         L     RF,AEXTRAWK                                                      
         NI    XTRAFLG1,X'FF'-X'40'  REPEAT CHKAUTH CODE                        
         DROP  RF                                                               
*                                                                               
VALP7A   LA    R3,IOKEY                                                         
         MVC   BWDKELST,RTKSTACD                                                
         MVC   BWDKELPO,RTKPO                                                   
         MVC   BWDKELDY,RTKDAYS                                                 
         MVC   BWDKELTM,RTKTIMES                                                
         MVI   BWDKELSQ,0                                                       
         LA    R1,MINHI2                                                        
         B     VALP8+4                                                          
*                                                                               
VALP8    LA    R1,MINSEQ2                                                       
         GOTO1 AMIN                                                             
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     VALP12                                                           
         CLC   BWDKEY(BWDKELSQ-BWDKEY),IOKEYSAV                                 
         BNE   VALP12                                                           
         L     R3,AIOAREA2                                                      
         CLI   BDPT,0              CHECK DAYPART/LENGTH FILTERS                 
         BE    *+14                                                             
         CLC   BDPT,BWDDPT                                                      
         BNE   VALP8                                                            
         CLI   BSLN,0                                                           
         BE    *+14                                                             
         CLC   BSLN,BWDSLN                                                      
         BNE   VALP8                                                            
         MVC   LDETKEY,BWDKEY      SAVE THE KEY                                 
*****                                                                           
         GOTO1 VMSPACK,APPARM,QMKT,BWDSTA,APWORK  GET BINARY                    
         BE    VALP8X                                                           
         LA    R2,BWSMSGH                                                       
         XC    8(L'BWSMSG,R2),8(R2)                                             
         MVC   8(L'BADCABLE,R2),BADCABLE                                        
         MVC   8+L'BADCABLE(4,R2),BWDSTA                                        
         OI    6(R2),X'80'                                                      
         LA    R2,BWSRECH                                                       
         DC    H'0'                                                             
         DC    C'$ABEND'                                                        
*                                                                               
BADCABLE DC    C'CANNOT ADD!  BAD CABLE SYSTEM: '                               
*****                                                                           
VALP8X   MVC   BSTA,APWORK+2                      STATION VALUE                 
*                                                                               
VALP9    BAS   RE,BUYADD           LET'S DO BUYADD                              
         BNE   VALPX                                                            
         LA    R8,1(R8)            BUMP COUNTER - NUM OF RECORDS                
         MVC   IOKEY(13),LDETKEY   NO-RESTORE DETAIL RECORD KEY                 
         GOTO1 AMIN,MINRD2                                                      
         BE    *+6                 VERY BAD IF WE DON'T FIND IT NOW             
         DC    H'0'                                                             
         L     R3,AIOAREA2         POINT TO THE DETAIL RECORD                   
         USING BWDRECD,R3                                                       
         OI    BWDINDS2,BWDINBR    NOW LINKED TO NBR RECORD                     
         GOTO1 AMIN,MINWRT2        LET'S UPDATE THE RECORD NOW                  
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    LIND,LBYAYES        WE DID ADDED AT LEAST 1 BRV                  
***  REREAD TO PUT IT BACK INTO THE SEQUENCE                                    
         MVC   IOKEY(13),LDETKEY   NO-RESTORE DETAIL RECORD KEY                 
         GOTO1 AMIN,MINRD2                                                      
         BE    *+6                 VERY BAD IF WE DON'T FIND IT NOW             
         DC    H'0'                                                             
***                                                                             
         B     VALP8                                                            
*                                                                               
VALP12   L     R4,LANXTREC                                                      
         LA    R4,RTABL(R4)                                                     
         B     VALP7                                                            
                                                                                
***  WE'RE DONE, NO MORE NEED TO BE MOVED TO BUYADD                             
VALP15   TM    LIND,LBYAYES                                                     
         BNO   VALP97                                                           
***                                                                             
         BRAS  RE,HDRINFO          NEED TO UPDATE HEADER INFO                   
***                                                                             
         XC    BWSMSG,BWSMSG                                                    
         MVC   BWSMSG(L'BYAMSG),BYAMSG                                          
         CVD   R8,APDUB                                                         
         UNPK  BWSMSG(3),APDUB                                                  
         MVI   BWSMSGH+5,L'BYAMSG                                               
         OI    BWSMSG+2,X'F0'                                                   
         MVC   FVMSGNO,=AL2(FVFSET)                                             
         B     VALP95                                                           
BYAMSG   DC    CL27'000 BUY REVISION(S) CREATED'                                
*                                                                               
VALP95   LA    R1,BWSKEYH                                                       
         ST    R1,FVADDR                                                        
         B     VALPX                                                            
*                                                                               
VALP97   XC    BWSMSG,BWSMSG                                                    
         MVC   BWSMSG(L'NOBYA),NOBYA   NO BUY REVISIONS CREATED                 
         MVC   FVMSGNO,=AL2(FVFSET)                                             
         B     VALPX                                                            
NOBYA    DC    CL24'NO BUY REVISIONS created'                                   
*                                                                               
VALPX    B     EXIT                                                             
*                                                                               
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* SAVE RECORD DETAILS TO RECORD TABLE FOR SUBSEQUENT SORT             *         
***********************************************************************         
         SPACE 1                                                                
SAVEREC  NTR1  ,                                                                
         L     R8,LANXTREC         A(NEXT ENTRY IN TABLE)                       
         USING RTABD,R8                                                         
         MVC   RTSTA,BWDSTA                                                     
         MVC   RTKSTACD,BWDKELST                                                
         LA    R4,SORTTAB                                                       
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
         CLC   BWDDAYS,0(R4)                                                    
         BNE   SAVR4                                                            
         CLC   BWDTIMES(2),1(R4)                                                
         BL    SAVR4                                                            
         CLC   BWDTIMES(2),3(R4)                                                
         BNL   SAVR4                                                            
         MVC   RTSEQ,5(R4)                                                      
         B     SAVR6                                                            
*                                                                               
SAVR4    LA    R4,6(R4)                                                         
         B     SAVR2                                                            
*                                                                               
SAVR6    MVI   RTDAYCD,1           DAY CODE                                     
         CLI   BWDDAYS,X'7C'       1=MO-FR                                      
         BE    SAVR8                                                            
         MVI   RTDAYCD,7           7=MO-SU                                      
         CLI   BWDDAYS,X'7F'                                                    
         BE    SAVR8                                                            
         MVI   RTDAYCD,8           8=SA-SU                                      
         CLI   BWDDAYS,X'03'                                                    
         BE    SAVR8                                                            
         SR    R1,R1               2-6=MIXED DAYS                               
         SR    RF,RF               9-15=SINGLE DAY                              
         ZIC   R0,BWDDAYS                                                       
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
         ICM   RE,3,BWDTIMES                                                    
         AHI   RE,-600             6A-MIDNIGHT,MIDIGHT-6A                       
         BNM   *+8                                                              
         AHI   RE,2400                                                          
         STCM  RE,3,RTTIMES                                                     
         ICM   RE,3,BWDTIMES+2                                                  
         AHI   RE,-600                                                          
         BNM   *+8                                                              
         AHI   RE,2400                                                          
         STCM  RE,3,RTTIMES+2                                                   
*                                                                               
SAVR10   MVC   RTKPO,BWDKELPO      RECORD KEY VALUES                            
         MVC   RTKDAYS,BWDKELDY                                                 
         MVC   RTKTIMES,BWDKELTM                                                
         LA    R8,RTABL(R8)                                                     
         C     R8,LARECTBX                                                      
         BL    *+6                                                              
         DC    H'0'                                                             
         ST    R8,LANXTREC         SAVE A(NEXT ENTRY)                           
*                                                                               
SAVRX    B     EXIT                                                             
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
* ROUTINE TO PERFORM BUY TRANSFER                                     *         
* INPUT  : R3 = A(BWS RECORD)                                         *         
* OUTPUT : FVMSGNO NE FVFOK IF ERROR                                  *         
***********************************************************************         
         SPACE 1                                                                
****  WE WILL BUILD THE NEW BUY REVISION RECORD RIGHT HERE                      
BUYADD   NTR1                                                                   
         USING BWDRECD,R3                                                       
         MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
BUYADD10 NI    LIND,X'FF'-LBRVDEL                                               
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         USING NBRRECD,R2                                                       
*                                                                               
         MVI   NBRKTYP,NBRKTYPQ    X'0D' TYPE                                   
         MVI   NBRKSTY,NBRKSTYQ    X'6B' SUBTYPE                                
         MVC   NBRKAGMD(4),BWDKAGMD   AGENCY/MD, BUYER, SEQUENCE NUMBER         
         MVC   NBRKSTA,BSTA                                                     
         MVI   NBRKNBSQ,1                                                       
         CLC   IOKEY(NBRKNBSQ-NBRKEY),LNBRKEY   SAME STATION AND STUFF?         
         BNE   BUYADD15             - YUP, USE THE BSEQNUM                      
         MVC   NBRKNBSQ,LNBRKEY+(NBRKNBSQ-NBRKEY)                               
*                                                                               
BUYADD15 GOTO1 AIO,DIRHI+IO1+IORDEL   READ FOR DELETES AS WELL                  
         BE    BUYADD20                                                         
         TM    IOERR,IOEDEL        RECORD IS DELETED?                           
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 AIO,FILGETU1+IORDEL   YES, GET NBR TO AIOAREA1                   
         BE    *+14                                                             
         TM    IOERR,IOEDEL                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   IOKEY(L'NBRKEY),IOKEYSAV                                         
         BNE   BUYADD23            DIDN'T FIND ANYTHING                         
         OI    LIND,LBRVDEL        FOUND A DELETED BRV                          
         B     BUYADD25                                                         
*                                                                               
BUYADD20 CLC   IOKEY(L'NBRKEY),IOKEYSAV                                         
         BNE   BUYADD23                                                         
         XR    R1,R1                                                            
         IC    R1,NBRKNBSQ                                                      
         LA    R1,1(R1)                                                         
         CHI   R1,255                                                           
         BH    EIIF                                                             
         STC   R1,NBRKNBSQ                                                      
         B     BUYADD15                                                         
*                                                                               
BUYADD23 MVC   IOKEY,IOKEYSAV      MOVE BACK THIS STUFF....NON-DELETE           
BUYADD25 MVC   LNBRKEY,IOKEY                                                    
*                                   ...NOT FOR USE FOR THIS PASS                
BUYADD35 L     RE,AIOAREA3         CLEAR OUT AIOAREA3                           
         LH    RF,=H'4000'         GONNA BUILD BUY REVISION HERE                
         XR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         L     R2,AIOAREA3         NBRRECD ON AIO3 NOW                          
         MVC   NBRKEY,IOKEY                                                     
         MVC   NBRLEN,=Y(NBRSLNQ+NBRFSTEL-NBRKEY)                               
         LA    R2,NBRFSTEL                                                      
         USING NBRSELD,R2                                                       
         LA    R3,BWDFSTEL                                                      
         USING BWDEL,R3                                                         
                                                                                
         MVI   NBRSEL,NBRSELQ      X'10' DESCRIPTION ELEMENT                    
         MVI   NBRSLN,NBRSLNQ                                                   
         MVC   NBRSPKOR,BWDPKOR    PACKAGE NUMBER                               
         MVC   NBRSDAYS,BWDDAYS                                                 
         MVC   NBRSSTA,BWDSTA                                                   
         MVC   NBRSTIMS(NBRSBOOK-NBRSTIMS),BWDTIMES   START/END TIMES           
*****                              ..DPT/SUB, SPOT LENGTH, COST 1 2 3           
*****                              .EFF DATE 23, PROGRAMMING, INDICATOR         
         MVC   NBRSBOOK(NBRSDTES-NBRSBOOK),BWDBOOK                              
*****                              ..ACTUAL UPGRADE SHARE BOOK, UPG PUT         
*****                              ..UPG SHR, INACTIVE WEEK MASK                
*****                              ..ALLOCATION RULE, USER CODE                 
*****                              ..DAYS FOR PACKAGE AND ORBIT                 
         MVC   NBRSREP,BWDREP                                                   
*****                              ..REP CODE (BINARY)                          
         GOTO1 VDATCON,APPARM,(5,0),(3,NBRSCRDT)                                
*****                              ..CREATION DATE IS 3 BYTE BINARY             
         OI    NBRSINDS,NBRSWBYA   CAME FROM WORK/BUYADD                        
         TM    CMPOPTS,CAMODLY     TEST DAILY SKED                              
         BZ    BUYADD50             - NOPE                                      
         OI    NBRSINDS,NBRSDALY   CAMPAIGN IS DAILY AT THIS MOMENT             
** NOTE **  NBRSDTES - START/END DATES SCHEDULE LEFT OUT ON PURPOSE!!           
*                                                                               
***  BUILDING DEMO ELEMENT                                                      
BUYADD50 XR    R0,R0                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         CLI   0(R3),0             NO MORE???                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R3),DMOELCDQ      X'02' ELEMENT                                
         BNE   BUYADD50                                                         
         USING DMOEL,R3                                                         
**                                                                              
         GOTO1 ELEMCOPY,APPARM,DMOELLN,NBRDMELQ,(R3)                            
***                                                                             
***  ANY OTHER ELEMENTS?                                                        
BUYADD55 XR    R0,R0                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         CLI   0(R3),0             NO MORE?                                     
         BE    BUYADD65             - NOPE, CONTINUE                            
         CLI   0(R3),SPWELCDQ      X'03' SPOTS PER WEEK ELEMENT?                
         BNE   BUYADD56             - NOPE, SKIP                                
         USING SPWEL,R3                                                         
         GOTO1 ELEMCOPY,APPARM,SPWELLN,NBRSPELQ,(R3)                            
         DROP  R3                                                               
         B     BUYADD55                                                         
*                                                                               
BUYADD56 CLI   0(R3),UPGELCDQ      X'04' UPGRADE ELEMENT?                       
         BNE   BUYADD57             - NOPE, SKIP                                
         USING UPGEL,R3                                                         
         GOTO1 ELEMCOPY,APPARM,UPGELLN,NBRUPELQ,(R3)                            
         DROP  R3                                                               
         B     BUYADD55                                                         
*                                                                               
BUYADD57 CLI   0(R3),ODTELCDQ      X'05' OVERRIDE DAY/TIME/STATION ELE?         
         BNE   BUYADD58             - NOPE, SKIP                                
         USING ODTEL,R3                                                         
         GOTO1 ELEMCOPY,APPARM,ODTELLN,NBRODELQ,(R3)                            
         DROP  R3                                                               
         B     BUYADD55                                                         
*                                                                               
BUYADD58 CLI   0(R3),COMELCDQ      X'06' COMMENT ELEMENT?                       
         BNE   BUYADD59             - NOPE, SKIP                                
         USING COMEL,R3                                                         
         GOTO1 ELEMCOPY,APPARM,COMELLN,NBRCMELQ,(R3)                            
         DROP  R3                                                               
         B     BUYADD55                                                         
*                                                                               
BUYADD59 CLI   0(R3),SHPELCDQ      X'08' SHARE/PUT OVERRIDE ELEMENT?            
         BNE   BUYADD60             - NOPE, SKIP                                
         USING SHPEL,R3                                                         
         GOTO1 ELEMCOPY,APPARM,SHPELLN,NBRSHELQ,(R3)                            
         DROP  R3                                                               
         B     BUYADD55                                                         
*                                                                               
BUYADD60 CLI   0(R3),BWIDECDQ      X'09' ID ELEMENT?                            
         BNE   BUYADD61             - NOPE, SKIP                                
         USING BWIDEL,R3                                                        
         GOTO1 ELEMCOPY,APPARM,BWIDELLN,NBRIDELQ,(R3)                           
         DROP  R3                                                               
         B     BUYADD55                                                         
*                                                                               
BUYADD61 CLI   0(R3),CS2ELCDQ      X'13' COST 2 OVERRIDE ELEMENT?               
         BNE   BUYADD55             - NOPE, NEXT ELEMENT PLZ                    
         USING CS2EL,R3                                                         
         GOTO1 ELEMCOPY,APPARM,CS2ELLN,NBRC2ELQ,(R3)                            
         DROP  R3                                                               
         B     BUYADD55                                                         
**                                                                              
***   WE'RE ADDING THE NWS DETAIL ELEMENT NOW                                   
BUYADD65 XC    APELEM,APELEM                                                    
         LA    R2,APELEM                                                        
         USING NBRDTELD,R2                                                      
         MVI   NBRDTEL,NBRDTELQ    X'80' NWS DETAIL ELEMENT                     
         MVI   NBRDTLEN,NBRDTLNQ                                                
         MVC   NBRDTKEY,LDETKEY    SAVE OFF THE DETAIL RECORD KEY               
         DROP  R2                                                               
         L     R2,AIOAREA3                                                      
         USING NBRRECD,R2                                                       
         GOTO1 AADDELS,NBRRECD                                                  
         DROP  R2                                                               
*                                                                               
*****  WE'RE GOING TO ADD THE BUY REVISION NOW!!                                
BUYADD70 L     R0,AIOAREA1                                                      
         L     RE,AIOAREA3                                                      
         LA    R1,2000                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    R1,FILADD1          ADD NBR RECORD                               
         TM    LIND,LBRVDEL        USING A DELETED RECORD                       
         BZ    *+8                                                              
         LA    R1,FILPUT1          PUT NBR RECORD                               
         GOTO1 AIO                 ADD/UPDATE THE BUY REVISION                  
*                                                                               
         TM    LIND,LBRVDEL        WAS THIS A DELETED BRV?                      
         BZ    BUYADD80             - NOPE, GET OUTTA HERE                      
         XC    IOKEY,IOKEY                                                      
         MVC   IOKEY(L'NBRKEY),LNBRKEY   NEED TO TAKE OFF DELETE BIT            
         GOTO1 AIO,DIRRDUP+IORDEL                                               
         BE    BUYADD80                                                         
         TM    IOERR,IOEDEL        RECORD IS DELETED?                           
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R3,IOKEY                                                         
         USING NBRRECD,R3                                                       
         NI    NBRKCNTL,X'FF'-X'80'    DELETE KEY                               
         DROP  R3                                                               
         GOTO1 AIO,DIRWRT                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
BUYADD80 XR    R1,R1               ...(DIRHI MIGHT HAVE CHANGED IT)             
         IC    R1,LNBRKEY+L'NBRKEY-1                                            
         LA    R1,1(R1)                                                         
         CHI   R1,256                                                           
         BH    EIIF                                                             
         STC   R1,LNBRKEY+L'NBRKEY-1   SAVING FOR NEXT NBR CREATION             
*                                                                               
BUYADDYS CLC   FVMSGNO,=AL2(FVFOK)                                              
         B     BUYADDX                                                          
*                                                                               
BUYADDX  B     EXIT                                                             
***********************************************************************         
* THIS SUBROUTINE COPIES ELEMENTS FROM BWDDTL TO NBR                  *         
* INPUT:       R2 POINTS TO THE BWDDTL TO BE COPIED TO NBR            *         
* OUTPUT:      THE NEW ELEMENT IS ADDED ON THE NEW NBR                *         
***********************************************************************         
ELEMCOPY NTR1                                                                   
         L     RE,0(R1)                                                         
         STC   RE,ELEMLN           SAVE OFF LENGTH OF ELEMENT                   
         L     RE,4(R1)                                                         
         STC   RE,ELEMCD           SAVE OFF THE ELEMENT CODE                    
         L     R3,8(R1)            R3 POINTS TO ELEM IN BWDREC                  
*                                                                               
         L     R2,AIOAREA3                                                      
         USING NBRRECD,R2                                                       
         XC    APELEM,APELEM                                                    
         LA    R4,APELEM                                                        
*                                                                               
         XR    RE,RE                                                            
         IC    RE,ELEMLN                                                        
         BCTR  RE,0                DECREMENT                                    
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(R3)       COPY THE WHOLE THING                         
         MVC   0(1,R4),ELEMCD                                                   
         GOTO1 AADDELS,NBRRECD                                                  
*                                                                               
ELEMCPX  B     EXIT                                                             
***                                                                             
*                                                                               
***********************************************************************         
* ERROR EXITS                                                         *         
***********************************************************************         
         SPACE 1                                                                
         USING TWAD,R5                                                          
ESAT     MVC   FVMSGNO,=AL2(FVIACTSA)                                           
         B     ERRORX                                                           
EMKT     MVC   FVMSGNO,=AL2(FVSTAXFR)                                           
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
EIIF     MVC   FVMSGNO,=AL2(FVFNOTV)                                            
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
***********************************                                             
* CALCULATES THE RATE OF THE SPOT SPECIFIED IN APELEM                           
*                                                                               
* ON ENTRY:    AIOAREA3            A(BUY RECORD)                                
*              APELEM              CONTAINS SPOT ELEMENT TO BE ADDED            
*                                                                               
* ON EXIT:     APFULL              CALCULATED RATE                              
***********************************                                             
BYLNRATE NTR1                                                                   
         L     RF,ASYS             A(SYSFACS)                                   
         L     RF,VCALLOV-SYSFACD(RF)                                           
         XC    APPARM(8*L'APPARM),APPARM                                        
         MVC   APPARM+4(4),=X'D9000A5F'    GETRATE                              
         GOTO1 (RF),APPARM,0                                                    
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,APPARM           A(GETRATE IN CORE)                           
*                                                                               
         XC    APWORK+32(32),APWORK+32   SINCE THE 1ST 21 BYTES OF              
         LA    RE,APWORK+32                  APWORK ARE BEING USED BY           
         ST    RE,APPARM                     BTREL, WE USE THE LAST 32          
         MVC   APPARM(1),BPRD                                                   
*                                                                               
*****E10 GOTO1 (RF),APPARM,,(0,AIOAREA3),(0,APELEM),(APROF7,0)                  
BYRATE10 MVI   APPARM+12,C'U'                                                   
         TM    APROFBTS,A00CANAD   TEST CANADIAN AGENCY                         
         BZ    *+8                                                              
         MVI   APPARM+12,C'C'                                                   
         GOTO1 (RF),APPARM,,(0,AIOAREA3),(0,APELEM),0                           
*                                                                               
         L     RE,APWORK+36                                                     
         L     RF,APWORK+44                                                     
         SR    RE,RF                                                            
         ST    RE,APFULL           GROSS DOLLARS LESS TAX                       
*                                                                               
BYRATEX  B     EXIT                                                             
***********************************************************************         
* EXTENTION ROUTINES                                                  *         
***********************************************************************         
         SPACE 1                                                                
EXTRA    NMOD1 0,**BW7X**,RA                                                    
         L     RC,APALOCAL                                                      
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         SRL   RF,24                                                            
         SLL   RF,2                                                             
         B     *+4(RF)                                                          
*                                                                               
         B     VALDT                                                            
*                                                                               
EQXIT    CR    RE,RE                                                            
         B     XIT                                                              
*                                                                               
NEQXIT   LTR   RB,RB                                                            
         B     XIT                                                              
*                                                                               
EXIT1    CLC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
XIT      J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE DATES OPTION AGAINST THE CAMPAIGN DATES                    *         
* OUTPUT : CC EQ - LDTINDS SET WITH WEEKS THAT ARE EXCLUDED           *         
*          CC NE - INVALID DATES                                      *         
***********************************************************************         
         SPACE 1                                                                
VALDT    CLI   INFSTART,0          TEST START YEAR GIVEN                        
         BNE   VALDT1                                                           
         ZIC   RE,CMPST            NO-USE CAMPAIGN START YEAR                   
         STC   RE,INFSTART                                                      
         CLC   INFSTART+1(2),CMPST+1   TEST M/D BEFORE CAMPAIGN START           
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
         CLC   INFSTART,CMPST      CHECK DATES ARE WITHIN CAMPAIGN              
         BL    VALDT8                                                           
         CLC   INFEND,CMPND                                                     
         BH    VALDT8                                                           
         GOTO1 VDATCON,APPARM,(3,INFSTART),(2,APFULL)                           
         GOTO1 (RF),(R1),(3,INFEND),(2,APFULL+2)                                
         LA    R2,LDTINDS                                                       
         L     R3,ATWA             SET WEEKS THAT ARE EXCLUDED                  
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
* CHECKS TO SEE IF THE CAMPAIGN HAS A IDR BUYER                                 
***********************************************************************         
CKCAMIDR DS    0H                                                               
         NMOD1 0,**CIDR**                                                       
         L     RC,0(R1)                                                         
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
CIDRX    CR    RC,RC                                                            
         J     EXIT                                                             
         DROP  R2                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* GETTING THE HEADER RECORD AND UPDATING THE INFELCD ELEMENT                    
*                                                                               
* ON EXIT:     INFELCD SHOULD BE SETUP WITH FLAG                                
*                                                                               
***********************************************************************         
HDRINFO  NTR1  BASE=*,LABEL=*                                                   
         XC    IOKEY,IOKEY         WE CAN BLOW IT AWAY NO PROBLEM               
         LA    R2,IOKEY            BUILD HEADER POINTER TO FIND                 
         USING BWHKEY,R2                                                        
         XC    BWHKEY,BWHKEY                                                    
         MVI   BWHKTYP,BWHKTYPQ                                                 
         MVI   BWHKSUB,BWHKSUBQ                                                 
         MVC   BWHKAGMD,BAGYMD                                                  
         OC    BWHKAGMD,BBYRMASK                                                
         MVC   BWHKBYR,BBYR                                                     
         MVC   BWHKCAM,BCAM                                                     
         MVC   BWHKMKT,BMKT                                                     
         DROP  R2                                                               
*                                                                               
         GOTO1 AIO,DIRHI+IO2                                                    
         CLC   IOKEY(BWHKSEQ-BWHKEY),IOKEYSAV                                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 AIO,FILGETU2                                                     
         L     R6,AIOAREA2                                                      
         LA    R6,BWHFSTEL-BWHKEY(R6)                                           
         XR    R0,R0                                                            
HDRINF10 CLI   0(R6),0             LOOK FOR INFO ELEMENT (X'06')                
         BE    HDRINF40            NEED TO ADD ONE THEN                         
         CLI   0(R6),INFELCDQ                                                   
         BE    HDRINF30                                                         
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     HDRINF10                                                         
*                                                                               
         USING INFELD,R6                                                        
HDRINF30 TM    INFFLAG1,IFF1BYRV   ALREADY DOING REVISIONS?                     
         BNZ   HDRINFX              - YES, GET OUTTA HERE                       
         OI    INFFLAG1,IFF1BYRV                                                
         B     HDRINF50                                                         
         DROP  R6                                                               
*                                                                               
HDRINF40 LA    R1,APELEM           SAVE ADDRESS WHERE TO PUT INFO ELEM          
         XC    APELEM,APELEM                                                    
         USING INFELD,R1                                                        
         MVI   INFELCD,INFELCDQ                                                 
         MVI   INFELLN,INFELLNQ                                                 
         OI    INFFLAG1,IFF1BYRV                                                
         DROP  R1                                                               
*                                                                               
         L     R6,AIOAREA2                                                      
         GOTO1 AADDELS,(R6)                                                     
*                                                                               
HDRINF50 LA    R1,FILPUT2          PUT NWH RECORD                               
         GOTO1 AIO                                                              
*                                                                               
HDRINFX  J     EXIT                                                             
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
GFLTRC40 CLC   CMPST,DFINFSDT                                                   
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
         CLC   CMPST,DFFLTSTR                                                   
         BL    GFLTRC60                                                         
         CLC   CMPST,DFFLTEND                                                   
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
GFLTRCX  J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
NDEMOS   EQU   14                                                               
***********************************************************************         
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
COMWRK   DS    0C                  COMMON BETWEEN BWS05 AND BWS07               
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE SPNWS05WRK                                                     
         PRINT ON                                                               
         SPACE 2                                                                
LOCALD   DSECT                                                                  
*                                                                               
******** ORG   LOCALD+2048                                                      
         ORG   LOCAL05X            BWS07 WORK AREA                              
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
LDTINDS  DS    XL(NMAXWKS)         DATE INDICATORS TABLE                        
LEXCLD   EQU   X'80'               THIS WEEK IS EXCLUDED FROM TRANSFER          
LWKINDS  DS    XL(NMAXWKS)         WEEK INDICATORS TABLE                        
LFRZ     EQU   X'80'               THIS WEEK IS FROZEN                          
LXFRTOT  DS    XL(NMAXWKS)         ORIGINAL TRANSFER TOTALS SPTS/WEEK           
LBUYTOT  DS    XL(NMAXWKS)         CURRENT BUYLINES TOTALS SPTS/WEEK            
LWKTAB   DS    XL((NMAXWKS+1)*8)   WEEKS TABLE (SEE BLDWKS FOR FORMAT)          
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
ELEMLN   DS    X                                                                
ELEMCD   DS    X                                                                
*                                                                               
LIND     DS    X                                                                
LBYAYES  EQU   X'80'               AT LEAST 1 BRV MADE FROM BUYADD              
LBRVDEL  EQU   X'40'               CURRENT BRV WAS DELETED                      
*                                                                               
LCHGIND  DS    X                   RECORD CHANGE INDICATOR                      
LSLN     EQU   X'01'                                                            
LDAYS    EQU   X'02'                                                            
LSTDATE  EQU   X'04'                                                            
LTIMES   EQU   X'08'                                                            
LTIMNOTX EQU   X'10'                                                            
LCST     EQU   X'20'                                                            
LEFFCOST EQU   X'40'                                                            
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
LSPWEL   DS    XL32                                                             
LSVSPWEL DS    XL32                                                             
LBTREL   DS    XL32                                                             
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
LNBRKEY  DS    XL13                SAVE THE NBR KEY                             
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
LSVSTA   DS    CL8                                                              
LSVDAYS  DS    XL1                                                              
*                                                                               
LORBDAY  DS    XL1                                                              
LORBTIM1 DS    XL2                                                              
LORBTIM2 DS    XL2                                                              
*                                                                               
LFLTSNUM DS    XL1                 INTERSECTING STARTING FLIGHT NUMBER          
LFLTNNUM DS    XL1                 INTERSECTING ENDING   FLIGHT NUMBER          
*                                                                               
BSEQNUM  DS    XL1                 BINARY SEQ NUMBER                            
*                                                                               
LACNTAB  DS    (LNACNTAB)CL15                                                   
LNACNTAB EQU   12                                                               
         EJECT                                                                  
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
         SPACE 1                                                                
LSKEDENL EQU   2+2*NMAXWKS         ENTRY LENGTH                                 
LSKEDS   DS    21XL(LSKEDENL)      TABLE                                        
LCHG     EQU   X'80'               BUYLINE CHANGE INDICATOR                     
LADD     EQU   X'40'               ADD BUY ELEMENT(S) TO BUYLINE                
LDEL     EQU   X'20'               DELETE ALL SPOTS FROM BUYLINE                
LAFD     EQU   X'10'               BUY HAS MATCHED SPOTS                        
LPAY     EQU   X'08'               BUY HAS PAID SPOTS                           
LDONTCHG EQU   X'04'               DON'T CHANGE THE BUY                         
LBDSTART EQU   X'02'               BUY START DATE CHANGED                       
LSKEDSL  EQU   *-LSKEDS            LENGTH OF TABLE                              
LSKEDSX  EQU   *                   END OF TABLE                                 
         SPACE 1                                                                
*                                                                               
*  CAUTION : MAKE SURE W/S DOES NOT EXTEND BEYOND 4096 BYTES !!                 
*                                                                               
         ORG                                                                    
         ORG   LOCALD+4096                                                      
LOCALX   EQU   *                                                                
         EJECT                                                                  
***********************************************************************         
* EXTRA WORKING STORAGE FOR THIS OVERLAY                              *         
***********************************************************************         
EXTRAWKD DSECT                                                                  
XTRAFLG1 DS    XL1                 EXTRA FLAGS                                  
XF1NXESL EQU   X'80'                - SOME LINES NOT XFR CAUSE OF ESLN          
XAUTH    EQU   X'40'               IF ON, ALREADY WENT THROUGH CODE             
XSDE     EQU   X'20'               IF ON, SDESK AUTH OPEN FOR PRD OPTN          
*                                                                               
BTCHFLG1 DS    XL1                 BATCH FLAGS                                  
BFL1DSNT EQU   X'80'               - WE HAVE A DARE ORDER THAT WAS SENT         
*                                                                               
BYLNNSPT DS    XL1                 # OF SPOTS FOR THIS BUYLINE                  
BYLNNDOL DS    XL4                 # OF DOLLARS FOR THIS BUYLINE                
LSTABYTB DS    XL((MAXSTA+1)*STABYL)   STATION/BUYLINE TABLE                    
EXTRAWKL EQU   *-EXTRAWKD                                                       
         EJECT                                                                  
STABYD   DSECT                     DSECT FOR STATION/BUYLINE TABLE              
SBSTA    DS    CL8                 STATION                                      
SBLNLO   DS    PL2                 LOW BUY LINE                                 
SBLNHI   DS    PL2                 HIGH BUY LINE                                
STABYL   EQU   *-STABYD                                                         
         SPACE 2                                                                
RTABD    DSECT                     DSECT FOR RECORD TABLE                       
RTSORT   DS    0CL7                                                             
RTKSTACD DS    XL1                                                              
RTSEQ    DS    XL1                                                              
RTDAYCD  DS    XL1                                                              
RTTIMES  DS    XL4                                                              
*                                                                               
RTSTA    DS    CL8                 RECORD PORTION LESS THE KEY                  
RTKPO    DS    XL1                                                              
RTKDAYS  DS    XL2                                                              
RTKTIMES DS    XL2                                                              
RTABL    EQU   *-RTABD                                                          
         EJECT                                                                  
TWAD     DSECT                                                                  
         ORG   BWSTABH                                                          
* SPNWSFB                                                                       
* SPNWSCAM                                                                      
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
         PRINT OFF                                                              
       ++INCLUDE SPNWSFBD                                                       
         EJECT                                                                  
       ++INCLUDE SPNWSHDR                                                       
         EJECT                                                                  
       ++INCLUDE SPNWSCAM                                                       
         EJECT                                                                  
       ++INCLUDE SPNWSBRV                                                       
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
       ++INCLUDE DDGLVXCTLD                                                     
         EJECT                                                                  
       ++INCLUDE DDGLOBEQUS                                                     
         EJECT                                                                  
REQHDRD  DSECT                                                                  
       ++INCLUDE DDREQHDR                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009SPNWS0C   02/26/07'                                      
         END                                                                    
