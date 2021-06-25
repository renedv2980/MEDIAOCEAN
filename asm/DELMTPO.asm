*          DATA SET DELMTPO    AT LEVEL 047 AS OF 11/01/11                      
*PROCESS USING(WARN(15))                                                        
*PHASE DELMTPOB                                                                 
         TITLE 'DEMCON - LOCAL MONTHLIES - OUTPUT PHASE'                        
DELMTPO  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 TPTWRKX-TPTWRKD,**LMTPO,R7                                       
         USING TPTWRKD,RC          RC=A(W/S)                                    
         USING DEMCOND,R8          R8=A(GLOBAL W/S)                             
         L     R9,ASREC                                                         
         USING INTERD,R9           R9=A(SORT RECORD)                            
         L     RA,ACOMFACS                                                      
         USING COMFACSD,RA         RA=A(COMMON FACILITIES)                      
         B     *+4(R1)                                                          
         B     CNV2                PROCESS A RECORD                             
         B     CNV4                LAST TIME HOOK                               
*                                                                               
CNV2     CLI   INTRTYP,BSCODEQU    STATION/MARKET PASSIVES                      
         BE    CNV10                                                            
         CLI   INTRTYP,DRCODEQU    RATINGS QUARTER HOUR RECORD                  
         BE    CNV14                                                            
         CLI   INTRTYP,PICODEQU    PROGRAM INDEX PASSIVES                       
         BE    CNV60                                                            
         B     CNVX                                                             
*                                                                               
CNV4     MVI   INTRTYP,X'FF'                                                    
         B     CNV16               GO WRITE LAST RECORD                         
*                                                                               
CNVXH    DS    0H                                                               
         LHI   R0,1                                                             
         J     CNVXCR                                                           
                                                                                
CNVXL    DS    0H                                                               
         LHI   R0,-1                                                            
         J     CNVXCR                                                           
                                                                                
CNVXE    DS    0H                                                               
         SR    R0,R0                                                            
         J     CNVXCR                                                           
                                                                                
CNVXCR   DS    0H                                                               
         AR    R0,RB                                                            
         CR    R0,RB                                                            
         J     CNVX                                                             
                                                                                
CNVX     XMOD1 1                                                                
         EJECT                                                                  
* BUILD STATION/MARKET, MARKET/STATION & STATION/BOOK PASSIVES                  
*                                                                               
CNV10    LA    R6,THISKEY                                                       
         USING BSKEY,R6            BUILD STATION/MARKET RECORD                  
         XC    THISKEY,THISKEY                                                  
         MVI   BSCODE,BSCODEQU                                                  
         MVC   BSMEDIA,MEDIA                                                    
         MVC   BSSRC,OUTSRC                                                     
         MVC   BSBOOK,INTBOOK                                                   
         XC    BSBOOK,=X'FFFF'                                                  
         MVI   BSIND,BSINDEQU                                                   
         MVC   BSSTAT,INTSTA                                                    
         CLI   INTSPILL,C'Y'                                                    
         BNE   *+10                                                             
         MVC   BSKMKT,INTMRKT                                                   
         MVC   BSSTYP,INTSTYP                                                   
         MVC   BSBTYP,INTBTYP      INPUT PHASE BOOK TYPE                        
         MVC   BSRMKT,INTMRKT                                                   
         GOTO1 ABLDREC,DMCB,(C'P',BSKEY)                                        
         GOTO1 APUTTAPE                                                         
*                                                                               
         USING MLKEY,R6            BUILD MARKET/STATION RECORD                  
         MVI   MLIND,MLINDEQU                                                   
         MVC   MLRMKT,INTMRKT                                                   
         MVC   MLSTAT,INTSTA                                                    
         XC    MLKMKT,MLKMKT                                                    
         CLI   INTSPILL,C'Y'                                                    
         BNE   *+10                                                             
         MVC   MLKMKT,INTMRKT                                                   
         MVC   MLBTYP,INTBTYP      INPUT PHASE BOOK TYPE                        
         GOTO1 ABLDREC,DMCB,(C'P',MLKEY)                                        
         GOTO1 APUTTAPE                                                         
*                                                                               
         USING SBKEY,R6            BUILD STATION/BOOK RECORD                    
         XC    THISKEY,THISKEY                                                  
         MVI   SBCODE,SBCODEQU                                                  
         MVC   SBMEDIA,MEDIA                                                    
         MVC   SBSRC,OUTSRC                                                     
         MVC   SBSTAT,INTSTA                                                    
         CLI   INTSPILL,C'Y'                                                    
         BNE   *+10                                                             
         MVC   SBKMKT,INTMRKT                                                   
         MVC   SBSTYP,INTSTYP                                                   
         MVC   SBBTYP,INTBTYP      INPUT PHASE BOOK TYPE                        
         MVC   SBRMKT,INTMRKT                                                   
         MVC   SBBOOK,INTBOOK                                                   
         GOTO1 ABLDREC,DMCB,(C'P',SBKEY)                                        
         GOTO1 APUTTAPE                                                         
         B     CNVX                                                             
         EJECT                                                                  
* BUILD DEMO RECORDS                                                            
*                                                                               
CNV14    LA    R6,THISKEY                                                       
         USING DRKEY,R6            BUILD KEY                                    
         XC    THISKEY,THISKEY                                                  
         MVI   DRCODE,DRCODEQU                                                  
         MVC   DRMEDIA,MEDIA                                                    
         MVC   DRSRC,OUTSRC                                                     
         MVC   DRSTAT,INTSTA                                                    
         MVC   DRBOOK,INTBOOK                                                   
         XC    DRBOOK,=X'FFFF'                                                  
         CLI   INTSPILL,C'Y'                                                    
         BNE   *+10                                                             
         MVC   DRKMKT,INTMRKT                                                   
         MVC   DRSTYP,INTSTYP                                                   
         MVC   DRBTYP,INTBTYP      INPUT PHASE BOOK TYPE                        
         MVI   CONDFLAG,0          SET DEMO ELEMENT BUILD FLAG                  
         CLC   THISKEY(L'DRKMAJOR),PREVKEY                                      
         BE    CNV28                                                            
*                                  MAJOR KEY CONTROL BREAK                      
CNV16    CLI   PUTSW,C'Y'          TEST IF LAST RECORD WRITTEN                  
         BE    CNV18                                                            
         L     R6,AOREC                                                         
         LA    R6,4(R6)                                                         
         MVC   DRHIGHD,PREVDAY     SET HIGH DAY & QTR HOUR IN KEY               
         MVC   DRHIQHR,PREVQHR                                                  
         BAS   RE,CHKDEMO          TEST TO SEE IF CONVERSION GOING OK           
         GOTO1 APUTTAPE            PUT LAST RECORD TO TAPE                      
         MVI   PUTSW,C'Y'                                                       
*                                                                               
CNV18    XC    PREVKEY,PREVKEY     CLEAR LAST TIME VALUES                       
         XC    PREVPNAM,PREVPNAM                                                
         CLI   INTRTYP,X'FF'       EXIT IF LAST TIME HOOK                       
         BNE   CNV20                                                            
         MVI   INTRTYP,DRCODEQU                                                 
         B     CNVX                                                             
*                                  BUILD DEMO RECORDS                           
CNV20    GOTO1 ABLDREC,DMCB,THISKEY                                             
         MVC   PREVKEY,THISKEY                                                  
         LA    R6,TEMP                                                          
         USING MARELEM,R6          BUILD MARKET ELEMENT                         
         MVI   MARCODE,MARCODEQ                                                 
         MVI   MARELN,MARLNEQ                                                   
         MVC   MARNO,INTMRKT                                                    
         MVC   MARTYPE,INTMTYP                                                  
         MVC   MARSTYP,INTSTYP                                                  
         MVC   MARDATE,TODAYB                                                   
         GOTO1 APUTEL,MARELEM                                                   
*                                                                               
CNV28    LA    R6,TEMP                                                          
         USING QHELEM,R6                                                        
         MVI   QHCODE,QHCODEQ                                                   
         MVC   QHDAY,INTDAY                                                     
         MVC   QHSQH,INTSQH                                                     
         MVC   QHEQH,INTEQH                                                     
         MVC   QHWKS,INTWEEKS                                                   
         MVC   QHPNAME(L'INTPNAM),INTPNAM                                       
         LA    R1,QHPNAME+L'INTPNAM-1                                           
         CLI   0(R1),C' '          LOCATE END OF PROGRAM NAME                   
         BNE   *+8                                                              
         BCT   R1,*-8                                                           
         LA    R1,1(R1)                                                         
         SR    R1,R6                                                            
         STC   R1,QHELN            SET ELEMENT LENGTH                           
*                                                                               
         LA    R3,TEMP2            BUILD INFO ELEMENT                           
         USING QIELEM,R3                                                        
         MVI   QICODE,QICODEQ                                                   
         MVI   QIELN,QIELNEQ1                                                   
         MVC   QIPNUM(3),INTPNUM+1                                              
         MVC   QIPTYPE,INTPTYP                                                  
         MVC   QIPNAM6,INTPNAM6                                                 
         MVI   QIREV,1             REVISION 1 FOR PROGRAM SOURCE                
         MVC   QIPRSRC,INTPRSRC    PROGRAM SOURCE                               
         MVC   QIAFFIL,MYSPACES                                                 
         OC    INTAFFL,INTAFFL     IF AFFIL IS SPACES OR ZEROS                  
         BZ    CNV28A                LEAVE IT AS SPACES                         
         CLC   INTAFFL,MYSPACES                                                 
         BE    CNV28A                                                           
         MVC   QIAFFIL,INTAFFL     OTHERWISE, COPY FROM INTERM REC              
         DROP  R3                                                               
*                                                                               
CNV28A   LA    R3,TEMP3            BUILD THE PROGRAM AIRED ELEMENT              
         USING PDAELEM,R3                                                       
         MVI   PDACODE,PDACODEQ                                                 
         MVI   PDAELN,PDALENQ                                                   
         MVC   PDADNUM,INTPDNUM                                                 
         MVC   PDADAYS,INTPDMAP                                                 
         DROP  R3                                                               
*                                                                               
CNV29    CLI   PUTSW,C'Y'          TEST IF DEMO RECORD WRITTEN                  
         BNE   CNV30                                                            
         GOTO1 APUTEL,QHELEM       NO - ADD ELEMENT                             
*                                                                               
CNV30    CLI   CONDFLAG,0          TEST IF DEMO ELEMENTS BUILT                  
         BNE   CNV33                                                            
*                                                                               
CNV31    OC    ATRACDCB,ATRACDCB                                                
         BZ    CNV31A                                                           
         L     R2,ATRACDCB                                                      
         PUT   (R2),4(R9)                                                       
*                                                                               
CNV31A   LA    R5,DBLOCKA                                                       
         USING DBLOCKD,R5                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'TP '                                                   
         MVC   DBSELMED,MEDIA                                                   
         MVC   DBSELSRC,OUTSRC                                                  
         MVC   DBSELBK,INTBOOK                                                  
         ST    RA,DBCOMFCS                                                      
         MOVE  (CONDLEN,1000),INTACCS                                           
         GOTO1 CDEMEL,DMCB,(C'C',0),DBLOCKD,CONDLEN                             
         CLI   DBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   CONDFLAG,1                                                       
CNV32    GOTO1 CHELLO,DMCB,(C'P',CORE),CONDLEN,TEMP2                            
         GOTO1 CHELLO,DMCB,(C'P',CORE),CONDLEN,TEMP3                            
*                                                                               
CNV33    CLI   PUTSW,C'Y'          TEST IF DEMO RECORD WRITTEN                  
         BNE   CNV38                                                            
         LA    R1,CONDWRK1         NO - ADD DEMO ELEMENTS TO RECORD             
         SR    R0,R0                                                            
*                                                                               
CNV34    CLI   0(R1),0             TEST E-O-L                                   
         BE    CNV36                                                            
         GOTO1 APUTEL,(R1)                                                      
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     CNV34                                                            
*                                  SAVE THIS TIME VALUES                        
CNV36    MVC   PREVDAY,INTDAY                                                   
         MVC   PREVQHR,INTEQH                                                   
         MVC   PREVPNAM,INTPNAM                                                 
         MVI   PUTSW,C'N'          SET DEMO RECORD NOT WRITTEN                  
         B     CNVX                                                             
         EJECT                                                                  
* CONDENSE QTR HOUR & DEMO ELEMENTS. PROGRAM NAME WILL BE DROPPED FROM          
* QTR HR ELEMENT IF IT MATCHES PREVIOUS. 4BYTE DEMO ELEMENTS WILL BE            
* CREATED IF ELEMENTS EXACTLY MATCH ANY PREVIOUS QTR HR ON THIS RECORD.         
*                                                                               
CNV38    LA    R1,CONDWRK2         R1=A(OUTPUT,STRING)                          
         LA    R6,TEMP                                                          
         USING QHELEM,R6           R6=A(QTR HOUR ELEMENT)                       
         SR    RE,RE                                                            
         IC    RE,QHELN            GET LENGTH (INCLUDING PROGRAM NAME)          
         CLC   INTPNAM,PREVPNAM    TEST IF PROGRAM NAME MATCHES PREV.           
         BNE   *+8                                                              
         LA    RE,QHPNAME-QHELEM   GET LENGTH (EXCLUDING PROGRAM NAME)          
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),QHELEM      MOVE ELEMENT TO OUTPUT STRING                
         STC   RE,1(R1)            SET NEW ELEMENT LENGTH                       
         AR    R1,RE                                                            
         MVI   0(R1),0                                                          
         L     R6,AOREC                                                         
         LA    R6,4(R6)                                                         
         USING DRKEY,R6                                                         
         ST    R6,AREC                                                          
         MVC   RECLEN,DRRLEN       SAVE CURRENT OUTPUT RECORD LENGTH            
         LA    R6,DRFRSTEL                                                      
         ST    R6,AFRSTEL          SAVE A(FIRST ELEMENT)                        
         LA    R2,CONDWRK1         R2=A(INPUT STRING)                           
*                                  CONDENSE DEMO ELEMENTS                       
CNV40    CLI   0(R2),0             TEST E-O-L                                   
         BE    CNV48                                                            
         L     R6,AFRSTEL          R6=A(FIRST ELEMENT)                          
*                                  FIND MATCH WITH PREVIOUS ELEMENT             
CNV42    CLI   0(R6),0             TEST E-O-R                                   
         BE    CNV44                                                            
         IC    RE,1(R6)                                                         
         BCTR  RE,0                GET L'ELEMENT-1                              
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R6),0(R2)       MATCH RECORD EL WITH INPUT EL                
         BE    *+12                                                             
         LA    R6,1(RE,R6)         NOT EQUAL - BUMP TO NEXT                     
         B     CNV42                                                            
*                                  BUILD DUPLICATE DEMO ELEMENT                 
         MVC   0(1,R1),0(R2)                                                    
         MVI   1(R1),4                                                          
         S     R6,AREC                                                          
         STCM  R6,3,2(R1)          SET DISP INTO RECORD OF DUPLICATE            
         OI    2(R1),X'80'                                                      
         LA    R1,4(R1)                                                         
         B     CNV46                                                            
*                                  MOVE INPUT EL TO OUTPUT STRING               
CNV44    IC    RE,1(R2)                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),0(R2)                                                    
         AR    R1,RE                                                            
*                                  BUMP TO NEXT INPUT ELEMENT                   
CNV46    MVI   0(R1),0                                                          
         IC    RE,1(R2)                                                         
         AR    R2,RE                                                            
         B     CNV40                                                            
*                                  TEST IF CONDENSED STRING WILL FIT            
CNV48    LA    RE,CONDWRK2                                                      
         SR    R1,RE                                                            
         AH    R1,RECLEN                                                        
         CH    R1,MAXLEN                                                        
         BNH   CNV50                                                            
*                                  NO - WRITE PREVIOUS DEMO RECORD              
         MVI   PUTSW,C'N'                                                       
         B     CNV16                                                            
*                                  YES - ADD CONDENSED ELS TO RECORD            
CNV50    LA    R1,CONDWRK2                                                      
         SR    R0,R0                                                            
         B     CNV34                                                            
         EJECT                                                                  
***********************************************************************         
*================== PROGRAM INDEX RECORDS (PASSIVE) ==================*         
CNV60    DS    0H                                                               
         LA    R6,MYPIKEY                                                       
         USING PIKEY,R6                                                         
                                                                                
         CLC   PIKEY(MAJPIKYL),INTKEY                                           
         BE    CNV65                                                            
         OC    PIKEY(L'PIKMAJOR),PIKEY                                          
         BZ    CNV63                                                            
         GOTO1 ABLDREC,DMCB,(C'P',PIKEY)                                        
         GOTO1 APUTTAPE                                                         
*                                                                               
CNV63    MVC   PIKEY(L'PIKMAJOR),INTKEY                                         
         B     CNV67                                                            
*                                                                               
CNV65    CLC   PISQH,INTKEY+EQHDISP                                             
         BNH   *+6                                                              
         DC    H'0'                                                             
         MVC   PIEQH,INTKEY+EQHDISP                                             
         B     CNV60X                                                           
*                                                                               
CNV67    CLI   PIMEDIA,X'FF'                                                    
         BNE   CNV60X                                                           
         XC    MYPIKEY(20),MYPIKEY                                              
*                                                                               
CNV60X   B     CNVX                                                             
         DROP  R6                                                               
         SPACE 2                                                                
MAJPIKYL EQU   PIDAY-PIKEY+1                                                    
EQHDISP  EQU   PIEQH-PIKEY                                                      
***********************************************************************         
*============================= CHECK DEMO ============================*         
                                                                                
* THIS IS MORE OF A TRAP TO SEE IF THE CONVERSION IS GOING OKAY                 
* AT ENTRY,                                                                     
*   AOREC = A(RECORD TO BE RELEASED TO TAPE)                                    
                                                                                
         DS    0H                                                               
CHKDEMO  NTR1                                                                   
         L     R6,AOREC                                                         
         LA    R6,4(R6)                                                         
         USING DRKEY,R6                                                         
         TM    DRSTAT,X'F0'                                                     
         BO    CKDX                                                             
                                                                                
         LA    R3,DRFRSTEL                                                      
         B     CKD30                                                            
*                                                                               
CKD20    DS    0H                  R3-->QH ELEMENT                              
         XC    HALF,HALF           USE HALF FOR 2 FLAGS                         
         LR    RE,R3               GET RE-->X'31' ELEMENT                       
         LR    RF,R3               GET RF-->X'33' ELEMENT                       
                                                                                
         SR    R0,R0                                                            
CKD22A   ICM   R0,1,1(RE)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    RE,R0                                                            
         CLI   0(RE),0                                                          
         BE    CKD24                                                            
         CLI   0(RE),X'31'                                                      
         BNE   CKD22A                                                           
         LR    RF,RE               POINT RF CLOSER TO X'33' ELEM                
         CLI   1(RE),4             CHECK IF BACKWARD POINTER                    
         BH    CKD22C                                                           
         ICM   R1,3,2(RE)           IF IT IS,                                   
         SLL   R1,17                                                            
         SRL   R1,17                                                            
         LA    RE,DRKEY(R1)          ADJUST RE TO REAL DEMO ELEMENT             
CKD22C   SR    R1,R1               RE-->ELEM W/ DEMO VALUES                     
         IC    R1,2(RE)            R1 = L(EACH CELL)                            
         SLL   R1,28               ACTUALLY, THE LENGTH IS                      
         SRL   R1,28                STORED IN LOWEST NIBBLE                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         OC    3(0,RE),3(RE)       3(?,RE) = RHOMES VALUE                       
         BZ    *+8                                                              
         MVI   HALF,1                                                           
                                                                                
CKD24    DS    0H                  BUMP RF TO X'33' ELEMENT                     
         SR    R0,R0                                                            
CKD24A   ICM   R0,1,1(RF)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    RF,R0                                                            
         CLI   0(RF),0                                                          
         BE    CKD25                                                            
         CLI   0(RF),X'33'                                                      
         BNE   CKD24A                                                           
         CLI   1(RF),4             CHECK IF BACKWARD POINTER                    
         BH    CKD24C                                                           
         ICM   R1,3,2(RF)           IF IT IS,                                   
         SLL   R1,17                                                            
         SRL   R1,17                                                            
         LA    RF,DRKEY(R1)          ADJUST RF TO REAL DEMO ELEMENT             
CKD24C   SR    R1,R1               RF-->ELEM W/ DEMO VALUES                     
         IC    R1,2(RF)            R1 = L(EACH CELL)                            
         SLL   R1,28               ACTUALLY, THE LENGTH IS                      
         SRL   R1,28                STORED IN LOWEST NIBBLE                     
         AR    RF,R1                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         OC    3(0,RF),3(RF)       3(?,RF) = DHOMES VALUE                       
         BZ    *+8                                                              
         MVI   HALF+1,1                                                         
                                                                                
CKD25    DS    0H                  THESE CONDITIONS MUST BE MET                 
         B     CKD30               KILL THIS TEST - AINT TRUE FOR SRC           
         CLI   HALF,1               IF RHOMES > 0,                              
         BNE   *+14                                                             
         CLI   HALF+1,1              THEN DHOMES MUST BE > 0                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CKD30    DS    0H                                                               
         SR    R0,R0                                                            
CKD32    ICM   R0,1,1(R3)                                                       
         BNZ   *+6                                                              
         DC    H'0'                ELEM LENGTH CAN'T BE ZERO                    
         AR    R3,R0                                                            
         CLI   0(R3),0                                                          
         BE    CKD40                                                            
         CLI   0(R3),QHCODEQ       BUMP TO NEXT QH ELEM                         
         BNE   CKD32                                                            
         B     CKD20                                                            
*                                                                               
CKD40    DS    0H                                                               
         B     CKDX                                                             
         DROP  R6                                                               
*                                                                               
CKDX     DS    0H                                                               
         B     CNVX                                                             
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*======================== GENERIC DIVIDE LOGIC =======================*         
                                                                                
* At entry, DIVIDEND, DIVISOR are set.                                          
* At exit, QUOTIENT and REMAINDR are set.                                       
                                                                                
DIVIDE   NTR1                                                                   
         XC    QUOTIENT,QUOTIENT                                                
         XC    REMAINDR,REMAINDR                                                
                                                                                
         ICM   RF,15,DIVISOR       IF DIVISOR IS ZERO,                          
         BZ    DIVIDEX              CALLER GETS ZERO BACK                       
         OC    DIVIDEND,DIVIDEND   IF DIVIDEND IS ZERO                          
         BZ    DIVIDEX              CALLER GETS ZERO BACK ALSO                  
                                                                                
         DS    0H                  CALCULATE QUOTIENT                           
         LM    R0,R1,DIVIDEND                                                   
         SLDA  R0,1                DOUBLE DIVIDEND                              
         DR    R0,RF                                                            
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AHI   R1,1                                                             
         SRA   R1,1                                                             
         ST    R1,QUOTIENT                                                      
                                                                                
         DS    0H                  CALCULATE REMAINDER                          
         AHI   R0,1                                                             
         SRA   R0,1                                                             
         ST    R0,REMAINDR                                                      
                                                                                
DIVIDEX  DS    0H                                                               
         B     CNVX                                                             
***********************************************************************         
         EJECT                                                                  
* LITERALS ETC.                                                                 
*                                                                               
         LTORG                                                                  
DAYTAB   DS    0XL(1+3)                                                         
         DC     X'10',C'MON'        MONDAY                                      
         DC     X'20',C'TUE'        TUESDAY                                     
         DC     X'30',C'WED'        WEDNESDAY                                   
         DC     X'40',C'THU'        THURSDAY                                    
         DC     X'50',C'FRI'        FRIDAY                                      
         DC     X'60',C'SAT'        SATDAY                                      
         DC     X'70',C'SUN'        SUNDAY                                      
         DC     X'95',C'M-F'        MON-FRI                                     
         DC    X'00'                                                            
*                                                                               
TEMP2    DS    XL255                                                            
TEMP3    DS    XL255                                                            
MYSPACES DC    CL30' '                                                          
                                                                                
DIVIDEND DS    D                                                                
DIVISOR  DS    F                                                                
REMAINDR DS    F                                                                
QUOTIENT DS    F                                                                
*                                                                               
MYSTAT   DS    X                                                                
THISKEY  DC    XL20'00'                                                         
PREVKEY  DC    XL20'00'                                                         
MYPIKEY  DC    20X'00'                                                          
PREVDAY  DC    X'00'                                                            
PREVQHR  DC    X'00'                                                            
PREVPNAM DC    XL14'00'                                                         
*                                                                               
PUTSW    DC    C'Y'                Y=PREVIOUS RECORD WRITTEN                    
MAXLEN   DC    H'1000'           MAX DEMO RECORD SIZE                           
DEMFIL   DC    CL7'DEMFIL'         HELLO FILE NAME                              
CORE     DC    C'CORETAB'          HELLO FILE NAME                              
DEMDIR   DC    CL7'DEMDIR'         DATAMGR DEMO DIRECTORY                       
READHI   DC    CL7'DMRDHI'         DATAMGR READ HIGH                            
READSQ   DC    CL7'DMRSEQ'         DATAMGR READ SEQUENTIAL                      
*                                                                               
* DSECT TO COVER TEMPORARY W/S                                                  
*                                                                               
*                                                                               
TPTWRKD  DSECT                                                                  
AREC     DS    A                                                                
AFRSTEL  DS    A                                                                
RECLEN   DS    H                                                                
*                                                                               
CONDFLAG DS    X                                                                
CONDLEN  DS    XL2                                                              
CONDWRK1 DS    1000C                                                            
CONDWRK2 DS    1000C                                                            
*                                                                               
TPTWRKX  EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE DELMINTD                                                       
         EJECT                                                                  
* DEDEMFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE DEDEMFILE                                                      
         PRINT ON                                                               
* DEDEMCNVD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DEDEMCNVD                                                      
         PRINT ON                                                               
         SPACE 2                                                                
* DEDBLOCK                                                                      
         PRINT OFF                                                              
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
         PRINT ON                                                               
         SPACE 2                                                                
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 2                                                                
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'047DELMTPO   11/01/11'                                      
         END                                                                    
