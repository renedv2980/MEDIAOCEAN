*          DATA SET SPBUY05    AT LEVEL 030 AS OF 08/22/19                      
*PHASE T21105A                                                                  
         TITLE 'T21105 - SPOTPAK BUY PROGRAM - ALLOCATIONS/OTOS'                
T21105   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21105,RR=R8                                                   
         L     RC,0(R1)                                                         
         USING SPBUYWKD,RC                                                      
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
         USING T21105+4096,R9                                                   
         L     RA,VBUYSAVE                                                      
         USING BUYSAVE,RA                                                       
         L     R3,VBUYTWA                                                       
         USING T211FFD,R3                                                       
*                                                                               
         C     R8,RELO                                                          
         BE    HAVERELO                                                         
         L     RF,VCOMFACS                                                      
         L     RF,CPROTOFF-COMFACSD(RF)                                         
         BASR  RE,RF                                                            
         ST    R8,RELO                                                          
         L     RF,VCOMFACS                                                      
         L     RF,CPROTON-COMFACSD(RF)                                          
         BASR  RE,RF                                                            
*                                                                               
HAVERELO MVI   ERRCD,NORECALL                                                   
         OC    SVKEY+14(4),SVKEY+14                                             
         BZ    BUYERR                                                           
         MVC   KEY,SVKEY                                                        
         LA    RE,REC                                                           
         ST    RE,AREC                                                          
         GOTO1 GETREC                                                           
*                                                                               
         TM    SVAFLAG1,X'22'      TEST CTA OR TRADE ACTIVE                     
         BZ    B100                                                             
         GOTO1 GOGETCTA,DMCB,('CIBCPYQ',AREC)                                   
*                                                                               
B100     DS    0H                                                               
         GOTO1 VBLDQLST            GENERATE REQUEST PRD LIST                    
*                                                                               
         MVC   BUSTAT,BDSTAT                                                    
         XC    PRDLIST,PRDLIST     CLEAR ADDED PRD LIST                         
* ON ENTRY R2 HAS A(FIRST ACTV FLDHDR)                                          
         XC    FSTOPS,FSTOPS                                                    
         MVI   FSTOPS,C','                                                      
         LA    R4,8(R2)                                                         
         ST    R4,FADDR                                                         
         XC    FLEN,FLEN                                                        
         GOTO1 FLDVAL                                                           
*                                                                               
         MVI   ERRCD,TRCDERR                                                    
         CLI   FLEN+1,1                                                         
         BNE   BUYERR                                                           
         CLI   0(R4),C'O'          OTO                                          
         BE    B200                                                             
         CLI   SVPRD,X'FF'                                                      
         BNE   BUYERR                                                           
         CLC   =C'A,M=',0(R4)      ACCEPT THESE                                 
         BE    B600                                                             
         CLC   =C'A,FIX',0(R4)                                                  
         BE    B700                                                             
         CLC   =C'A,JS',0(R4)                                                   
         BE    B700                                                             
         CLC   =C'A,AMB',0(R4)                                                  
         BE    B750                                                             
         CLI   0(R4),C'A'          ALLOCATE                                     
         BE    B400                                                             
         CLI   0(R4),C'C'          CHANGE $ OR MASPRD                           
         BE    B600                                                             
         DC    H'0'                                                             
*                                                                               
EQXIT    CR    RB,RB                                                            
         B     *+6                                                              
NEQXIT   LTR   RB,RB                                                            
*                                                                               
EXIT     XIT1  1                                                                
*                                                                               
BUYERR   GOTO1 ERROR                                                            
RELO     DC    A(0)                                                             
*                                                                               
BCX      DS    0H                                                               
         L     RE,ASVDARE          DARE TESTS DONE AT ADDEL/DELEL               
         OI    SVDRFLG2-SVDARED(RE),SVDRFLG2_IGN  SET TO IGNORE DARE            
         GOTO1 SETCHGDT                                                         
*                                                                               
BCX2     TM    SVAFLAG1,X'22'      TEST CTA OR TRADE                            
         BZ    BCX4                                                             
         GOTO1 GOGETCTA,DMCB,('CIBCHGQ',AREC)                                   
*                                                                               
BCX4     LA    RE,PRDLIST          ADDED PRD LIST                               
         CLI   0(RE),0                                                          
         BE    *+8                                                              
         ST    RE,DMCB+20          PASS ADDR IF ANY PRDS IN LIST                
*                                                                               
         MVC   KEY,SVKEY           DISK ADDR MUST BE OK FOR DMDAPTRS            
         GOTO1 PUTREC                                                           
         OI    SVUPDATE,X'40'      SET BUY CHANGE FLAG                          
         MVI   SVMGINIT,0          FORCE MG TABLE REBUILD                       
*                                                                               
         DS    0H                                                               
         GOTO1 VBLDQLST            GENERATE REQUEST PRD LIST                    
*                                                                               
         GOTO1 CALLDSP                                                          
*                                                                               
         C     R2,FLAST            TEST DATA IN INPUT AREA                      
         BNH   *+12                YES                                          
         LA    R2,BUYINP1H                                                      
         MVI   BUYINP2H+5,0        SUPPRESS FURTHER INPUT                       
*                                                                               
         LA    R2,BUYINP1H         POINT TO INPUT LINE 1                        
*                                                                               
BCX6     CLI   8(R2),C'*'          TEST PROCESSED PREVIOUSLY                    
         BE    BCX8                                                             
*                                                                               
         MVC   ELEM(L'BUYINP1),8(R2)                                            
         MVI   8(R2),C'*'                                                       
         MVC   9(L'BUYINP1-1,R2),ELEM                                           
*                                                                               
BCX8     FOUT  (R2)                                                             
*                                                                               
         LLC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         C     R2,FLAST            TEST PAST LAST INPUT FIELD                   
         BH    BCX10                                                            
         C     R2,FADDR            TEST PAST LAST FIELD PROCESSED               
         BNH   BCX6                                                             
*                                                                               
BCX10    CLI   BUYMSG,0                                                         
         BNE   *+10                                                             
         MVC   BUYMSG(22),=C'** ACTION COMPLETED **'                            
         B     EXIT                                                             
         EJECT                                                                  
* OTO'S                                                                         
*                                                                               
B200     DS    0H                                                               
* FOR INFOMERCIAL CLIENT SAVE DATE/TIME/DURATION                                
         TM    SVCOPT1,X'40'                                                    
         BZ    B201                                                             
         LA    R6,BDELEM                                                        
         MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0B'                                                     
         BRAS  RE,NEXTEL                                                        
         BNE   B201                                                             
         TM    6(R6),X'C0'         TEST MINUS/MINUSSED                          
         BNZ   B201                                                             
         MVC   BUXDATE,2(R6)                                                    
         MVC   BUXTIME,BDTIMST                                                  
         MVC   BUXSEC,BDSEC                                                     
*                                                                               
B201     XC    STDTP,STDTP         FORCE DSPLY DATES                            
         MVC   ENDDTP,=X'FFFF'                                                  
*                                                                               
B202     XC    BUELDATA,BUELDATA                                                
         L     R4,FADDR            POINT TO NEXT OTO                            
         AH    R4,FLEN                                                          
         LR    RE,R4                                                            
         AHI   R4,1                                                             
         TM    SVXFRCTL,SVXFR_SDT  ALLOW DESKTOP TO STRING -OTO'S               
         BZ    B202A                                                            
         CLI   0(RE),C','                                                       
         BNE   B202A                                                            
         ST    R4,FADDR                                                         
         XC    FLEN,FLEN                                                        
         GOTO1 FLDVAL                                                           
         XC    FLEN,FLEN                                                        
*                                                                               
B202A    MVI   ERRCD,BADOTO                                                     
         LA    RE,2(R4)            POINT TO PRESUMED DATE                       
         CLC   =C'--',0(R4)        ALLOW --JAN6 AS INPUT                        
         BE    B202B                                                            
         CLC   =C'MAUREEN',BUYBUH  LET MAUREEN DO THIS TOO                      
         BE    *+12                                                             
         TM    WRKRUPSW,WRKRUPSW_ISDRMG     TEST DARE MAKEGOOD                  
         BZ    *+14                                                             
         CLC   =C'-(',0(R4)        ALLOW -(JAN6 FOR DARE MG                     
         BE    B202B                                                            
*                                                                               
         LA    RE,1(R4)            POINT TO PRESUMED DATE                       
         CLI   0(R4),C'+'                                                       
         BE    B202C                                                            
         CLI   0(R4),C'-'                                                       
         BE    B202B                                                            
         TM    SVOPT1,SVOPT1_PAIDOTO TEST PAID OTO CODE USED                    
         BZ    BUYERR                NO                                         
         CLC   SVOTOCHR,0(R4)        TEST FOR SPECIAL PAID -OTO CODE            
         BNE   BUYERR                                                           
*                                                                               
B202B    CLI   SVAPROF+7,C'C'      -OTO CHECK FOR CANADA NETWORK                
         BNE   B202C                                                            
         CLI   BUYMD,C'N'          IF MED=N, MUST BE A LOCAL LEVEL BUY          
         BNE   B202C               SINCE NETWORK LEVEL CODE IN BUY15!           
         LHI   RF,SVB0PROF-BUYSAVE LOCAL LEVEL -OTOS PROFILE CONTROLLED         
         AR    RF,RA                                                            
         CLI   11(RF),C'Y'                                                      
         BE    B202C                                                            
         CLI   11(RF),C'O'                                                      
         BE    B202C                                                            
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=AL2(NOTLCLST)                                            
         B     BUYERR                                                           
B202C    DS    0H                                                               
*                                                                               
         ST    RE,FADDR            SET ADDRESS OF DATE                          
         XC    FLEN,FLEN                                                        
         MVI   EDTVAL,ELDTEDT                                                   
         GOTO1 CALLEDT                                                          
*                                                                               
         OC    STDTP,STDTP         TEST DSPLY DATE SAVED YET                    
         BNZ   *+10                YES                                          
         MVC   STDTP,BUELDT                                                     
         CLC   STDTP,BUELDT        SAVE EARLIEST OTO DATE                       
         BL    *+10                                                             
         MVC   STDTP,BUELDT                                                     
* TEST OTO WITHIN BUY DESC PERIOD                                               
B204W1   CLI   SVAPROF+5,C'1'                                                   
         BE    B204X                                                            
B204W2   MVI   ERRCD,NOTINPER                                                   
         GOTO1 VDATCON,DMCB,(2,BUELDT),(3,DUB)                                  
         CLC   DUB(3),BDSTART                                                   
         BL    B204W4                                                           
         CLC   DUB(3),BDEND                                                     
         BNH   B204X                                                            
                                                                                
* CHANGE BUELDT TO EST END YEAR AND SEE IF IT GETS BETTER                       
                                                                                
B204W4   GOTO1 VDATCON,DMCB,(2,BUELDT),DUB                                      
         LA    R0,1                                                             
         GOTO1 VADDAY,DMCB,(C'Y',DUB),WORK,(R0)                                 
         GOTO1 VDATCON,DMCB,WORK,(2,BUELDT)  CHANGE BUELDT                      
         GOTO1 (RF),(R1),,(3,DUB)            AND DUB                            
         CLC   DUB(3),BDSTART                                                   
         BL    BUYERR                                                           
         CLC   DUB(3),BDEND                                                     
         BH    BUYERR                                                           
*                                                                               
B204X    DS    0H                                                               
         CLI   BUYKEY+3,X'FF'      TEST POL                                     
         BNE   B250                NO                                           
         CLI   0(R4),C'+'          TEST +OTO                                    
         BNE   B220                NO                                           
* POL +OTO FORMAT IS JAN01XXX OR XXX-YYY/40-20                                  
         L     RE,FADDR                                                         
         AH    RE,FLEN             POINT TO PRD                                 
         ST    RE,FADDR                                                         
         XC    FLEN,FLEN                                                        
* FIRST TEST FOR DATA                                                           
         GOTO1 FLDVAL                                                           
         XC    FLEN,FLEN           SET FOR RE-EDIT                              
         LTR   R5,R5                                                            
         BNE   B205A               YES - GO EDIT                                
         TM    BDSTAT,X'01'        TEST NETPAK                                  
         BO    B205B               YES - ALLOW UNALL OTO                        
         MVI   ERRCD,BADOPOL                                                    
         CLI   SVCPROF+0,C'0'      TEST TRUE POL - MUST HAVE DATA               
         BE    BUYERR                                                           
         B     B205B                                                            
B205A    MVI   EDTVAL,ALLOEDT                                                   
         GOTO1 CALLEDT                                                          
*                                                                               
B205B    MVI   ERRCD,BADONUM                                                    
         TM    BDSTAT,X'80'        TEST POL NPW                                 
         BO    *+12                                                             
         CLI   BUELNPW,0           ELSE SHOULD NOT ENTER NPW                    
         BNE   BUYERR                                                           
         CLI   BUELNUM,0            AND NEVER SPOT NUMBER                       
         BNE   BUYERR                                                           
*                                                                               
         CLI   SVCPROF+0,C'0'      TEST BRAND POL                               
         BE    B206                NO                                           
* CHECK FOR FLIGHTS                                                             
         CLI   SVCXTRA+4,C'E'                                                   
         BNE   B205X                                                            
         LA    RE,SVFLTDTS                                                      
B205C    CLC   BUELDT,0(RE)        ELEM TO FLT START                            
         BL    *+14                                                             
         CLC   BUELDT,2(RE)        ELEM TO FLT END                              
         BNH   B205X                IN FLIGHT -OK                               
         LA    RE,4(RE)                                                         
         CLI   0(RE),0             TEST E-O-L                                   
         BNE   B205C                                                            
         MVI   ERRCD,NOTINFLT                                                   
         B     BUYERR                                                           
*                                                                               
B205X    OC    BUELPRD,BUELPRD     TEST BRAND ENTERED                           
         BNE   B206                YES                                          
         MVC   BUELPRD,BDMASPRD    NO - USE MASPRD FROM BUYLINE                 
         MVC   BUELSEC,BDSEC                                                    
         CLI   BDMASPRD+1,0        TEST P/B MASPRD                              
         BE    B206                NO                                           
         MVC   BUELPR2,BDMASPRD+1                                               
         ZIC   R0,BDSEC                                                         
         SRL   R0,1                                                             
         STC   R0,BUELSEC          ASSUME EVEN SPLIT                            
         STC   R0,BUELSEC2                                                      
* NOW FIND AN ALLOCATED REGEL TO TEST SPLIT                                     
         MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0B'                                                     
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         BNE   B206                                                             
         CLI   1(R6),18                                                         
         BNE   *-12                                                             
         MVC   BUELSEC,11(R6)                                                   
         MVC   BUELSEC2,15(R6)                                                  
*                                                                               
B206     MVI   ERRCD,BADOPOL                                                    
         CLI   SVCPROF+0,C'0'      TEST BRAND POL                               
         BE    B208                NO                                           
         CLI   BUELPRD,0           BRAND POL MUST BE ALLOC                      
         BE    BUYERR                                                           
         OC    BDMASPRD,BDMASPRD                                                
         BZ    B208                                                             
         MVI   ERRCD,BADOPRD                                                    
         CLC   BDMASPRD(2),BUELPRD NO WRONG BRAND OTO'S                         
         BNE   BUYERR                                                           
*                                                                               
B208     BAS   RE,SETPRDL          ADD NEW PRD(S) TO LIST                       
* UPDATE REC                                                                    
         XC    ELEM,ELEM                                                        
         MVI   ELEM,X'0C'                                                       
         MVI   ELEM+1,10                                                        
         MVC   ELEM+2(2),BUELDT                                                 
         MVC   ELEM+6(1),BUELPRSW                                               
*                                                                               
         TM    BDSTAT,X'80'        TEST POL NPW                                 
         BZ    B208A                                                            
         MVI   ERRCD,NPWERR                                                     
         SR    R0,R0                                                            
         ICM   R0,1,BUELNPW                                                     
         BZ    BUYERR              +OTO MUST SPECIFY NPW                        
         SLL   R0,2                                                             
         STC   R0,ELEM+7                                                        
*                                                                               
B208A    OC    ELEM+7(3),BUELCOS                                                
         BAS   RE,BLDELEM                                                       
*                                                                               
         GOTO1 TESTGLS,DMCB,ELEM   TEST GOALS PRESENT IF NEEDED                 
*                                                                               
         TM    BDSTAT2,X'80'       TEST DAILY SKED DATA PRESENT                 
         BZ    B208X                                                            
         MVI   ERRCD,DSKEDOTO                                                   
         GOTO1 VDATCON,DMCB,(2,ELEM+2),WORK                                     
         GOTO1 VGETDAY,DMCB,WORK,WORK+6                                         
         ZIC   RE,0(R1)            GET DAY NUMBER                               
         IC    RE,DAYTAB-1(RE)     GET BIT VALUE                                
         EX    RE,*+8                                                           
         B     *+8                                                              
         TM    BDDAY,0 **EXECUTED**                                             
         BZ    BUYERR                                                           
         B     B208X                                                            
*                                                                               
DAYTAB   DC    X'4020100804020100' MON=40,SUN=01                                
*                                                                               
B208X    MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0D'                                                     
         MVC   ELEMDT,ELEM+2       WE ALWAYS KNOW THE DATE                      
         MVI   ELEMNO,0                                                         
         LA    R6,BDELEM                                                        
B210     LR    R5,R6               SAVE PRV EL ADDR                             
         BRAS  RE,NEXTEL                                                        
         BNE   B212                                                             
*                                                                               
         CLC   ELEMDT,2(R6)        TEST SAME DATE                               
         BNE   B210A               ONLY NEED TO COUNT ELEMS THIS DATE           
         TM    6(R6),X'80'         IS IT A MINUS OTO                            
         BNZ   B210A                YES - DON'T ADD IT TO COUNT                 
         IC    RE,ELEMNO                                                        
         AHI   RE,1                                                             
         STC   RE,ELEMNO                                                        
*                                                                               
B210A    CLC   ELEM+2(2),2(R6)     TEST OTO DATE TO ELEM                        
         BH    B210                IF OTO IS HIGH, CONTINUE                     
         BL    B212                IF LOW, INSERT IT                            
* EQUAL - CHECK FOR A MINUS OTO WE CAN DELETE                                   
         CLC   ELEM(6),0(R6)       TEST OTO CD/LEN/DATE/PAY DATE                
         BNE   B210                NO                                           
         TM    6(R6),X'80'         IS IT A MINUS OTO                            
         BZ    B210                NO                                           
* TEST IDENTICAL ELEMENTS                                                       
         MVC   WORK(32),0(R6)      MOVE EL TO WORK AREA                         
         NI    WORK+6,X'7F'        TURN OFF MINUS SPOT IND                      
         IC    RE,WORK+1                                                        
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   WORK(0),ELEM  *EXECUTED*                                         
         BNE   B210                                                             
* MAKE SURE MINUS OTO ISN'T A MAKEGOOD                                          
         TM    6(R5),X'02'         TEST MG IND IN PRV REGEL                     
         BO    B210                YES - DON'T DELETE -OTO                      
* DELETE -OTO AND RESET MINUSSED IND IN REGEL                                   
         NI    6(R5),X'BF'                                                      
B211     DS    0H                                                               
         BAS   RE,DELEL                                                         
         CLI   0(R6),X'10'                                                      
         BL    *+12                                                             
         CLI   0(R6),X'19'                                                      
         BNH   B211                                                             
         B     B214                                                             
         SPACE 2                                                                
B212     DS    0H                                                               
         TM    SVCOPT1,X'40'       NO +OTO'S FOR INFOMERCIALS                   
         BZ    *+12                UNLESS REVERSING -OTO                        
         MVI   ERRCD,INVERR                                                     
         B     BUYERR                                                           
*                                                                               
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=AL2(CLTFRZN)                                             
         TM    SVCOPT2,X'08'       TEST CLIENT FROZEN                           
         BO    BUYERR                                                           
*                                                                               
         MVI   ERRCD,BADOTO                                                     
         OC    SVNDEF(16),SVNDEF   TEST CANAD NTWK AT NTWK LEVEL                
         BNZ   B213                                                             
         CLI   BUYMD,C'N'          NO +OTO'S AT LOCAL LEVEL                     
         BNE   *+12                                                             
         CLI   SVAPROF+7,C'C'      NO +OTO FOR CANAD NTWK                       
         BE    BUYERR                                                           
B213     BAS   RE,ADDEL                                                         
*                                                                               
         IC    RE,ELEMNO                                                        
         AHI   RE,1                                                             
         STC   RE,ELEMNO           INSERTED ELEMNO IS +1                        
*                                                                               
B214     MVC   SVMAKEDT,ELEMDT     SET DATE/SPOTNUM FOR                         
         MVC   SVMAKENO,ELEMNO     MATCHMAKER                                   
*                                                                               
         CLI   FSTOP,C','          TEST MORE OTO'S                              
         BE    B202                YES                                          
*                                                                               
         BAS   RE,CMTFIX                                                        
         B     B230                GO SET CHG FLAG AND WRITE REC                
         EJECT                                                                  
* POOL -OTO                                                                     
*                                                                               
B220     MVI   BYTE,C'N'           SET NOT A GENERIC SEARCH                     
         MVI   ERRCD,BADONUM                                                    
         CLI   BUELPRD,0           MUST NOT ENTER PRD                           
         BNE   BUYERR                                                           
         TM    WRKRUPSW,WRKRUPSW_ISDRMG     TEST DARE MAKEGOOD                  
         BO    *+12                         YES - LET IT GO                     
         CLI   SVPOLPRD,0          TEST BRAND POL BY BRAND                      
         BE    B221                NO                                           
         CLC   =C'--',0(R4)        TEST REVERSE MINUS OTO                       
         BE    B221                                                             
         TM    BDSTAT,X'80'        TEST POL NPW                                 
         BO    B221                                                             
* BRAND POL BY BRAND DELETES ANY SPOT FOR THE DATE FROM THE RIGHT               
         CLI   BUELNUM,0           TEST SPOT NUMBER SPECIFIED                   
         BNE   B221                YES - CAN ONLY DELETE THAT ONE               
*=================================================================*             
* MAKE SURE THE SFP'S ARE IN THE RIGHT SEQUENCE                   *             
*=================================================================*             
         L     RE,AMGWORK                                                       
         LA    RF,SVMGBPR1-MGWORK(RE)                                           
         CLC   0(1,RF),1(RF)                                                    
         BL    B220A                                                            
         IC    R0,0(RF)                                                         
         MVC   0(1,RF),1(RF)                                                    
         STC   R0,1(RF)                                                         
B220A    DS    0H                                                               
         BRAS  RE,CNTEL            COUNT ELEMENTS THIS DATE                     
         CLI   BUELNUM,0           TEST FOUND ANY                               
         BNE   B220B               YUP                                          
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=AL2(NOMISSED)                                            
         B     BUYERR              NO - ERROR                                   
*                                                                               
B220B    TM    WRKRUPSW,WRKRUPSW_ISDRMG     TEST DARE MAKEGOOD                  
         BZ    B221                         AS IF ...                           
*                                                                               
         CLI   SVMGBPR2-MGWORK(RE),0        DO WE KNOW THE PRODUCTS             
         BNE   *+8                          YES                                 
         MVI   BYTE,C'Y'           SET GENERIC SEARCH FLAG                      
*                                                                               
B221     BAS   RE,FNDEL            FIND ELEMENT IN REC                          
*                                                                               
B221X    CLC   =C'--',0(R4)        TEST REVERSE MINUS OTO                       
         BE    B225                                                             
         CLC   =C'-(',0(R4)        SPECIAL FOR DARE TO SET MG PNDG              
         BE    B232                                                             
*                                                                               
         TM    SVOPT1,SVOPT1_PAIDOTO TEST SPECIAL CODES FOR PAID -OTOS          
         BZ    B222B                 NO                                         
         CLI   0(R4),C'-'          TEST INPUT IS A -                            
         BNE   B222A               NO                                           
         TM    WRKRUPSW,WRKRUPSW_ISDRMG     IS THIS A DARE MAKEGOOD             
         BO    B222B               YES - HOPELESS                               
         BAS   RE,TESTBP           TEST SPOT PAID                               
         BZ    B222B               NO                                           
         MVI   ERRCD,PAIDOTO                                                    
         B     B222ERR             YES - NO -OTO                                
*                                                                               
B222A    CLC   SVOTOCHR,0(R4)      TEST INPUT IS SPECIAL -OTO CHAR              
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,TESTBP           TEST SPOT PAID                               
         BNZ   B222B               YES - OK                                     
         MVI   ERRCD,UNPDOTO                                                    
         B     B222ERR             NO - SHOULDN'T USE SPECIAL CODE              
B222B    DS    0H                                                               
         TM    BDSTAT,X'80'        TEST POL NPW                                 
         BO    B226                                                             
         MVI   ERRCD,MISSED                                                     
         TM    6(R6),X'46'         TEST MISSED/MDGD/HIATUS                      
         BNZ   B222ERR                                                          
         TM    WRKRUPSW,WRKRUPSW_ISDRMG     TEST DARE                           
         BO    *+12                                                             
         TM    6(R6),X'10'         TEST MG PENDING                              
         BO    B222ERR                                                          
         CLI   0(R6),X'0C'         IS THE ELEM A +OTO (CANT BE MINUS)           
         BNE   B226                NO                                           
* CHECK FOR A MINUS OTO AGAINST A +OTO THAT CAN BE DELETED                      
* IF SO, JUST DELETE +OTO                                                       
         BAS   RE,TESTBP                                                        
         BNE   B226                                                             
         BAS   RE,TESTMTCH                                                      
         BNE   B226                                                             
         TM    BDSTAT,X'80'        TEST POL NPW                                 
         BZ    B224                                                             
         SR    R0,R0                                                            
         ICM   R0,1,BUELNPW        TEST NPW SPECIFIED                           
         BZ    B224                NO - DELETE ALL SPOTS                        
         ZIC   RE,7(R6)            GET ORIGINAL NPW                             
         SRL   RE,2                                                             
         CR    RE,R0               TEST SAME NPW                                
         BNE   B226                                                             
* DELETE +OTO                                                                   
B224     L     RE,ASVDARE                                                       
         ST    R6,SVDRELEM-SVDARED(RE)  TEST DARE LOCK                          
         GOTO1 ATESTDAR,DMCB,(RC)                                               
*                                                                               
B224A    BAS   RE,DELEL                                                         
         CLI   0(R6),X'10'         TEST AFFID                                   
         BL    *+12                                                             
         CLI   0(R6),X'15'                                                      
         BNH   B224A                                                            
         B     B228                                                             
         SPACE 1                                                                
*----------------------------------------------------------------*              
* THE SPOT DOESN'T QUALIFY - IF IT'S A GENERIC SEARCH, TRY AGAIN *              
*----------------------------------------------------------------*              
         SPACE 1                                                                
B222ERR  DS    0H                                                               
         CLI   BYTE,C'Y'           TEST GENERIC SEARCH                          
         BNE   BUYERR              NO                                           
         IC    RE,BUELNUM                                                       
         BCTR  RE,0                                                             
         STC   RE,BUELNUM                                                       
         CLI   BUELNUM,0                                                        
         BE    BUYERR                                                           
         B     B221                                                             
         EJECT                                                                  
*=========================================================*                     
* NEW LOGIC TO REVERSE -OTO                               *                     
*=========================================================*                     
         SPACE 1                                                                
B225     MVI   ERRCD,NOTMINUS                                                   
         TM    6(R6),X'40'         TEST ALREADY MINUSSED                        
         BZ    BUYERR              CAN'T REVERSE IF NOT MINUS                   
         BAS   RE,TESTBP                                                        
         BNE   BUYERR                                                           
         BAS   RE,TESTMTCH                                                      
         BNE   BUYERR                                                           
*                                                                               
         L     RE,ASVDARE                                                       
         ST    R6,SVDRELEM-SVDARED(RE)  TEST DARE LOCK                          
         GOTO1 ATESTDAR,DMCB,(RC)                                               
*                                                                               
         MVI   ERRCD,MADEGOOD                                                   
         TM    6(R6),X'02'                                                      
         BO    BUYERR                                                           
         NI    6(R6),X'FF'-X'40'   UNSET MINUSSED                               
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),X'0C'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,DELEL                                                         
         B     B228                                                             
         SPACE 1                                                                
*=========================================================*                     
* BUILD AND INSERT -OTO                                   *                     
*=========================================================*                     
         SPACE 1                                                                
B226     TM    BDSTAT,X'80'        TEST POL NPW                                 
         BZ    B226X               NO                                           
* COUNT NUMBER OF SPOTS REMAINING THIS ELEMENT                                  
         LR    R7,R6               SAVE ELEMENT ADDRESS                         
         ZIC   R8,7(R6)                                                         
         SRL   R8,2                                                             
B226A    BRAS  RE,NEXTEL                                                        
         BNE   B226B                                                            
         CLC   2(2,R6),2(R7)       MATCH DATE                                   
         BNE   B226B                                                            
         CLI   0(R6),X'0C'         TEST OTO                                     
         BNE   B226B                                                            
         TM    6(R6),X'80'         TEST MINUS                                   
         BZ    B226B                                                            
         ZIC   RE,7(R6)            GET NUMBER OF SPOTS                          
         SRL   RE,2                                                             
         SR    R8,RE                                                            
         BP    B226A                                                            
         MVI   ERRCD,NEGSPOTS                                                   
         B     BUYERR                                                           
B226B    LR    R6,R7               RESTORE ELEM ADDRESS                         
         SR    R0,R0                                                            
         ICM   R0,1,BUELNPW        TEST NPW ENTERED                             
         BNZ   B226C                                                            
         STC   R8,BUELNPW          IF NOT, DELETE THE REST                      
         OI    6(R6),X'40'         AND INDICATE ALL MINUSED                     
         B     B226X                                                            
*                                                                               
* IF NPW ENTERED, MUST NOT EXCEED REMAINING SPOTS                               
*                                                                               
B226C    MVI   ERRCD,NEGSPOTS                                                   
         ZIC   RE,BUELNPW                                                       
         CR    R8,RE                                                            
         BL    BUYERR                                                           
         BH    *+8                                                              
         OI    6(R6),X'40'         SET ALL MINUSED IND                          
*                                                                               
B226X    DC    0H'0'                                                            
         TM    SVCOPT1,X'40'       TEST INFOMERCIAL CLIENT                      
         BZ    B226X4                                                           
         MVI   ELCDLO,X'73'        TEST COUNTS INPUT                            
         MVI   ELCDHI,X'73'                                                     
         LR    R7,R6               SAVE ELEMENT ADDRESS                         
         BRAS  RE,NEXTEL                                                        
         BNE   B226X2                                                           
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=AL2(HAVECNTS)                                            
         B     BUYERR                                                           
*                                                                               
B226X2   LR    R6,R7               RESTORE ELEMENT POINTER                      
*                                                                               
B226X4   XC    ELEM,ELEM                                                        
         MVC   ELEM(18),0(R6)      PRESERVE ELEM                                
         XC    ELEM+4(2),ELEM+4    CLEAR PAY DATE                               
         NI    ELEM+6,X'BF'        TURN OFF X'40' (MINUS) IN ELEM               
         XC    ELEM+12(2),ELEM+12    AND BILL DATES                             
         XC    ELEM+16(2),ELEM+16                                               
         TM    BDSTAT,X'80'        TEST POL NPW                                 
         BO    *+12                YES - MINUSED IND SET ABOVE                  
         OI    6(R6),X'40'         SET MINUSSED FLAG IN MISSED SPOT             
         NI    6(R6),X'FF'-X'10'   UNSET MG PENDING                             
         OI    ELEM+6,X'80'        SET MINUS SPOT IN OTO                        
         NI    ELEM+6,X'FF'-X'10'  UNSET MG PENDING THAT WAS COPIED             
         MVI   ELEM,X'0C'          SET OTO EL CODE                              
         TM    BDSTAT,X'80'        TEST POL NPW                                 
         BZ    B227                                                             
         ZIC   R0,BUELNPW          GET NUMBER TO -OTO                           
         SLL   R0,2                                                             
         NI    ELEM+7,X'03'        DROP ALL BUT 2 BITS                          
         IC    RE,ELEM+7                                                        
         OR    R0,RE                                                            
         STC   R0,ELEM+7                                                        
         EJECT                                                                  
B227     ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
B227A    CLI   0(R6),X'10'                                                      
         BL    B227X                                                            
         CLI   0(R6),X'19'                                                      
         BH    B227X                                                            
         CLI   0(R6),X'14'         TEST SPLT                                    
         BNE   *+14                                                             
         MVC   ELEM+18(4),0(R6)    SAVE ELEMENT                                 
         B     B227                                                             
         CLI   0(R6),X'11'         TEST INTG                                    
         BNE   B227B                                                            
         ZIC   RE,1(R6)                                                         
         SH    RE,=H'7'                                                         
         EX    RE,*+8              TEST INTG BILLED/PAID                        
         B     *+10                                                             
         OC    6(0,R6),6(R6)  *EXECUTED*                                        
         BZ    B227B               NO - DELETE                                  
         MVC   ELEM+22(6),0(R6)    MOVE ELEM BUT NOT BILL/PAY DATES             
         OI    ELEM+27,X'80'       SET MINUS IND                                
         B     B227                                                             
B227B    BAS   RE,DELEL                                                         
         B     B227A                                                            
B227X    DS    0H                                                               
         L     RE,ASVDARE                                                       
         LA    RF,ELEM                                                          
         ST    RF,SVDRELEM-SVDARED(RE)  TEST DARE LOCK                          
         GOTO1 ATESTDAR,DMCB,(RC)                                               
*                                                                               
         BAS   RE,ADDEL            ADD THE OTO                                  
         BAS   RE,ADDSXT                                                        
*                                                                               
B228     CLI   FSTOP,C','                                                       
         BE    B202                                                             
*                                                                               
B230     MVI   BUWHY,X'10'          SET LAST CHG CODE                           
         MVI   RCLOPT,RCLROT                                                    
         B     BCX                                                              
         EJECT                                                                  
*---------------------------------------------------------------------*         
* DARE CALLS WITH O,-(JAN6KA-KB TO SET MG PENDING FLAG                *         
* IT WAS JUST TOO DIFFICULT TO PUT THE CODE TO FIND THE ELEMENT       *         
* IN THE ALLOCATION CODE WHERE IT MORE LOGICALLY BELONGED             *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
B232     OI    6(R6),X'10'         SET MG PENDING                               
         B     BCX                                                              
         EJECT                                                                  
* NON -POL OTO'S                                                                
*                                                                               
B250     MVI   ERRCD,BADOREG                                                    
         CLI   BUELNUM,0                                                        
         BNE   BUYERR                                                           
         CLI   BUELNPW,0                                                        
         BNE   *+8                                                              
         MVI   BUELNPW,1                                                        
*                                                                               
         CLI   0(R4),C'+'                                                       
         BNE   B270                                                             
* +OTO                                                                          
         XC    ELEM,ELEM                                                        
         MVI   ELEM,X'07'                                                       
         MVI   ELEM+1,10                                                        
         CLI   BDTIME,0            TEST P/B                                     
         BE    *+8                                                              
         MVI   ELEM+1,12                                                        
         MVC   ELEM+2(2),BUELDT                                                 
         MVC   ELEM+7(1),BUELNPW                                                
*                                                                               
         MVI   ELCDLO,X'06'                                                     
         MVI   ELCDHI,X'08'                                                     
         LA    R6,BDELEM                                                        
B252     BRAS  RE,NEXTEL                                                        
         BNE   B254                                                             
         CLC   ELEM+2(2),2(R6)                                                  
         BH    B252                IF OTO IS HIGH, CONTINUE                     
         BL    B254                IF LOW, INSERT IT                            
* EQUAL - CHECK FOR A MINUS OTO WE CAN DELETE                                   
         CLC   ELEM(6),0(R6)       TEST OTO CD/LEN/DATE/PAY DATE                
         BNE   B252A                NO                                          
         TM    6(R6),X'80'         IS IT A MINUS OTO                            
         BZ    B252A                NO                                          
* TEST IDENTICAL ELEMENTS                                                       
         MVC   WORK(32),0(R6)      MOVE EL TO WORK AREA                         
         NI    WORK+6,X'7F'        TURN OFF MINUS SPOT IND                      
         IC    RE,WORK+1                                                        
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   WORK(0),ELEM  **EXECUTED**                                       
         BE    B253                                                             
         EJECT                                                                  
* ELEMENT DATES MATCH - IF NEXT ELEM AN AFFID, INSERT OTO HERE                  
*                                                                               
B252A    ZIC   R7,1(R6)                                                         
         AR    R7,R6                                                            
         CLI   0(R7),X'10'         TEST AFFID                                   
         BNE   B252                NO                                           
         LR    R6,R7               INSERT BEFORE AFFID BUT AFTER SPOT           
         B     B254                YES - INSERT OTO                             
* DELETE -OTO                                                                   
B253     BAS   RE,DELEL                                                         
         B     B256                                                             
*                                                                               
B254     MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=AL2(CLTFRZN)                                             
         TM    SVCOPT2,X'08'       TEST CLIENT FROZEN                           
         BO    BUYERR                                                           
*                                                                               
         BAS   RE,ADDEL                                                         
*        GOTO1 TESTGLS,DMCB,ELEM   TEST GOALS PRESENT IF NEEDED                 
*                                                                               
B256     CLI   FSTOP,C','                                                       
         BE    B202                                                             
         BAS   RE,CMTFIX                                                        
         B     B230                                                             
         EJECT                                                                  
* NON-POL -OTO                                                                  
*                                                                               
B270     MVI   ELCDLO,X'06'                                                     
         MVI   ELCDHI,X'08'                                                     
         LA    R6,BDELEM                                                        
         SR    R7,R7                                                            
         XC    WORK2(8),WORK2      CLEAR ELEM ADDR SAVE AREA                    
B272     BRAS  RE,NEXTEL                                                        
         BNE   B274                                                             
         CLC   BUELDT,2(R6)                                                     
         BH    B272                                                             
         BL    B274                                                             
         ZIC   R0,7(R6)            GET NUM OF SPOTS                             
         TM    6(R6),X'80'                                                      
         BZ    *+6                                                              
         LNR   R0,R0                                                            
         AR    R7,R0                                                            
         OC    WORK2(4),WORK2      SAVED FIRST ELEM ADDR YET                    
         BNZ   *+8                                                              
         ST    R6,WORK2            SAVE ADDR OF FIRST ELEM THIS DATE            
         ST    R6,WORK2+4          SAVE ADDR OF LAST ELEM THIS DATE             
         B     B272                                                             
B274     MVI   ERRCD,NEGSPOTS                                                   
         ZIC   R0,BUELNPW                                                       
         CR    R7,R0                                                            
         BL    BUYERR                                                           
* CHECK FOR MATCHING +OTO IN REC THAT CAN BE DELETED                            
         L     R6,WORK2+4                                                       
         CLI   0(R6),X'07'                                                      
         BNE   B276                                                             
         TM    6(R6),X'80'         IS IT A MINUS OTO                            
         BO    B276                YES - SKIP                                   
         CLC   7(1,R6),BUELNPW                                                  
         BNE   B276                                                             
         BAS   RE,TESTBP                                                        
         BNE   B276                                                             
* DELETE +OTO                                                                   
         BAS   RE,DELEL                                                         
         CLC   WORK2(4),WORK2+4    TEST ONLY ONE ELEM THIS DATE                 
         BNE   B278                NO                                           
         CLI   0(R6),X'10'         NEXT ELEM AN AFFID                           
         BE    B278D               YES - GO DELETE IT                           
         B     B278X               ELSE EXIT                                    
* ADD MINUS OTO                                                                 
*                                                                               
B276     TM    SVOPT1,SVOPT1_PAIDOTO  TEST SPCL CODES FOR PAID -OTOS            
         BZ    B277X                  NO                                        
*                                                                               
         MVI   WORK2+8,0           CLEAR FLAG                                   
         L     R6,WORK2            POINT TO FIRST ELEM                          
B277A    BAS   RE,TESTBP           TEST ELEM PAID                               
         BZ    *+8                 YES                                          
         MVI   WORK2+8,C'P'        SET FLAG FOR PAID SPOTS                      
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    B277B                                                            
         C     R6,WORK2+4          TEST REACHED LAST ELEM                       
         BNH   B277A                                                            
*                                                                               
B277B    MVI   ERRCD,PAIDOTO                                                    
         CLI   0(R4),C'-'          TEST - INPUT                                 
         BNE   B277C                                                            
         CLI   WORK2+8,C'P'        TEST ANY PAID SPOTS                          
         BE    BUYERR              YES - SHOULD USE X CODE                      
         B     B277X                                                            
*                                                                               
B277C    MVI   ERRCD,UNPDOTO                                                    
         CLC   SVOTOCHR,0(R4)      TEST INPUT IS SPECIAL CHAR                   
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   WORK2+8,C'P'        TEST ANY PAID SPOTS                          
         BNE   BUYERR              NO - SHOULDN'T USE SPECIAL CODE              
*                                                                               
B277X    DS    0H                                                               
         L     R6,WORK2+4            RESTORE REG                                
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
*                                                                               
         XC    ELEM,ELEM                                                        
         MVI   ELEM,X'07'                                                       
         MVI   ELEM+1,10                                                        
         CLI   BDTIME,0                                                         
         BE    *+8                                                              
         MVI   ELEM+1,12                                                        
         MVC   ELEM+2(2),BUELDT                                                 
         MVI   ELEM+6,X'80'                                                     
         MVC   ELEM+7(1),BUELNPW                                                
         BAS   RE,ADDEL                                                         
*                                                                               
         BAS   RE,CMTFIX                                                        
         SPACE 2                                                                
* NUMBER OF AFFIDS MUST NOT EXCEED REMAINING SPOTS                              
*                                                                               
B278     LA    R6,BDELEM                                                        
         SR    R7,R7                                                            
         MVI   ELCDLO,6                                                         
         MVI   ELCDHI,8                                                         
B278A    BRAS  RE,NEXTEL                                                        
         BNE   B278B                                                            
         CLC   BUELDT,2(R6)                                                     
         BH    B278A                                                            
         BL    B278B                                                            
         ZIC   R0,7(R6)                                                         
         TM    6(R6),X'80'                                                      
         BZ    *+6                                                              
         LNR   R0,R0                                                            
         AR    R7,R0                                                            
         LR    R8,R6               SAVE ELEM ADDRESS                            
         B     B278A                                                            
*                                                                               
B278B    LR    R6,R8               RESTORE ADDR OF LAST ELEM THIS DATE          
*                                                                               
B278C    ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),X'10'                                                      
         BNE   B278X                                                            
         LTR   R7,R7               TEST ANY MORE SPOTS FOR THIS AFFID           
         BZ    B278D               NO                                           
         BCTR  R7,0                                                             
         B     B278C                                                            
*                                                                               
* DELETE REMAINING AFFIDS                                                       
*                                                                               
B278D    BAS   RE,DELEL                                                         
         CLI   0(R6),X'10'                                                      
         BE    B278D                                                            
*                                                                               
B278X    B     B228                                                             
         EJECT                                                                  
* ALLOCATIONS                                                                   
*                                                                               
B400     XC    STDTP,STDTP                                                      
         MVC   ENDDTP,=X'FFFF'                                                  
*                                                                               
*                                                                               
         CLI   FSTOP,C','          TEST DATA IN INPUT AREA                      
         BNE   B405                NO                                           
* DATA IN INPUT AREA                                                            
B402     TM    SVXFRCTL,SVXFR_SDT+SVXFR_MAK                                     
         BZ    B402W                                                            
         L     RE,FADDR                                                         
         AH    RE,FLEN                                                          
         CLI   0(RE),C','                                                       
         BNE   B402W                                                            
         XC    FLEN,FLEN                                                        
         LA    R4,1(RE)                                                         
         ST    R4,FADDR                                                         
         GOTO1 FLDVAL                                                           
         XC    FLEN,FLEN                                                        
*                                                                               
B402W    MVI   EDTVAL,ELDTEDT                                                   
         GOTO1 CALLEDT                                                          
*                                                                               
         L     R4,FADDR            UPDATE POINTER TO PRD                        
         AH    R4,FLEN                                                          
         ST    R4,FADDR                                                         
         XC    FLEN,FLEN                                                        
         MVI   EDTVAL,ALLOEDT                                                   
         GOTO1 CALLEDT                                                          
         B     B410                                                             
*                                                                               
* DATA IN DSPLY AREA                                                            
*                                                                               
B405     MVI   ERRCD,TRCDERR                                                    
         LA    R0,BUYINP1H         MUST BE ON INPUT LINE 1                      
         CR    R2,R0                                                            
         BNE   BUYERR                                                           
*                                                                               
         CLI   SVRCLOPT,RCLROT     LAST ACTN SHOULD BE RCL ROT                  
         BE    B407                                                             
         CLI   SVRCLOPT,RCLPAY     OR RCL PAY                                   
         BE    B407                                                             
         B     BUYERR                                                           
*                                                                               
B407     LA    R2,BUYOUTH                                                       
*                                                                               
* FIND ALLOCATION AND PRECEDING FIELD                                           
*                                                                               
B408     ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    B425                                                             
         CLI   0(R2),9                                                          
         BNH   B408                                                             
         TM    1(R2),X'20'         TEST                                         
         BZ    *+12                                                             
         ST    R2,WORK2            SAVE PROT FLD ADDR                           
         B     B408                                                             
*                                                                               
         ST    R2,WORK2+4          SAVE UNP FLD ADDR                            
         TM    4(R2),X'20'         TEST CHANGED                                 
         BO    B408                NO-IGNORE                                    
         CLI   5(R2),0             TEST NO INPUT                                
         BE    B408                YES - IGNORE                                 
* EDIT DATE FIELD (PRO)                                                         
         L     R2,WORK2                                                         
         LA    R4,8(R2)                                                         
         CLI   0(R4),C'+'                                                       
         BNE   *+8                                                              
         LA    R4,1(R4)                                                         
         ST    R4,FADDR                                                         
         XC    FLEN,FLEN                                                        
         MVI   EDTVAL,ELDTEDT                                                   
         GOTO1 CALLEDT                                                          
* EDIT PRODUCT FIELD                                                            
         L     R2,WORK2+4                                                       
         LA    R4,8(R2)                                                         
         ST    R4,FADDR                                                         
         XC    FLEN,FLEN                                                        
         MVI   EDTVAL,ALLOEDT                                                   
         GOTO1 CALLEDT                                                          
         MVI   ERRCD,BADCOMMA                                                   
         CLI   FSTOP,C','                                                       
         BE    BUYERR                                                           
*                                                                               
* ALL DATA NOW IN BUELDATA - FIND ELEM AND UPDATE REC                           
*                                                                               
B410     TM    BUELPRSW,X'20'      TEST COST OVERRIDE ENTERED                   
         BZ    B410A                                                            
         CLI   SVAPROF+7,C'C'      TEST CANADA                                  
         BNE   B410A                                                            
         CLI   BUYMD,C'N'          TEST NETWORK                                 
         BNE   B410A                                                            
         MVI   ERRCD,NOCOSTOR                                                   
         B     BUYERR                                                           
*                                                                               
B410A    CLI   SVCPROF+0,C'0'      TEST BRAND POL                               
         BE    B412                NO                                           
* BRAND POL                                                                     
         CLI   BUELPRD,0           TEST PRD ENTERED                             
         BNE   *+12                YES                                          
         TM    BUELPRSW,X'24'      NO - OK IF COST OVRD OR HIAT                 
         BNZ   B412                                                             
         CLC   BDMASPRD(2),BUELPRD  MUST MATCH MASPRD                           
         BE    B412                                                             
         OC    BDMASPRD,BDMASPRD   UNLESS NO MASPRD                             
         BZ    B412                                                             
         CLC   BUELPRD,SVTRDPR     OR MATCH TRD PRD FOR NON-TBS TRADE           
         BE    B412                                                             
         MVI   ERRCD,INVBRPOL                                                   
         B     BUYERR                                                           
*                                                                               
B412     OC    STDTP,STDTP         SAVE EARLIEST ALLOCATION DATE                
         BNZ   *+10                * FOR DISPLAY                                
         MVC   STDTP,BUELDT                                                     
         CLC   STDTP,BUELDT                                                     
         BNH   *+10                                                             
         MVC   STDTP,BUELDT                                                     
*                                                                               
         TM    WRKRUPSW,WRKRUPSW_ISDRMG     TEST DARE MAKEGOOD                  
         BZ    B412B                        AS IF ...                           
         BRAS  RE,CNTEL                     COUNT ELEMENTS THIS DATE            
         CLI   BUELNUM,0                    TEST FOUND ANY                      
         BE    BUYERR                       NO - ERROR                          
*                                                                               
B412B    BAS   RE,FNDEL                                                         
         TM    BUELPRSW,X'F7'      TEST ANY BITS BUT FREE RIDER                 
         BNZ   B412C                                                            
         SR    R0,R0                                                            
         CLI   1(R6),10                                                         
         BNH   *+8                                                              
         ICM   R0,2,10(R6)         GET FIRST PRD                                
         CLI   1(R6),14                                                         
         BNH   *+8                                                              
         ICM   R0,1,14(R6)         GET SECOND PRD, IF ANY                       
         CLM   R0,3,BUELPRD        TEST SAME PRODUCTS                           
         BNE   B412D                                                            
*                                                                               
         CLI   T211FFD+1,C'*'      DDS TERMINAL?                                
         BNE   B412D                                                            
         CLC   =C'TEST',BUYBU      BUYER NAME TEST?                             
         BNE   B412D               NO                                           
         B     B422                ALLOW TURN OFF MAKEGOOD PENDING BIT          
*                                   FOR PAID SPOT SET BY FNDEL3 -HWON           
B412C    TM    BUELPRSW,X'10'      MAKEGOOD PENDING?                            
         BZ    *+12                                                             
         OI    6(R6),X'10'         SET MAKEGOOD PENDING                         
         B     B422                GET OUT                                      
*                                                                               
B412D    BAS   RE,TESTBP                                                        
         BNE   BUYERR                                                           
         BAS   RE,TESTMTCH                                                      
         BNE   BUYERR                                                           
         MVI   ERRCD,MADEGOOD                                                   
         TM    6(R6),X'02'                                                      
         BO    BUYERR                                                           
         MVI   ERRCD,DARMGPND      MAKEGOOD PENDING                             
         TM    6(R6),X'10'                                                      
         BO    BUYERR                                                           
         MVI   ERRCD,MISSED                                                     
         TM    6(R6),X'40'                                                      
         BO    BUYERR                                                           
         TM    BUELPRSW,X'20'      TEST OVRD ENTERED THIS TIME                  
         BZ    B412X               NO                                           
         L     RE,ASVDARE                                                       
         ST    R6,SVDRELEM-SVDARED(RE)  TEST DARE LOCK                          
         GOTO1 ATESTDAR,DMCB,(RC)                                               
         NC    7(3,R6),=X'FC0000'                                               
         TM    BDSTAT,X'80'        TEST POL NPW                                 
         BO    *+10                                                             
         XC    7(3,R6),7(R6)                                                    
         OC    7(3,R6),BUELCOS                                                  
         OI    6(R6),X'20'                                                      
         MVI   ERRCD,NOCOSHIA                                                   
         TM    6(R6),X'04'         TEST HIATUS                                  
         BO    BUYERR                                                           
*                                                                               
         CLI   SVAPROF+7,C'C'      EXTRA CHECK FOR CANADA NETWORK               
         BNE   B412X                                                            
         CLI   BUYMD,C'N'          IF MED=N, MUST BE A LOCAL LEVEL BUY          
         BNE   B412X               SINCE NETWORK LEVEL CODE IN BUY15!           
         LHI   RF,SVB0PROF-BUYSAVE LOCAL LEVEL $OV PROFILE CONTROLLED           
         AR    RF,RA                                                            
         CLI   11(RF),C'Y'                                                      
         BE    B412X                                                            
         CLI   11(RF),C'C'                                                      
         BE    B412X                                                            
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=AL2(NOTLCLST)                                            
         B     BUYERR                                                           
*                                                                               
* TEST COST OVRD = BUY DESC COST                                                
B412X    TM    6(R6),X'20'                                                      
         BZ    B413                                                             
         SR    RE,RE                                                            
         ICM   RE,7,7(R6)                                                       
         SR    RF,RF                                                            
         ICM   RF,7,BDCOST                                                      
         TM    BDSTAT,X'80'        TEST POL NPW                                 
         BZ    *+8                                                              
         N     RE,=X'0003FFFF'     DROP NPW                                     
         CR    RE,RF                                                            
         BNE   B413                                                             
         NI    6(R6),X'DF'         UNSET COST OVRD IND                          
         NC    7(3,R6),=X'FC0000'                                               
         TM    BDSTAT,X'80'                                                     
         BO    *+10                                                             
         XC    7(3,R6),7(R6)        AND CLEAR COST OVRD                         
         BAS   RE,DELSXT                                                        
         MVI   BUXTFLAG,0          AND RESET FLAG IF ON                         
*                                                                               
B413     XC    ELEM,ELEM                                                        
         CLI   BUXTFLAG,0                                                       
         BE    B413A                                                            
         MVI   ELEM+18,X'13'       BUILD SPLIT ELEMENT                          
         MVI   ELEM+19,3                                                        
         MVC   ELEM+20(1),BUXTFLAG                                              
         CLI   BUELPRD,0           TEST NEW PRD ALLOCATED                       
         BNE   B413A               YES                                          
         BAS   RE,DELSXT                                                        
         BAS   RE,ADDSXT           ADD EXTENSION ELEMENT                        
         B     B422                AND EXIT                                     
*                                                                               
B413A    CLI   BUELPRD,0           TEST NEW ALLOC ENTERED                       
         BNE   B413B               YES                                          
         TM    BUELPRSW,X'04'      TEST NEW ALLO = HIAT                         
         BO    B413B                                                            
         TM    BUELPRSW,X'20'      TEST COST OVRD THIS TIME                     
         BZ    B413B               NO - MUST BE UNALL                           
         B     B422                FORGET ABOUT CASE OF M=UNALL                 
* CREATE NEW ELEM                                                               
B413B    BAS   RE,SETPRDL          ADD NEW PRD(S) TO LIST                       
*                                                                               
B413C    MVC   ELEM(18),0(R6)                                                   
         NI    ELEM+6,X'20'        TURN OFF ALL BITS EXC. COST OVRD             
         OC    ELEM+6(1),BUELPRSW  'OR' IN NEW INDS                             
*                                                                               
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         ICM   RF,7,BDCOST                                                      
         ICM   RE,7,BUELCOS                                                     
         TM    BDSTAT,X'80'        TEST POL NPW                                 
         BZ    *+8                                                              
         N     RE,=X'0003FFFF'     DROP NPW                                     
         CR    RE,RF                                                            
         BNE   B413D                                                            
         NI    ELEM+6,X'DF'        UNSET COST OVRD IND                          
         XC    ELEM+7(3),ELEM+7                                                 
         XC    ELEM+18(3),ELEM+18  AND CLEAR EXT ELEM                           
*                                                                               
B413D    MVI   ELEM+1,10                                                        
         CLI   BUELPRD,0                                                        
         BNE   B414                                                             
         MVI   ERRCD,ALLOCOTO                                                   
         OC    BDMGDATE,BDMGDATE   MAY NOT UNALLOCATE MG                        
         BNZ   BUYERR                                                           
         TM    ELEM+6,X'04'        TEST NEW ALLO = HIATUS                       
         BZ    B420                NO                                           
         NI    ELEM+6,X'DF'        UNSET COST OVRD IND                          
         NC    ELEM+7(3),=X'FC0000'                                             
         TM    BDSTAT,X'80'                                                     
         BO    B420                                                             
         XC    ELEM+7(3),ELEM+7    AND CLEAR COST OVRD                          
         XC    ELEM+18(3),ELEM+18  AND CLEAR EXT ELEM                           
         B     B420                                                             
*                                                                               
B414     BAS   RE,BLDELEM                                                       
*                                                                               
         GOTO1 TESTGLS,DMCB,ELEM   TEST GOALS PRESENT IF NEEDED                 
*                                                                               
B420     BAS   RE,FLMTEST                                                       
         BAS   RE,DELEL            DELETE OLD ELEM                              
         BAS   RE,DELSXT                                                        
* ADD NEW ONE                                                                   
         TM    SVCOPT3,COP3SPOD    TEST SPODS ALLOWED                           
         BZ    B420A                                                            
         CLI   BUELPRSW,0          TEST DOING ANYTHING SPECIAL                  
         BNE   *+12                YES - DON'T DELETE IT                        
         CLI   BUELPRD,0           TEST UNALLOCATED                             
         BE    B421A               UNALLOCATED SPOD GETS DELETED !              
         CLI   ELEM+1,14           TEST ONE ALLOCATION                          
         BNE   B420A                                                            
         CLC   BDSEC,ELEM+11       TEST SLN IS BUYLINE SLN                      
         BE    B420A                                                            
         OI    BDSTAT3,BDST3_SPODS SET FLAG FOR SPODS IN BUY                    
*                                                                               
*                                                                               
B420A    BAS   RE,ADDEL                                                         
*                                                                               
         BAS   RE,ADDSXT           ADD EXTENSION ELEM IF NEEDED                 
         TM    6(R6),X'04'         TEST ALLO = HIATUS                           
         BZ    B422                                                             
*                                                                               
B421     ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
*                                                                               
B421A    CLI   0(R6),X'10'                                                      
         BL    B422                                                             
         CLI   0(R6),X'19'                                                      
         BH    B422                                                             
         BAS   RE,DELEL                                                         
         B     B421                                                             
*                                                                               
B422     C     R2,FLAST            ARE WE IN INPUT AREA                         
         BH    B408                NO - LOOK FOR MORE FIELDS                    
         CLI   FSTOP,C','                                                       
         BE    B402                                                             
*                                                                               
B425     MVI   BUWHY,X'20'          SET LAST CHG CODE                           
         MVI   RCLOPT,RCLROT                                                    
         MVC   ENDDTP,=X'FFFF'     SET TO DISPLAY TO E-O-R                      
         B     BCX                                                              
         EJECT                                                                  
*===================================================================            
***  MASTER ALLOCATIONS ***                                                     
*===================================================================            
                                                                                
B600     CLI   2(R4),C'M'          MASPRD                                       
         BE    B610                                                             
         DC    H'0'                                                             
         SPACE 1                                                                
* MASPRD FORMATS ARE M=DC, M=DC,JAN01   M=DC,JAN01-JAN15 *                      
         SPACE 1                                                                
B610     XC    STDTP,STDTP         SET DEFAULT DATES                            
         MVC   ENDDTP,=X'FFFF'                                                  
         LA    R4,4(R4)                                                         
         ST    R4,FADDR                                                         
         XC    FLEN,FLEN                                                        
         XC    BUELDATA,BUELDATA                                                
         MVI   EDTVAL,ALLOEDT                                                   
         GOTO1 CALLEDT                                                          
*                                                                               
         TM    BUELPRSW,X'20'      TEST COST OVERRIDE ENTERED                   
         BZ    B610A                                                            
         CLI   SVAPROF+7,C'C'      TEST CANADA                                  
         BNE   B610A                                                            
         CLI   BUYMD,C'N'          TEST NETWORK                                 
         BNE   B610A                                                            
         MVI   ERRCD,NOCOSTOR                                                   
         B     BUYERR                                                           
*                                                                               
B610A    MVI   ERRCD,BADOPRD                                                    
         CLI   SVCPROF+0,C'0'      TEST BRAND POL                               
         BE    B612                NO                                           
         CLI   SVPOLPRD,0          TEST BRD POL BY BRD                          
         BNE   B610X               YES                                          
         CLI   FSTOP,C','          TEST DATES PRESENT                           
         BNE   B620                NO                                           
         OC    BDMASPRD,BDMASPRD   IF NO MASPRD, FORGET THIS LOGIC              
         BZ    B614                                                             
*                                                                               
* OTHERWISE INPUT MUST BE HIAT OR = BDMASPRD                                    
*                                                                               
B610X    CLI   BUELPRD,0                                                        
         BNE   B611                                                             
         TM    BUELPRSW,X'24'      TEST COST OVRD OR HIAT                       
         BZ    BUYERR                                                           
         B     B612                                                             
*                                                                               
B611     CLC   BDMASPRD,BUELPRD                                                 
         BE    B612                                                             
         CLC   BUELPRD,SVTRDPR     OR MATCH TRD PRD FOR NON-TBS TRADE           
         BNE   BUYERR                                                           
*                                                                               
B612     CLI   FSTOP,C','          COMMA MEANS DATES PRESENT                    
         BE    B614                                                             
         MVI   ERRCD,BADCOMMA                                                   
         CLI   FSTOP,0                                                          
         BNE   BUYERR                                                           
         B     B620                                                             
*                                                                               
B614     MVI   BUWKS,0             RESET IND                                    
         L     R4,FADDR                                                         
         LH    R5,FLEN                                                          
         LA    R4,1(R4,R5)                                                      
         LTR   R5,R5               IF FLEN RESET, NO STOP CHAR                  
         BNZ   *+6                                                              
         BCTR  R4,0                                                             
         GOTO1 VDATVAL,DMCB,(1,(R4)),WORK                                       
         L     R5,0(R1)                                                         
         MVI   ERRCD,PERERR                                                     
         LTR   R5,R5                                                            
         BZ    BUYERR                                                           
         MVC   WORK+6(6),WORK                                                   
         AR    R4,R5                                                            
         CLI   0(R4),C'-'                                                       
         BE    B614A                                                            
         CLI   0(R4),C' '                                                       
         BH    BUYERR                                                           
         B     B615                                                             
*                                                                               
B614A    LA    R4,1(R4)                                                         
         GOTO1 VDATVAL,DMCB,(1,(R4)),WORK+6                                     
         L     R5,0(R1)                                                         
         LTR   R5,R5                                                            
         BZ    B614WK                                                           
*                                                                               
         AR    R4,R5                                                            
         OI    0(R4),C' '                                                       
         CLI   0(R4),C' '                                                       
         BNE   BUYERR                                                           
         B     B615                                                             
*                                                                               
* TEST FOR '-W' INPUT                                                           
*                                                                               
B614WK   ST    R4,FADDR                                                         
         XC    FLEN,FLEN                                                        
         MVI   FSTOPS,C'W'                                                      
         GOTO1 FLDVAL                                                           
         CLI   FSTOP,C'W'                                                       
         BNE   BUYERR                                                           
         TM    FVAL,X'08'          TEST NUMERIC                                 
         BZ    BUYERR                                                           
         LTR   R5,R5                                                            
         BZ    BUYERR                                                           
         CVB   R0,DUB                                                           
         LTR   R0,R0                                                            
         BZ    BUYERR                                                           
         STC   R0,BUWKS            SAVE NUMBER OF WEEKS                         
         LA    R4,1(R4,R5)         POINT PAST STOP CHAR                         
         OI    0(R4),C' '                                                       
         CLI   0(R4),C' '                                                       
         BNE   BUYERR                                                           
         MVC   WORK+6(6),WORK      RESTORE END DATE DESTROYED BY DATVAL         
* WORK OUT ACTUAL DATES                                                         
B615     MVC   WORK(2),SVSTART     MOVE EST START YEAR                          
         CLC   SVSTART(2),SVEND    EST ALL IN ONE YEAR                          
         BE    B617                YES                                          
         CLC   WORK+2(4),SVSTART+2   INPUT MMDD TO ES ST MMDD                   
         BNL   *+10                  IF INPUT HI OR EQ USE START YR             
         MVC   WORK(2),SVEND                                                    
*                                                                               
B617     MVC   WORK+6(2),WORK      MOVE START YY TO END YY                      
         CLC   WORK+2(4),WORK+8    ST MMDD TO END MMDD                          
         BNH   B617A                                                            
         GOTO1 VADDAY,DMCB,(C'Y',WORK+6),WORK+6,F'1'                            
*                                                                               
B617A    MVI   ERRCD,STENDERR                                                   
         CLC   WORK(6),WORK+6                                                   
         BH    BUYERR                                                           
*                                                                               
         CLI   BUWKS,0             TEST WEEKS INPUT                             
         BE    B617X               NO                                           
* CALCULATE END DATE                                                            
         ZIC   R0,BUWKS                                                         
         B     B617BCT                                                          
*                                                                               
B617B    GOTO1 VADDAY,DMCB,WORK+6,WORK+12,7                                     
         MVC   WORK+6(6),WORK+12                                                
B617BCT  BCT   R0,B617B                                                         
*                                                                               
B617X    DS    0H                                                               
         MVI   ERRCD,ESPERERR                                                   
         CLC   WORK(6),SVSTART                                                  
         BL    BUYERR                                                           
         CLC   WORK+6(6),SVEND                                                  
         BH    BUYERR                                                           
*                                                                               
         GOTO1 VDATCON,DMCB,WORK,(2,STDTP)                                      
         GOTO1 (RF),(R1),WORK+6,(2,ENDDTP)                                      
* START AND END DATES MUST BE IN RECORD                                         
         MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0D'                                                     
         LA    R6,BDELEM                                                        
         MVI   ERRCD,INVDATE                                                    
*                                                                               
B618A    BRAS  RE,NEXTEL                                                        
         BNE   BUYERR                                                           
         CLC   2(2,R6),STDTP                                                    
         BNE   B618A                                                            
         B     *+12                START AND END DATES MAY BE EQUAL             
*                                                                               
B618B    BRAS  RE,NEXTEL                                                        
         BNE   BUYERR                                                           
         CLC   2(2,R6),ENDDTP                                                   
         BNE   B618B                                                            
         B     B621                                                             
*                                                                               
* NO DATES ENTERED - CHECK FOR M=HIAT OR M=$ - BOTH ARE INVALID                 
*                                                                               
B620     MVI   ERRCD,NOALLHIA                                                   
         TM    BUELPRSW,X'24'      TEST COST OVRD OR HIAT                       
         BNZ   BUYERR                                                           
         CLI   SVCPROF+0,C'0'      TEST BRD POL                                 
         BE    B621                NO                                           
         MVI   ERRCD,INVBRPOL                                                   
         CLI   BUELPRD,0                                                        
         BE    BUYERR                                                           
*                                                                               
B621     DS    0H                                                               
         BAS   RE,SETPRDL          ADD NEW PRD(S) TO LIST                       
         XC    ELEM,ELEM                                                        
         MVI   ELEM,X'0B'                                                       
         MVI   ELEM+1,10                                                        
         BAS   RE,BLDELEM                                                       
*                                                                               
B622     MVC   ELEM+6(1),BUELPRSW                                               
*                                                                               
         MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0D'                                                     
         LA    R6,BDELEM                                                        
*                                                                               
B623     DS    0H                                                               
         TM    BUELPRSW,X'04'      TEST M=HIAT                                  
         BO    B640                YES                                          
         CLI   BUELPRD,0           TEST NO PRD                                  
         BNE   B624                                                             
         TM    BUELPRSW,X'20'      TEST COST OVRD                               
         BO    B640                                                             
*                                                                               
B624     BRAS  RE,NEXTEL                                                        
         BNE   B635                                                             
         TM    6(R6),X'04'         TEST HIATUS                                  
         BO    B624                YES - SKIP                                   
         BAS   RE,TESTBP           TEST PAID                                    
         BE    B627                NO                                           
B625     CLI   SVCPROF+0,C'0'      PAID - TEST BRAND POL                        
         BE    B624                NO-SKIP ELEM                                 
         CLC   BDMASPRD(2),BUELPRD IF ANY PAID ELEMS CANT CHANGE MASPRD         
         BE    B624                                                             
         B     BUYERR                                                           
B627     MVI   ERRCD,MISSED                                                     
         TM    6(R6),X'C2'         TEST MINUS,MINUSSED,MADE-GOOD                
         BNZ   B625                YES - SKIP IF POL                            
         BAS   RE,TESTMTCH                                                      
         BNE   B625                                                             
* TEST ELEM IN REQUESTED PERIOD BEFORE CHANGING IT                              
         CLC   2(2,R6),STDTP                                                    
         BL    B624                                                             
         CLC   2(2,R6),ENDDTP                                                   
         BH    B624                                                             
*                                                                               
B628     CLC   0(1,R6),ELCDLO      TEST REGEL                                   
         BE    *+12                YES                                          
         CLI   SVCPROF+0,C'0'      TEST BRD POL                                 
         BE    B623                SKIP OTO'S FOR TRUE POL                      
*                                                                               
         TM    BUELPRSW,X'20'      TEST COST OVRD ENTERED                       
         BZ    B628A                                                            
         L     RE,ASVDARE                                                       
         ST    R6,SVDRELEM-SVDARED(RE)  TEST DARE LOCK                          
         GOTO1 ATESTDAR,DMCB,(RC)                                               
         NC    7(3,R6),=X'FC0000'                                               
         TM    BDSTAT,X'80'        TEST POL NPW                                 
         BO    *+10                                                             
         XC    7(3,R6),7(R6)                                                    
         OC    7(3,R6),BUELCOS                                                  
         OI    6(R6),X'20'          AND COST OVRD IND                           
* TEST COST OVRD = BDCOST                                                       
B628A    TM    BUELPRSW,X'04'      TEST M=HIAT                                  
         BO    B628X               YES - CLEAR COST O/R                         
         TM    6(R6),X'20'                                                      
         BZ    B629                                                             
         SR    RE,RE                                                            
         ICM   RE,7,7(R6)                                                       
         SR    RF,RF                                                            
         ICM   RF,7,BDCOST                                                      
         TM    BDSTAT,X'80'        TEST POL NPW                                 
         BZ    *+8                                                              
         N     RE,=X'0003FFFF'     DROP NPW                                     
         CR    RE,RF                                                            
         BNE   B629                                                             
*                                                                               
B628X    NI    6(R6),X'DF'         TURN OFF OVRD IND                            
         NC    7(3,R6),=X'FC0000'                                               
         TM    BDSTAT,X'80'                                                     
         BO    *+10                                                             
         XC    7(3,R6),7(R6)       AND RESET COST                               
*                                                                               
B629     MVC   ELEM(1),0(R6)       SET ELEM CODE                                
         MVC   ELEM+2(8),2(R6)     MOVE DATE/INDS/COST                          
         CLI   BUELPRD,0           TEST NO ALLOC ENTERED                        
         BNE   *+12                                                             
         TM    BUELPRSW,X'20'      TEST COST OVRD THIS TIME                     
         BO    B623                YES - IGNORE UNALL REQUEST                   
         NI    ELEM+6,X'30'        DROP ALL BUT $OVRD AND MG PNDG               
         OC    ELEM+6(1),BUELPRSW                                               
*                                                                               
         CLI   ELEM+1,10           IF NOT ALLOCATED                             
         BE    B630                NO GOALS CHECK                               
         GOTO1 TESTGLS,DMCB,ELEM   TEST GOALS PRESENT IF NEEDED                 
*                                                                               
B630     BAS   RE,FLMTEST          TEST TO DELETE FILM NUMBERS                  
*                                                                               
B631     DS    0H                                                               
         BAS   RE,DELEL                                                         
         BAS   RE,DELSXT                                                        
*                                                                               
         BAS   RE,ADDEL                                                         
         BAS   RE,ADDSXT                                                        
         TM    6(R6),X'04'         TEST ALLO = HIATUS                           
         BZ    B623                                                             
         LR    R7,R6                                                            
         ZIC   R0,1(R7)                                                         
         AR    R7,R0                                                            
B632     CLI   0(R7),X'10'                                                      
         BL    B632X                                                            
         CLI   0(R7),X'19'                                                      
         BH    B632X                                                            
         GOTO1 VRECUP,DMCB,BUYREC,(R7)                                          
         B     B632                                                             
*                                                                               
B632X    B     B623                                                             
*                                                                               
B635     CLI   SVCPROF+0,C'0'      TEST BRD POL                                 
         BNE   *+14                YES                                          
         CLC   STDTP(4),=X'0000FFFF'  TRUE POL - TEST DATES ENTERED             
         BNE   B637                                                             
         CLI   BUELPRD,0           TEST NEW PRD                                 
         BE    B637                NO (HIAT)                                    
         CLC   BUELPRD,SVTRDPR     OR MATCH TRD PRD FOR NON-TBS TRADE           
         BE    *+10                NEED CASH PRD AS MASPRD !                    
         MVC   BDMASPRD(2),BUELPRD                                              
B637     B     B425                                                             
         EJECT                                                                  
* INPUT IS M=HIAT OR M=COST OVRD                                                
* ALLOW IT UNLESS AFFECTED SPOTS ARE BILLED/PAID/MATCHED                        
*                                                                               
B640     BRAS  RE,NEXTEL                                                        
         BNE   B425                                                             
         TM    6(R6),X'04'         TEST HIATUS                                  
         BO    B640                YES - SKIP                                   
         CLC   2(2,R6),STDTP                                                    
         BL    B640                                                             
         CLC   2(2,R6),ENDDTP                                                   
         BH    B640                                                             
         BAS   RE,TESTBP                                                        
         BNZ   B625                                                             
         BAS   RE,TESTMTCH                                                      
         BNE   B625                                                             
         MVI   ERRCD,MISSED                                                     
         TM    6(R6),X'C2'         TEST MINUS/MINUSSED/MADE-GOOD                
         BNZ   B625                                                             
*                                                                               
         B     B628                                                             
         EJECT                                                                  
*========================================================*                      
* SPECIAL CODE FOR JACQUE SPIEGEL TO FIX MISSING PASSIVE *                      
* POINTERS ON SPTDIR                                     *                      
* AND CHECK FOR DUPLICATE -OTO ELEMENTS                  *                      
* AND ADD MISSING SPCL TRAFFIC ELEMENT IF MISSING        *                      
*========================================================*                      
         SPACE 1                                                                
B700     LA    R6,BDELEM                                                        
         MVI   ELCDLO,X'0C'                                                     
         MVI   ELCDHI,X'0C'                                                     
*                                                                               
B702     BRAS  RE,NEXTEL                                                        
         BNE   B710                                                             
*                                                                               
B704     TM    6(R6),X'80'         TEST MINUS                                   
         BZ    B702                                                             
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0               POINT TO NEXT ELEMENT                        
         CLI   0(R6),X'0C'         IS IT AN OTO                                 
         BNE   B702                                                             
         TM    6(R6),X'80'         IS IT -                                      
         BZ    B702                                                             
*                                                                               
B706     GOTO1 VRECUP,DMCB,BUYREC,(R6)    DELETE THE ELEMENT                    
         CLI   0(R6),X'10'                AND OTHER ELEMS X'10'-X'1F'           
         BL    B708                                                             
         CLI   0(R6),X'1F'                                                      
         BL    B706                                                             
*                                                                               
B708     BRAS  RE,NEXTEL2                                                       
         BE    B704                                                             
*                                                                               
B710     CLI   SVMCLUNQ,0          TEST SPECIAL TRAFFIC CLIENT                  
         BE    B720                NO                                           
* SEARCH FOR EXISTING ELEMENT                                                   
         MVI   ELCDLO,X'61'                                                     
         MVI   ELCDHI,X'61'                                                     
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         BE    B720                                                             
* NONE THERE - BUILD A NEW ONE                                                  
         MVI   ELEM,X'61'                                                       
         MVI   ELEM+1,6                                                         
         MVC   ELEM+2(2),SVMCLCOD                                               
         MVC   ELEM+4(1),SVMCLUNQ                                               
         MVC   ELEM+5(1),SVMCLPRD                                               
         GOTO1 VRECUP,DMCB,BUYREC,ELEM,(R6)                                     
*                                                                               
B720     DS    0H                                                               
         LA    R6,BDELEM                                                        
         MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0D'                                                     
*                                                                               
B722     BRAS  RE,NEXTEL                                                        
         BNE   B730                                                             
*                                                                               
         LA    R4,10(R6)           POINT TO FIRST ALLOCATION                    
         ZIC   R5,1(R6)                                                         
         SH    R5,=H'10'                                                        
         BNP   B722                                                             
         SRL   R5,2                SET FOR BCT                                  
*                                                                               
B724     BAS   RE,B700SET                                                       
         LA    R4,4(R4)                                                         
         BCT   R5,B724                                                          
         B     B722                                                             
*                                                                               
B730     DS    0H                  DO PUTREC TO GET MISSING POINTERS            
         B     BCX2                                                             
*                                                                               
B700SET  LA    R1,PRDLIST          TEST IF PRD IN LIST ALREADY                  
*                                                                               
B700SET2 CLI   0(R1),0                                                          
         BE    B700SET4                                                         
         CLC   0(1,R1),0(R4)                                                    
         BER   RE                                                               
         LA    R1,1(R1)                                                         
         B     B700SET2                                                         
*                                                                               
B700SET4 MVC   0(1,R1),0(R4)       ADD PRD TO LIST                              
         BR    RE                                                               
*==========================================================*                    
* FIX BUY RECORDS WITH DATE SEQUENCE PROBLEMS              *                    
* INPUT IS A,AMB=DSKADDR                                   *                    
*==========================================================*                    
         SPACE 1                                                                
B750     DS    0H                                                               
         L     RF,VCOMFACS                                                      
         L     RF,CHEXIN-COMFACSD(RF)                                           
         GOTO1 (RF),DMCB,6(R4),DUB,8                                            
         OC    12(4,R1),12(R1)                                                  
         BNZ   B751                                                             
         MVC   BUYMSG(13),=C'HEX NOT VALID'                                     
         B     EXIT                                                             
*                                                                               
B751     XC    KEY,KEY             SO WE'LL KNOW WE DID IT                      
         MVC   KEY+14(4),DUB                                                    
         GOTO1 GETREC                                                           
*                                                                               
         LA    R6,BDELEM                                                        
         MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0C'                                                     
         XC    HALF,HALF                                                        
*                                                                               
B752     BRAS  RE,NEXTEL                                                        
         BNE   B756                                                             
         CLC   HALF,2(R6)          DATES SHOULD NOT DESCEND                     
         BH    B754                                                             
         MVC   HALF,2(R6)          SAVE CURRENT DATE                            
         B     B752                                                             
* DELETE ALL REMAINING 0B/0C ELEMENTS                                           
B754     GOTO1 VRECUP,DMCB,BUYREC,(R6)                                          
         BRAS  RE,NEXTEL2                                                       
         BE    B754                                                             
         GOTO1 PUTREC                                                           
         MVC   BUYMSG(22),=C'** ACTION COMPLETED **'                            
         B     EXIT                                                             
*                                                                               
B756     MVC   BUYMSG(22),=C'** NOTHING TO FIX   **'                            
         B     EXIT                                                             
         EJECT                                                                  
SETPRDL  NTR1                                                                   
         LA    R4,BUELPRD                                                       
         BAS   RE,TSTPRD                                                        
         LA    R4,BUELPR2                                                       
         BAS   RE,TSTPRD                                                        
         B     EXIT                                                             
*                                                                               
TSTPRD   NTR1                                                                   
         CLI   0(R4),0                                                          
         BE    EXIT                                                             
         MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0D'                                                     
         LA    R6,BDELEM                                                        
TSTPRD2  BRAS  RE,NEXTEL                                                        
         BNE   TSTPRD4                                                          
         ZIC   R0,1(R6)                                                         
         SH    R0,=H'10'                                                        
         BNP   TSTPRD2                                                          
         SRL   R0,2                                                             
         LA    R7,10(R6)                                                        
         CLC   0(1,R7),0(R4)       SAME PRD                                     
         BE    EXIT                                                             
         LA    R7,4(R7)                                                         
         BCT   R0,*-14                                                          
* PRD NOT IN ANY ELEMENT - SEE IF IN LIST YET                                   
TSTPRD4  LA    R7,PRDLIST                                                       
*                                                                               
TSTPRD6  CLI   0(R7),0                                                          
         BE    TSTPRD8                                                          
         CLC   0(1,R7),0(R4)                                                    
         BE    EXIT                                                             
         LA    R7,1(R7)                                                         
         B     TSTPRD6                                                          
* ADD PRD TO LIST                                                               
TSTPRD8  MVC   0(1,R7),0(R4)                                                    
         B     EXIT                                                             
         EJECT                                                                  
FNDEL    LR    R8,RE               SAVE CALLING REG                             
         MVI   ERRCD,BADSPOT                                                    
         XC    ELEMDT,ELEMDT                                                    
         MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0D'                                                     
         LA    R6,BDELEM                                                        
         CLI   BUELNUM,0                                                        
         BNE   *+8                                                              
         MVI   BUELNUM,1                                                        
FNDEL2   BRAS  RE,NEXTEL                                                        
         BNE   BUYERR                                                           
         CLC   ELEMDT,2(R6)                                                     
         BE    *+8                                                              
         MVI   ELEMNO,0                                                         
         MVC   ELEMDT,2(R6)                                                     
         IC    RE,ELEMNO                                                        
*                                                                               
* IF DDS TERMINAL & BYRNM IS TEST, ALLOW PRD ALLOC EVEN IF MKGD PNDING          
* ALSO ALLOW IT IF WE ARE COMING FROM DARE MAKEGOOD                             
*&&DO                                                                           
         TM    WRKRUPSW,WRKRUPSW_ISDRMG     TEST DARE MAKEGOOD                  
         BNZ   FNDEL2A                      YES - LET IT GO                     
         CLI   T211FFD+1,C'*'      DDS TERMINAL?                                
         BNE   FNDEL3                                                           
         CLC   =C'TEST',BUYBU      BUYER NAME TEST?                             
         BNE   FNDEL3                                                           
*&&                                                                             
FNDEL2A  TM    6(R6),X'80'         TEST MINUS SPOT                              
         BO    *+8                                                              
         LA    RE,1(RE)                                                         
         STC   RE,ELEMNO                                                        
*                                                                               
FNDEL3   CLC   ELEMDT,BUELDT                                                    
         BNE   FNDEL2                                                           
         CLC   ELEMNO,BUELNUM                                                   
         BNE   FNDEL2                                                           
*                                                                               
* IF DDS TERMINAL AND BUYERNAME IS TEST, TURN OFF MAKEGOOD PENDING BIT          
*                                                                               
         CLI   T211FFD+1,C'*'      DDS TERMINAL?                                
         BNE   FNDELX                                                           
         CLC   =C'TEST',BUYBU      BUYER NAME TEST?                             
         BNE   FNDELX                                                           
         NI    6(R6),X'EF'         TURN OFF X'10'/MAKEGOOD PENDING BIT          
*                                                                               
FNDELX   LR    RE,R8                                                            
         BR    RE                                                               
         SPACE 2                                                                
DELEL    NTR1                                                                   
         CLI   0(R6),X'06'                                                      
         BL    DELEL2                                                           
         CLI   0(R6),X'0D'                                                      
         BH    DELEL2                                                           
         L     RE,ASVDARE                                                       
         ST    R6,SVDRELEM-SVDARED(RE)  TEST DARE LOCK                          
         GOTO1 ATESTDAR,DMCB,(RC)                                               
DELEL2   DS    0H                                                               
         GOTO1 VRECUP,DMCB,BUYREC,(R6)                                          
         B     EXIT                                                             
*                                                                               
ADDEL    NTR1                                                                   
         CLI   ELEM,X'06'                                                       
         BL    ADDEL2                                                           
         CLI   ELEM,X'0D'                                                       
         BH    ADDEL2                                                           
         L     RE,ASVDARE                                                       
         LA    R0,ELEM                                                          
         ST    R0,SVDRELEM-SVDARED(RE)  TEST DARE LOCK ON THIS DATE             
         GOTO1 ATESTDAR,DMCB,(RC)                                               
ADDEL2   DS    0H                                                               
         GOTO1 VRECUP,DMCB,BUYREC,ELEM,(R6)                                     
         B     EXIT                                                             
         EJECT                                                                  
****************************************************************                
*     WARNING TO MEL -- THIS CODE IS DISINCTLY DIFFERENT       *                
*              FROM ROUGHLY SIMILAR CODE IN BASE.              *                
*       DO NOT REPLACE THIS WITH CALL TO VBLDEL HOWEVER        *                
*              TEMPTING IT MAY APPEAR                          *                
****************************************************************                
         SPACE 1                                                                
BLDELEM  NTR1                                                                   
*                                                                               
         CLI   BUELPRD,0                                                        
         BE    EXIT                                                             
         MVI   ELEM+1,14                                                        
         MVC   ELEM+10(1),BUELPRD                                               
         MVC   ELEM+11(1),BUELSEC                                               
         XC    ELEM+12(2),ELEM+12  CLEAR BILL DATE                              
         CLI   BUELPR2,0                                                        
         BE    EXIT                                                             
         MVI   ELEM+1,18                                                        
         MVC   ELEM+14(1),BUELPR2                                               
         MVC   ELEM+15(1),BUELSEC2                                              
         XC    ELEM+16(2),ELEM+16  CLEAR BILL DATE                              
*                                                                               
         B     EXIT                                                             
         SPACE 2                                                                
DELSXT   NTR1                                                                   
         SR    R0,R0                                                            
         B     DELSXT4                                                          
*                                                                               
DELSXT2  IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
*                                                                               
DELSXT4  CLI   0(R6),0                                                          
         BE    EXIT                                                             
         CLI   0(R6),X'13'                                                      
         BL    DELSXT2                                                          
         BH    EXIT                                                             
         BAS   RE,DELEL                                                         
         B     EXIT                                                             
         SPACE 2                                                                
ADDSXT   NTR1                                                                   
         CLI   BUXTFLAG,0                                                       
         BE    EXIT                                                             
         SR    R0,R0                                                            
ADDSXT2  IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),X'10'                                                      
         BL    *+12                                                             
         CLI   0(R6),X'13'                                                      
         BNH   ADDSXT2                                                          
         GOTO1 VRECUP,DMCB,BUYREC,ELEM+18,(R6)                                  
         B     EXIT                                                             
         SPACE 2                                                                
* TEST SPOT AT 0(R6) IS PAID                                                    
TESTBP   MVI   ERRCD,BLLDPAID                                                   
         OC    4(2,R6),4(R6)                                                    
         BNZR  RE                  EXIT WITH CC NEQ IF PAID                     
         BR    RE                  ELSE EXIT WITH CC EQ                         
         EJECT                                                                  
* TEST FOR AFFIDS FOR ELEM AT 0(R6).                                            
* ASSUME ELCDLO AND ELCDHI CONTAIN REGEL ARGUMENTS                              
*                                                                               
TESTMTCH NTR1                                                                   
         MVI   ERRCD,MATCHED                                                    
         CLC   =C'MC',AGYALPHA    TEST MCCANN                                   
         BNE   TMTCH0                                                           
         CLC   BDEND,=X'660401'                                                 
         BL    EQXIT                                                            
*                                                                               
TMTCH0   LR    R5,R6               SAVE EL ADDRESS                              
*                                                                               
TMTCH2   BRAS  RE,NEXTEL                                                        
         BNE   TMTCH4                                                           
         CLI   BUYKEY+3,X'FF'                                                   
         BE    TMTCH4                                                           
         CLC   2(2,R5),2(R6)       TEST SAME DATE                               
         BE    TMTCH2                                                           
TMTCH4   LR    R7,R6               R7 IS END OF SRCH                            
         BCTR  R7,0                                                             
TMTCH6   CLI   0(R5),X'10'         TEST AFFID                                   
         BNE   TMTCH8              EXIT WITH CC NOT EQ IF MATCHED               
* IGNORE MATCHED STATUS IF RSVP ELEM FOLLOWS                                    
         ZIC   R6,1(R5)                                                         
         AR    R5,R6                                                            
         CLI   0(R5),X'17'                                                      
         BE    EQXIT                                                            
         B     NEQXIT                                                           
TMTCH8   ZIC   R6,1(R5)                                                         
         BXLE  R5,R6,TMTCH6                                                     
         B     EQXIT                                                            
         SPACE 2                                                                
* MAKE SURE COMMENTS LAST ELEMS IN RECORD                                       
*                                                                               
CMTFIX   NTR1                                                                   
*                                                                               
         MVC   DUB(4),AREC1        FROM                                         
         MVC   DUB+4(4),AREC2      TO                                           
         GOTO1 MOVEREC                                                          
*                                                                               
         MVI   ELCDLO,X'62'                                                     
         MVI   ELCDHI,X'FF'                                                     
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         BNE   EXIT                NO COMMENTS                                  
*                                                                               
CMTF2    BAS   RE,DELEL                                                         
         BRAS  RE,NEXTEL2                                                       
         BE    CMTF2                                                            
* R6 POINTS TO E-O-R                                                            
         LR    R7,R6               SAVE E-O-R ADDRESS                           
         L     RE,AREC2                                                         
         LA    R6,BDELEM-BUYREC(RE)                                             
*                                                                               
CMTF4    BRAS  RE,NEXTEL                                                        
         BNE   EXIT                                                             
         GOTO1 VRECUP,DMCB,BUYREC,(R6),(R7)                                     
         ZIC   R0,1(R7)            ADD NEXT ELEM AFTER THIS ONE                 
         AR    R7,R0                                                            
         B     CMTF4                                                            
*                                                                               
         EJECT                                                                  
* COMPARE PRD ALLOCATIONS BETWEEN NEW (ELEM) AND OLD (R6) ELEMENTS              
* IF CHANGED, DELETE FILM ELEM IF PRESENT                                       
*                                                                               
FLMTEST  NTR1                                                                   
         LR    R7,R6                                                            
FLMT2    ZIC   R0,1(R7)                                                         
         AR    R7,R0                                                            
         CLI   0(R7),X'11'                                                      
         BE    FLMT2                                                            
         CLI   0(R7),X'12'                                                      
         BNE   EXIT                                                             
*                                                                               
         CLI   ELEM+1,10           TEST NEW ELEM UNALL                          
         BE    FLMTX               YES-DELETE FILM                              
         CLC   ELEM+1(1),1(R6)     ELEMS SAME LEN                               
         BNE   FLMTX                                                            
         CLC   ELEM+10(2),10(R6)   SAME PRD1/SLN1                               
         BNE   FLMTX                                                            
         CLI   ELEM+1,14                                                        
         BNH   EXIT                                                             
         CLC   ELEM+14(2),14(R6)   SAME PRD2/SLN2                               
         BE    EXIT                                                             
FLMTX    DS    0H                                                               
         GOTO1 VRECUP,DMCB,BUYREC,(R7)                                          
         B     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================*         
* COUNT NUMBER OF SPOTS FOR THE MINUS DATE                            *         
*=====================================================================*         
         SPACE 1                                                                
CNTEL    NTR1  BASE=*,LABEL=*                                                   
         LA    R6,BDELEM                                                        
         MVI   BUELNUM,0                                                        
         SR    R7,R7               CLEAR ELEMENT COUNTER                        
         MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0D'                                                     
*                                                                               
CNTEL2   BRAS  RE,NEXTEL                                                        
         BNE   CNTELX                                                           
         CLC   2(2,R6),BUELDT      ELEMENT TO OTO DATE                          
         BL    CNTEL2                                                           
         BH    CNTELX                                                           
*                                                                               
         TM    6(R6),X'80'         IF MINUS                                     
         BO    CNTEL2              SKIP IT                                      
*                                                                               
CNTEL3   LA    R7,1(R7)            BUMP SPOT COUNTER                            
*                                                                               
         TM    6(R6),X'40'         TEST MINUSSED                                
         BO    CNTEL2              YES-SKIP                                     
*                                                                               
         TM    DRMGFLG,OTOOK       OTO OK EVEN IF MKGD PENDING?                 
         BO    *+12                                                             
         TM    6(R6),X'10'         IF MAKEGOOD PENDING AND NOT OTOOK            
         BO    CNTEL2              SKIP IT                                      
*                                                                               
CNTEL4   TM    WRKRUPSW,WRKRUPSW_ISDRMG     TEST DARE MAKEGOOD                  
         BZ    CNTEL10                      AS IF ...                           
         L     RE,AMGWORK                                                       
         CLI   SVMGBPR2-MGWORK(RE),0        DO WE KNOW THE PRODUCTS             
         BE    CNTEL6                       NO - USE THIS SPOT                  
* DARE MAKEGOOD WITH PRDS SPECIFIED - NEED TO MATCH PRDS                        
* THEN GET PRD CODES IN HI/LO SEQUENCE                                          
         CLI   1(R6),10            TEST SPOT ALLOCATED                          
         BNH   CNTEL2              NO - DON'T USE IT                            
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         IC    R0,10(R6)           BPRD1                                        
         CLI   1(R6),14                                                         
         BNH   *+8                                                              
         IC    R1,14(R6)           BPRD2                                        
         STC   R0,HALF                                                          
         STC   R1,HALF+1                                                        
         CR    R0,R1                                                            
         BL    *+12                                                             
         STC   R1,HALF                                                          
         STC   R0,HALF+1                                                        
         L     RE,AMGWORK                                                       
         CLC   HALF,SVMGBPR1-MGWORK(RE)   TEST SAME PRDS (HIGH FIRST)           
         BNE   CNTEL2              ...AS IF....                                 
*                                                                               
CNTEL6   L     RE,AMGWORK                                                       
         TM    6(R6),X'20'                TEST COST OVRD ON SPOT                
         BO    CNTEL8                     YES                                   
         TM    SVMGFLAG-MGWORK(RE),X'80'  TEST MG HAS OVERRIDE                  
         BZ    CNTEL10                    NO                                    
         CLC   BDCOST,SVMGCOST-MGWORK(RE) TEST COST MATCHES BUYLINE             
         BE    CNTEL10                                                          
         B     CNTEL2                                                           
* SPOT HAS OVERRIDE - MAKEGOOD SHOULD HAVE ONE TOO                              
CNTEL8   TM    SVMGFLAG-MGWORK(RE),X'80'   TEST MG HAS OVERRIDE                 
         BZ    CNTEL2                      NO - IGNORE                          
         CLC   7(3,R6),SVMGCOST-MGWORK(RE) TEST COSTS MATCH                     
         BNE   CNTEL2                      NO - CONTINUE                        
*                                                                               
CNTEL10  STC   R7,BUELNUM          SET HIGHEST AVAIL SPOT NUM                   
         B     CNTEL2                                                           
CNTELX   XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
NEXTEL   CLI   0(R6),0                                                          
         JE    NEXTELX                                                          
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         LTR   R0,R0                                                            
         JNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
NEXTEL2  CLI   0(R6),0                                                          
         JE    NEXTELX                                                          
         CLC   ELCDLO,0(R6)                                                     
         JH    NEXTEL                                                           
         CLC   ELCDHI,0(R6)                                                     
         JL    NEXTEL                                                           
         CR    RB,RB                                                            
         J     *+6                                                              
NEXTELX  LTR   RB,RB                                                            
         BR    RE                                                               
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE SPBUYWORK                                                      
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
 END                                                                            
