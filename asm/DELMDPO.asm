*          DATA SET DELMDPO    AT LEVEL 003 AS OF 03/03/16                      
*PROCESS USING(WARN(15))                                                        
*PHASE DELMDPOA                                                                 
         TITLE 'DEMCON - DPT CONVERSION - OUTPUT PHASE'                         
DECNVDPT CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 260,**DPTCNV                                                     
         USING DPTWRKD,RC          RC=A(W/S)                                    
         USING DEMCOND,R8          R8=A(GLOBAL W/S)                             
         L     R9,ASREC                                                         
         USING INTERD,R9           R9=A(SORT RECORD)                            
         L     RA,ACOMFACS                                                      
         USING COMFACSD,RA         RA=A(COMMON FACILITIES)                      
         B     *+4(R1)                                                          
         B     CNV2                PROCESS A RECORD                             
         B     CNV4                LAST TIME HOOK                               
*                                                                               
CNV2     CLI   INTRTYP,1           SATELLITE STATION RECORD                     
         BE    CNV6                                                             
         CLI   INTRTYP,BSCODEQU    STATION/MARKET RECORD                        
         BE    CNV10                                                            
         CLI   INTRTYP,DRCODEQU    RATINGS RECORD                               
         BE    CNV14                                                            
*&&DO                                                                           
         CLI   INTRTYP,UCODEQU     UNIVERSE RECORD                              
         BE    CNV12                                                            
*&&                                                                             
         B     CNVX                                                             
*                                                                               
CNV4     XC    SATLASTP,SATLASTP   CLEAR SATELLITE STATION TABLE                
         MVI   INTRTYP,X'FF'                                                    
         B     CNV16               GO WRITE LAST RECORD                         
*                                                                               
CNVX     XMOD1 1                                                                
         EJECT                                                                  
* BUILD SATELLITE STATION TABLE                                                 
*                                                                               
CNV6     LM    RE,RF,SATLASTP                                                   
         LTR   RE,RE               TEST IF FIRST TIME                           
         BNZ   *+16                                                             
         LA    RE,SATTAB           YES - SET TO START OF TABLE                  
         XC    0(L'SATTAB+1,RE),0(RE)                                           
         LR    RF,RE                                                            
         CLC   INTSTA,1(RE)        TEST IF CHANGE OF PARENT                     
         BE    CNV8                                                             
         MVI   0(RF),C'P'          BUILD PARENT ENTRY                           
         MVC   1(L'INTSTA,RF),INTSTA                                            
         LR    RE,RF                                                            
         LA    RF,L'INTSTA+1(RF)                                                
*                                                                               
CNV8     MVI   0(RF),C'S'          BUILD SATELLITE ENTRY                        
         MVC   1(L'INTSAT,RF),INTSAT                                            
         LA    RF,L'INTSAT+1(RF)                                                
         MVI   0(RF),0             SET END OF TABLE                             
         STM   RE,RF,SATLASTP                                                   
         B     CNVX                                                             
         EJECT                                                                  
* BUILD STATION/MARKET, MARKET/STATION & STATION/BOOK RECORDS                   
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
         MVC   BSBTYP,BOOKTYPE                                                  
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
         MVC   MLSTYP,INTSTYP                                                   
         MVC   MLBTYP,INTBTYP                                                   
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
         MVC   SBBTYP,BOOKTYPE                                                  
         MVC   SBRMKT,INTMRKT                                                   
         MVC   SBBOOK,INTBOOK                                                   
         GOTO1 ABLDREC,DMCB,(C'P',SBKEY)                                        
         GOTO1 APUTTAPE                                                         
         B     CNVX                                                             
         EJECT                                                                  
*&&DO                                                                           
* BUILD UNIVERSE RECORDS                                                        
*                                                                               
CNV12    LA    R6,THISKEY                                                       
         USING UKEY,R6             BUILD BASIC KEY                              
         XC    THISKEY,THISKEY                                                  
         MVI   UCODE,UCODEQU                                                    
         MVC   UMEDIA,MEDIA                                                     
         MVC   USRC,OUTSRC                                                      
         MVC   UMKT,INTMRKT                                                     
         MVC   UBOOK,INTBOOK                                                    
         XC    UBOOK,=X'FFFF'                                                   
         MVC   UBTYP,BOOKTYPE                                                   
*                                                                               
         LA    R2,INTUNIVS         R2=A(UNIVERSE VALUES)                        
         LA    R3,1                R3=DEMO NUMBER                               
         LA    R0,22               R0=NUMBER OF VALUES                          
*                                                                               
CNV13    STC   R3,UDEMO            SET DEMO NUMBER & UNIVERSE VALUES            
         MVC   UUNIV,0(R2)                                                      
         GOTO1 ABLDREC,DMCB,(C'P',UKEY)                                         
         GOTO1 APUTTAPE                                                         
         LA    R2,4(R2)            BUMP TO NEXT VALUE                           
         LA    R3,1(R3)                                                         
         BCT   R0,CNV13            DO FOR NUMBER OF DEMOS                       
         B     CNVX                                                             
         EJECT                                                                  
*&&                                                                             
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
         MVC   DRBTYP,BOOKTYPE                                                  
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
         USING SATELEM,R6          BUILD SATELLITE STATION ELEMENT              
         OC    SATLASTP,SATLASTP                                                
         BZ    CNV27               EXIT IF TABLE EMPTY                          
         LA    RE,SATTAB                                                        
*                                  LOCATE PARENT IN SATELLITE TABLE             
CNV22    CLI   0(RE),0                                                          
         BE    CNV27                                                            
         CLI   0(RE),C'P'          TEST IF A PARENT ENTRY                       
         BNE   *+14                                                             
         CLC   INTSTA,1(RE)        TEST IF CORRECT PARENT                       
         BE    *+12                                                             
         LA    RE,L'INTSTA+1(RE)   NO - BUMP TO NEXT                            
         B     CNV22                                                            
*                                                                               
         LA    RE,L'INTSTA+1(RE)   RE=A(FIRST SATELLITE ENTRY)                  
         LA    R1,SATCALL          R1=A(SATELLITE IN ELEMENT)                   
         MVI   SATCODE,SATCODEQ                                                 
*                                  BUILD LIST OF SATELLITES IN ELEMENT          
CNV24    CLI   0(RE),C'S'                                                       
         BNE   CNV26                                                            
         MVC   0(L'INTSTA,R1),1(RE)                                             
         LA    R1,L'SATCALL(R1)                                                 
         LA    RE,L'INTSTA+1(RE)                                                
         B     CNV24                                                            
*                                                                               
CNV26    SR    R1,R6                                                            
         STC   R1,SATLEN           SET ELEMENT LENGTH                           
         GOTO1 APUTEL,SATELEM                                                   
*                                  BUILD DPT MARKET INFO ELEMENT                
CNV27    LA    R6,TEMP                                                          
         USING DIELEM,R6                                                        
         MVI   DIECODE,DIECODEQ                                                 
         MVI   DILEN,DIELNEQ                                                    
         MVC   DIMCLASS,INTMKTCL                                                
         MVC   DIMSUBCL,INTSUBCL                                                
         MVC   DIDMAIND,INTDMA                                                  
         MVC   DITZ,INTTZ                                                       
         MVC   DIROUND,INTROUND                                                 
         MVC   DIRSTAT,INTRSTAT                                                 
         MVC   DIAFFL,INTAFFL                                                   
         MVC   DICHNL,INTCHNL                                                   
         MVC   DIADJ1,INTADJ1                                                   
         MVC   DIADJ2,INTADJ2                                                   
         MVC   DIADJ3,INTADJ3                                                   
         GOTO1 APUTEL,DIELEM                                                    
*                                  BUILD QTR HOUR ELEMENT                       
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
         CLI   PUTSW,C'Y'          TEST IF DEMO RECORD WRITTEN                  
         BNE   CNV30                                                            
         GOTO1 APUTEL,QHELEM       NO - ADD ELEMENT                             
*                                                                               
CNV30    CLI   CONDFLAG,0          TEST IF DEMO ELEMENTS BUILT                  
         BNE   CNV32                                                            
         CLI   MEDIA,C'C'          DO NOT GO TO MKTAC FOR CANADA                
         BE    CNV31                                                            
******   GOTO1 VDEDPTAC,DMCB,(C'G',DEMCOND)                                     
*                                                                               
CNV31    LA    R7,DBLOCKA                                                       
         USING DBLOCKD,R7                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'DPT'                                                   
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
*                                                                               
CNV32    CLI   PUTSW,C'Y'          TEST IF DEMO RECORD WRITTEN                  
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
* LITERALS ETC.                                                                 
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
SATLASTP DC    A(0)                A(LAST PARENT IN SATTAB)                     
SATLASTS DC    A(0)                A(LAST SATELLITE IN SATTAB)                  
SATTAB   DS    50CL5               PARENT/SATELLITE STATION TABLE               
*                                  THIS TIME/LAST TIME VALUES                   
THISKEY  DC    XL20'00'                                                         
PREVKEY  DC    XL20'00'                                                         
PREVDAY  DC    X'00'                                                            
PREVQHR  DC    X'00'                                                            
PREVPNAM DC    XL14'00'                                                         
*                                                                               
PUTSW    DC    C'Y'                Y=PREVIOUS RECORD WRITTEN                    
MAXLEN   DC    H'1000'             MAX DEMO RECORD SIZE                         
         SPACE 2                                                                
* DSECT TO COVER TEMPORARY W/S                                                  
*                                                                               
DPTWRKD  DSECT                                                                  
AREC     DS    A                                                                
AFRSTEL  DS    A                                                                
RECLEN   DS    H                                                                
CONDFLAG DS    X                                                                
CONDLEN  DS    XL2                                                              
CONDWRK1 DS    1000C                                                            
CONDWRK2 DS    1000C                                                            
         EJECT                                                                  
       ++INCLUDE DEINTD                                                         
       ++INCLUDE DEINTDPTD                                                      
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
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003DELMDPO   03/03/16'                                      
         END                                                                    
