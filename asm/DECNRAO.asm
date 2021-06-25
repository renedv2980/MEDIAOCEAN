*          DATA SET DECNRAO    AT LEVEL 040 AS OF 12/12/18                      
*PROCESS USING(WARN(15))                                                        
*PHASE DECNRAOA                                                                 
         TITLE 'DEMCON - RADIO CONVERSION - OUTPUT PHASE'                       
DECNRAO  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 RADWRKX-RADWRKD,**RADCNV                                         
         USING RADWRKD,RC          RC=A(W/S)                                    
         USING DEMCOND,R8          R8=A(GLOBAL W/S)                             
         L     R9,ASREC                                                         
         USING INTERD,R9           R9=A(SORT RECORD)                            
         L     RA,ACOMFACS                                                      
         USING COMFACSD,RA         RA=A(COMMON FACILITIES)                      
         B     *+4(R1)                                                          
         B     CNV2                PROCESS A RECORD                             
         B     CNV4                LAST TIME HOOK                               
*                                                                               
CNV2     DS    0H                                                               
         CLI   INTRTYP,1           SATELLITE STATION RECORD                     
         BE    CNV6                                                             
*^^TESTMKT                                                                      
         CLI   RELMKNAM,C'N'                                                    
         BNE   *+12                                                             
         BAS   RE,BLDMKTRC                                                      
         MVI   RELMKNAM,C'Y'                                                    
*^^EOTESTMKT                                                                    
         LA    R6,4(R9)            SET TO SORT KEY                              
         USING DRKEY,R6                                                         
         MVC   SVDRCODE,DRCODE     SAVE THE FILE CODE                           
         DROP  R6                                                               
         CLI   INTRTYP,BSCODEQU    STATION/MARKET RECORD                        
         BE    CNV10                                                            
         CLI   INTRTYP,DRCODEQU    RATINGS RECORD                               
         BE    CNV14                                                            
*&&DO                                                                           
         CLI   INTRTYP,UCODEQU     UNIVERSE RECORD                              
         BE    CNV12                                                            
*&&                                                                             
         CLI   INTRTYP,DSECDEQU    CALL-LETTER CHANGE PASSIVE RECORD            
         BE    CNV60                                                            
         B     CNVX                                                             
*                                                                               
CNV4     XC    SATLASTP,SATLASTP   CLEAR SATELLITE STATION TABLE                
         MVI   INTRTYP,X'FF'                                                    
*&&DO                                                                           
         B     CNV16               GO WRITE LAST RECORD                         
*&&                                                                             
         B     CNV18               LAST RECORD ALREADY WRITTEN                  
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
         MVI   BSHOME,C'H'         SET UP FOR HOME MARKET                       
         MVC   BSKMKT,INTMRKT                                                   
         CLI   INTSPILL,C'Y'       SPILL ?                                      
         BNE   *+8                                                              
         MVI   BSHOME,C'S'         FLAG AS SPILL                                
         MVC   BSSTYP,INTSTYP                                                   
         MVC   BSBTYP,INTBTYP                                                   
         CLI   BOOKTYPE,0          OVERRIDE BTYPE IF AVAILABLE                  
         BE    *+10                                                             
         MVC   BSBTYP,BOOKTYPE                                                  
         MVC   BSRMKT,INTMRKT                                                   
         GOTO1 ABLDREC,DMCB,(C'P',BSKEY)                                        
         GOTO1 APUTTAPE                                                         
*                                                                               
         USING MLKEY,R6            BUILD MARKET/STATION RECORD                  
         XC    THISKEY,THISKEY                                                  
         MVI   MLCODE,MLCODEQU                                                  
         MVC   MLMEDIA,MEDIA                                                    
         MVC   MLSRC,OUTSRC                                                     
         MVC   MLBOOK,INTBOOK                                                   
         XC    MLBOOK,=X'FFFF'                                                  
         MVI   MLIND,MLINDEQU                                                   
         MVC   MLRMKT,INTMRKT                                                   
         MVC   MLSTAT,INTSTA                                                    
         XC    MLKMKT,MLKMKT                                                    
         MVI   MLHOME,C'H'         SET UP FOR HOME MARKET                       
         MVC   MLKMKT,INTMRKT                                                   
         CLI   INTSPILL,C'Y'       SPILL ?                                      
         BNE   *+8                                                              
         MVI   MLHOME,C'S'         FLAG AS SPILL                                
         MVC   MLBTYP,INTBTYP                                                   
         CLI   BOOKTYPE,0          OVERRIDE BTYPE IF AVAILABLE                  
         BE    *+10                                                             
         MVC   MLBTYP,BOOKTYPE                                                  
         GOTO1 ABLDREC,DMCB,(C'P',MLKEY)                                        
         GOTO1 APUTTAPE                                                         
*                                                                               
         USING SBKEY,R6            BUILD STATION/BOOK RECORD                    
         XC    THISKEY,THISKEY                                                  
         MVI   SBCODE,SBCODEQU                                                  
         MVC   SBMEDIA,MEDIA                                                    
         MVC   SBSRC,OUTSRC                                                     
         MVC   SBSTAT,INTSTA                                                    
         MVI   SBHOME,C'H'         SET UP FOR HOME MARKET                       
         MVC   SBKMKT,INTMRKT                                                   
         CLI   INTSPILL,C'Y'       SPILL ?                                      
         BNE   *+8                                                              
         MVI   SBHOME,C'S'         FLAG AS SPILL                                
         MVC   SBSTYP,INTSTYP                                                   
         MVC   SBBTYP,INTBTYP                                                   
         CLI   BOOKTYPE,0          OVERRIDE BTYPE IF AVAILABLE                  
         BE    *+10                                                             
         MVC   SBBTYP,BOOKTYPE                                                  
         MVC   SBRMKT,INTMRKT                                                   
         MVC   SBBOOK,INTBOOK                                                   
         GOTO1 ABLDREC,DMCB,(C'P',SBKEY)                                        
         GOTO1 APUTTAPE                                                         
         B     CNVX                                                             
         EJECT                                                                  
* BUILD UNIVERSE RECORDS                                                        
*                                                                               
CNV12    DC    H'0'                NO UNIVERSE RECORDS FOR RADIO                
         EJECT                                                                  
* BUILD DEMO RECORDS                                                            
*                                                                               
CNV14    LA    R6,THISKEY                                                       
         USING DRKEY,R6            BUILD KEY                                    
         XC    THISKEY,THISKEY                                                  
         MVC   DRCODE,SVDRCODE     SET THE FILE CODE                            
         MVC   DRMEDIA,MEDIA                                                    
         MVC   DRSRC,OUTSRC                                                     
         MVC   DRSTAT,INTSTA                                                    
         MVC   DRBOOK,INTBOOK                                                   
         XC    DRBOOK,=X'FFFF'                                                  
         MVI   DRHOME,C'H'         SET UP FOR HOME MARKET                       
         MVC   DRKMKT,INTMRKT                                                   
         CLI   INTSPILL,C'Y'       SPILL ?                                      
         BNE   *+8                                                              
         MVI   DRHOME,C'S'         FLAG AS SPILL                                
         MVC   DRSTYP,INTSTYP                                                   
         MVC   DRBTYP,INTBTYP                                                   
         CLI   BOOKTYPE,0          OVERRIDE BTYPE IF AVAILABLE                  
         BE    *+10                                                             
         MVC   DRBTYP,BOOKTYPE                                                  
         MVI   CONDFLAG,0          SET DEMO ELEMENT BUILD FLAG                  
*                                                                               
         L     R6,ASREC            SET THE SECTION CODE                         
         LA    R6,4(R6)                                                         
         MVC   MYSECT,DRHIQHR+1                                                 
*                                                                               
         CLC   PREVSTA,INTSTA                                                   
         BNE   CNV16                                                            
         CLC   PREVDAY,INTDAY                                                   
         BNE   CNV16                                                            
         CLC   PREVQHR,INTEQH                                                   
         BNE   CNV16                                                            
         B     CNV52               CONDENSE THE SECTIONS                        
*                                                                               
CNV16    DS    0C                                                               
         L     R6,AOREC                                                         
         LA    R6,4(R6)                                                         
         CLI   DRCODE,C'D'         TAKE CARE OF DAYPART                         
         BE    *+8                                                              
         CLI   DRCODE,DRCODEQU     TAKE CARE OF FIRST TIME                      
         BNE   CNV18                                                            
         MVC   DRHIGHD,PREVDAY     SET HIGH DAY & QTR HOUR IN KEY               
         MVC   DRHIQHR,PREVQHR                                                  
         GOTO1 APUTTAPE            PUT LAST RECORD TO TAPE                      
*                                                                               
CNV18    XC    PREVKEY,PREVKEY     CLEAR LAST TIME VALUES                       
         XC    PREVPNAM,PREVPNAM                                                
                                                                                
         CLI   INTRTYP,X'FF'       EXIT IF LAST TIME HOOK                       
*&&DO                                                                           
         BNE   CNV20                                                            
*&&                                                                             
         BNE   *+12                                                             
         MVI   INTRTYP,DRCODEQU                                                 
         B     CNVX                                                             
                                                                                
         OC    INTSTA,INTSTA       IF THIS IS SET,                              
         BZ    CNVX                 EXIT                                        
*                                  BUILD DEMO RECORDS                           
CNV20    GOTO1 ABLDREC,DMCB,THISKEY                                             
         MVC   PREVKEY,THISKEY                                                  
         LA    R6,TEMP                                                          
         USING MARELEM,R6          BUILD MARKET ELEMENT                         
         MVI   MARCODE,MARCODEQ                                                 
         MVI   MARELN,MARLNEQ2     EXTENDED MARKET ELEMENT                      
         MVC   MARNO,INTMRKT                                                    
         MVC   MARTYPE,INTMTYP                                                  
         MVC   MARSTYP,INTSTYP                                                  
         MVC   MARDATE,TODAYB      COMPRESSED DATE                              
         MVC   MARAIRTM,INTPNO     AIR TIME INDICATOR                           
         MVC   MARACTCD,INTPNO+1   STATION ACTIVITY CODE                        
         GOTO1 APUTEL,MARELEM                                                   
*                                                                               
         USING SATELEM,R6          BUILD SATELLITE STATION ELEMENT              
         OC    SATLASTP,SATLASTP                                                
         BZ    CNV28               EXIT IF TABLE EMPTY                          
         LA    RE,SATTAB                                                        
*                                  LOCATE PARENT IN SATELLITE TABLE             
CNV22    CLI   0(RE),0                                                          
         BE    CNV28                                                            
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
         GOTO1 APUTEL,QHELEM       NO - ADD ELEMENT                             
*                                                                               
         LA    R6,TEMP             BUILD SECTION LEAD                           
         XC    TEMP,TEMP                                                        
         USING SLELEM,R6                                                        
         MVI   SLCODE,SLCODEQ                                                   
         MVI   SLLEN,3                                                          
         MVC   SLSECT,MYSECT                                                    
         GOTO1 APUTEL,SLELEM                                                    
*                                                                               
CNV31    LA    R7,DBLOCKA                                                       
         USING DBLOCKD,R7                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'TP '                                                   
         MVC   DBSELMED,MEDIA                                                   
         MVC   DBSELSRC,OUTSRC                                                  
         MVC   DBSELBK,INTBOOK                                                  
         CLC   INTBOOK,=X'5F0B'    W/NEW FMT: SET 5E TO 5F0B OR GREATER         
         BNL   *+10                                                             
         MVC   DBSELBK,=X'5F0B'                                                 
         CLI   DBSELSRC,C'M'                                                    
         BE    *+8                                                              
         MVI   DBSELSRC,C'A'       FORCE TO ARB                                 
         ST    RA,DBCOMFCS                                                      
         MOVE  (CONDLEN,2000),INTACCS                                           
         GOTO1 CDEMEL,DMCB,(C'C',0),DBLOCKD,CONDLEN                             
         CLI   DBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   CONDFLAG,1                                                       
*                                                                               
CNV32    LA    R1,CONDWRK1         NO - ADD DEMO ELEMENTS TO RECORD             
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
         MVC   PREVSTA,INTSTA                                                   
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
CNV52    LA    R7,DBLOCKA          CREATE DEMO SECTIONS                         
         USING DBLOCKD,R7                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'TP '                                                   
         MVC   DBSELMED,MEDIA                                                   
         MVC   DBSELSRC,OUTSRC                                                  
         MVC   DBSELBK,INTBOOK                                                  
         CLI   DBSELSRC,C'M'                                                    
         BE    *+8                                                              
         MVI   DBSELSRC,C'A'                                                    
         L     RE,AOREC            SET ADDRESS OF RECORD                        
         LA    RE,4(RE)                                                         
         ST    RE,DBAREC                                                        
         L     RE,AOREC            SET END OF RECORD                            
         SR    RF,RF                                                            
         ICM   RF,12,0(RE)          GET LENGTH BYTES                            
         SRL   RF,16                POSITION THEM                               
         LTR   RF,RF                BE CAREFUL                                  
         BNZ   *+6                                                              
         DC    H'0'                 WHOOPS                                      
         AR    RE,RF               *********IN 'RE'                             
         ST    RE,DBAQUART                                                      
         XC    0(60,RE),0(RE)                                                   
*                                                                               
         LA    R6,TEMP             BUILD SECTION LEAD                           
         XC    TEMP,TEMP                                                        
         USING SLELEM,R6                                                        
         MVI   SLCODE,SLCODEQ                                                   
         MVI   SLLEN,3                                                          
         MVC   SLSECT,MYSECT                                                    
         GOTO1 APUTEL,SLELEM                                                    
         B     CNV31               NOW RETURN TO BUILD THE ELEMENTS             
         EJECT                                                                  
***********************************************************************         
*=============== STATION EQUIVALENCE RECORDS (PASSIVE) ===============*         
CNV60    LA    R6,THISKEY                                                       
         USING DSEKEY,R6                                                        
         XC    THISKEY,THISKEY                                                  
         MVC   0(L'DSEKMAJ,R6),INTKEY                                           
         GOTO1 ABLDREC,DMCB,(C'P',DSEKEY)                                       
         GOTO1 APUTTAPE                                                         
         B     CNVX                                                             
         DROP  R6                                                               
***********************************************************************         
*^^TESTMKT                                                                      
         EJECT                                                                  
***********************************************************************         
*======================== BUILD MARKET RECORD ========================*         
BLDMKTRC NTR1                                                                   
         LA    R2,TM                                                            
*                                                                               
BMR010   DS    0H                                                               
         ZICM  R3,0(R2),(3)                                                     
         BZ    BMRX                                                             
                                                                                
         L     R0,AOREC                                                         
         LR    R1,R3                                                            
         LR    RE,R2                                                            
         LR    RF,R3                                                            
         MVCL  R0,RE                                                            
                                                                                
         GOTO1 APUTTAPE                                                         
                                                                                
         AR    R2,R3                                                            
         B     BMR010                                                           
                                                                                
*                                                                               
BMRX     DS    0H                                                               
         J     CNVX                                                             
***********************************************************************         
*^^EOTESTMKT                                                                    
         EJECT                                                                  
* LITERALS ETC.                                                                 
*                                                                               
         LTORG                                                                  
*^^TESTMKT                                                                      
         SPACE 2                                                                
TM       DS    0X                                                               
*                                                                               
TM001Q   EQU   0011                                DETROIT                      
TM001    DS    0X                                                               
         DC     AL2(TM001X-TM001,0)                                             
         DC     CL3'ARA',15X'00'                                                
         DC     AL2(TM001Q),AL2((TM001X-TM001)-4)                               
         DC     X'00'                                                           
TM00101  DS     0X                                                              
         DC      X'01',AL1(TM00101X-TM00101)                                    
         DC      AL2(TM001Q)                                                    
         DC      C'(DETROIT)'                                                   
TM00101X EQU    *                                                               
         DC     X'00'                               EOR MARKER                  
TM001X   EQU   *                                                                
*                                                                               
TM002Q   EQU   0061                                HARTFORD                     
TM002    DS    0X                                                               
         DC     AL2(TM002X-TM002,0)                                             
         DC     CL3'ARA',15X'00'                                                
         DC     AL2(TM002Q),AL2((TM002X-TM002)-4)                               
         DC     X'00'                                                           
TM00201  DS     0X                                                              
         DC      X'01',AL1(TM00201X-TM00201)                                    
         DC      AL2(TM002Q)                                                    
         DC      C'(HARTFORD)'                                                  
TM00201X EQU    *                                                               
         DC     X'00'                               EOR MARKER                  
TM002X   EQU   *                                                                
*                                                                               
TM003Q   EQU   0343                                MODESTO                      
TM003    DS    0X                                                               
         DC     AL2(TM003X-TM003,0)                                             
         DC     CL3'ARA',15X'00'                                                
         DC     AL2(TM003Q),AL2((TM003X-TM003)-4)                               
         DC     X'00'                                                           
TM00301  DS     0X                                                              
         DC      X'01',AL1(TM00301X-TM00301)                                    
         DC      AL2(TM003Q)                                                    
         DC      C'(MODESTO)'                                                   
TM00301X EQU    *                                                               
         DC     X'00'                               EOR MARKER                  
TM003X   EQU   *                                                                
*                                                                               
TM004Q   EQU   0503                                FAYETTEVILLE                 
TM004    DS    0X                                                               
         DC     AL2(TM004X-TM004,0)                                             
         DC     CL3'ARA',15X'00'                                                
         DC     AL2(TM004Q),AL2((TM004X-TM004)-4)                               
         DC     X'00'                                                           
TM00401  DS     0X                                                              
         DC      X'01',AL1(TM00401X-TM00401)                                    
         DC      AL2(TM004Q)                                                    
         DC      C'(FAYETEVILLE)'                                               
TM00401X EQU    *                                                               
         DC     X'00'                               EOR MARKER                  
TM004X   EQU   *                                                                
*                                                                               
         DC    X'0000'                                                          
*^^EOTESTMKT                                                                    
         SPACE 2                                                                
* TABLE OF EXTENDED DATA FILES - BYTES 0-1 = MEDIA/SOURCE                       
*                                                                               
XTENDTAB DS    0CL2                                                             
         DC    C'TN'               US NSI                                       
XTENDENT EQU   (*-XTENDTAB)/L'XTENDTAB                                          
*                                                                               
SATLASTP DC    A(0)                A(LAST PARENT IN SATTAB)                     
SATLASTS DC    A(0)                A(LAST SATELLITE IN SATTAB)                  
SATTAB   DS    50CL5               PARENT/SATELLITE STATION TABLE               
*                                  THIS TIME/LAST TIME VALUES                   
THISKEY  DC    XL20'00'                                                         
PREVKEY  DC    XL20'00'                                                         
PREVDAY  DC    X'00'                                                            
PREVQHR  DC    X'00'                                                            
PREVPNAM DC    XL14'00'                                                         
PREVSTA  DC    XL5'00'                                                          
*                                                                               
MYSECT   DC    X'01'                                                            
SVDRCODE DC    X'00'                                                            
PUTSW    DC    C'Y'                Y=PREVIOUS RECORD WRITTEN                    
MAXLEN   DC    H'2000'             MAX DEMO RECORD SIZE                         
DEMFIL   DC    C'DEMFIL'           HELLO FILE NAME                              
*^^TESTMKT                                                                      
RELMKNAM DC    C'Y'                Y=RELEASED MARKET NAME RECORD                
*^^EOTESTMKT                                                                    
         SPACE 2                                                                
* DSECT TO COVER TEMPORARY W/S                                                  
*                                                                               
RADWRKD  DSECT                                                                  
AREC     DS    A                                                                
AFRSTEL  DS    A                                                                
RECLEN   DS    H                                                                
*                                                                               
MATHFACS DS    0F                  DEMOMATH BLOCK                               
MATHABLK DS    A                   A(USER DBLOCK)                               
MATHFCTR DS    F                                                                
MATHIFIL DS    CL3                                                              
MATHOFIL DS    CL3                                                              
MATHOSRC DS    CL3                                                              
MATHFACL EQU   *-MATHFACS                                                       
*                                                                               
CONDFLAG DS    X                                                                
CONDLEN  DS    XL2                                                              
CONDWRK1 DS    2000C                                                            
CONDWRK2 DS    2000C                                                            
RADWRKX  EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE DEINTD                                                         
       ++INCLUDE DEINTTP95D                                                     
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
**PAN#1  DC    CL21'040DECNRAO   12/12/18'                                      
         END                                                                    
