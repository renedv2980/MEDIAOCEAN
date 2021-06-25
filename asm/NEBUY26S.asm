*          DATA SET NEBUY26S   AT LEVEL 080 AS OF 05/01/02                      
*PHASE T31126A,+0                                                               
         TITLE 'NETPAK BUY PROGRAM - SPECIAL RATE MAINT - T31126'               
T31126   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**BUYUN*,RA,RR=RE                                              
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING T31126+8192,RC                                                   
         L     R9,0(R1)            R9 POINTS TO GLOBAL WORKING STORAGE          
         USING BUYWRKD,R9                                                       
         L     R8,ATWA             R8 POINTS TO THE TWA                         
         USING TWAD,R8                                                          
         L     R7,AOVWORK          R7 POINTS TO LOCAL STORAGE                   
         USING TEMPD,R7                                                         
         ST    RE,MYRELO                                                        
         ST    R1,MYPARM                                                        
         LA    R6,NEBLOCKA         R6 POINTS TO NETBLOCK                        
         USING NEBLOCKD,R6                                                      
         L     R5,ABUYVALS         R5 POINTS TO BUY VALUES                      
         USING BUYVALD,R5                                                       
         SPACE                                                                  
UNIT     TM    MODE,FIRST          FIRST TIME CLEAR SAVE AREA                   
         BZ    *+10                                                             
         XC    SVDATA,SVDATA                                                    
         BAS   RE,ACTED            EDIT ACTION FIELD                            
         CLC   DATE,SVDATE         TEST FOR CHANGE IN AIR DATE                  
         BE    *+8                 NO                                           
         OI    MODE,DISPLAY        YES-FORCE DISPLAY                            
         MVC   SVDATE,DATE                                                      
         CLC   SUB,SVSUB           TEST FOR CHANGE IN SUB-LINE                  
         BE    *+8                                                              
         OI    MODE,DISPLAY        YES-FORCE DISPLAY                            
         MVC   SVSUB,SUB                                                        
         SPACE                                                                  
*                                                                               
* ACTION CHANGE                                                                 
*                                                                               
UNIT010  CLI   ACTION,CSR          TEST FOR ACTION CHANGE                       
         BNE   UNIT190                                                          
         TM    MODE,DISPLAY        TEST FOR FORCED DISPLAY                      
         BO    UNIT190             YES                                          
*                                                                               
         BAS   RE,LOCKPACK                                                      
         BAS   RE,GETUNIT                                                       
*                                                                               
         BAS   RE,EDSCAN                                                        
         BAS   RE,CRSCAN                                                        
*                                                                               
         L     R4,NBAIO                                                         
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(L'NUKEY),0(R4)                                               
         GOTO1 AIO,DMCB,UPDATE+PASSDEL+UNT+DIR+HIGH                             
         CLC   KEY(L'NUKEY),KEYSAVE MAKE SURE KEY IS THERE                      
         BE    *+6                                                              
         DC    H'0'                SOMETHING WRONG                              
         L     R4,AIOAREA1                                                      
*                                                                               
         GOTO1 AIO,DMCB,UPDATE+UNT+FILE+GET,AIOAREA4                            
         GOTO1 (RF),(R1),UNT+FILE+PUT,(R4)  PUT NEW RECORD BACK                 
         SPACE                                                                  
         USING NURECD,R4                                                        
         SPACE                                                                  
UNIT130  BAS   RE,REACT            RE-CONSTRUCT ACTION FIELD                    
         GOTO1 VNETVAL,DMCB,NEBLOCKD                                            
         BAS   RE,DISUNIT          RE-DISPLAY CHANGED UNIT                      
         GOTO1 VBLDRQST                                                         
         B     UNITX                                                            
         SPACE                                                                  
* ACTION DISPLAY OR FORCED DISPLAY                                              
*                                                                               
UNIT190  BAS   RE,GETUNIT                                                       
         BAS   RE,DISUNIT                                                       
         B     UNITX                                                            
         SPACE                                                                  
* SET MESSAGE AND CURSOR THEN EXIT                                              
*                                                                               
UNITX    BAS   RE,MSG                                                           
         NI    MODE,X'FF'-DISPLAY                                               
         B     EXXMOD                                                           
         EJECT                                                                  
* SUB-ROUTINE TO EDIT ACTION FIELD                                              
*                                                                               
ACTED    ST    RE,SAVEREG                                                       
         LA    R2,BUYACTH                                                       
         ST    R2,FADDR                                                         
         XC    FLAST,FLAST                                                      
         XC    FTERM,FTERM                                                      
         MVI   FTERM,COMMA         SEARCH FOR COMMA AT END OF ACTION            
         GOTO1 AFVAL,0                                                          
         MVI   CAACTSW,NO                                                       
         CLI   FSTOP,COMMA         TEST IF COMMA FOUND                          
         BNE   ACTM                NO                                           
         CLC   FLD(2),=C'CA'                                                    
         BNE   *+8                                                              
         MVI   CAACTSW,YES                                                      
         SPACE                                                                  
ACTED2   XC    FTERM,FTERM                                                      
         MVI   FTERM,COMMA                                                      
         GOTO1 AFVAL,0                                                          
         CLI   FLDH+5,0            TEST FOR ANY INPUT                           
         BE    ACTM                                                             
         MVI   FERN,INVERR                                                      
         MVI   FNDX,2                                                           
         GOTO1 VSCANNER,DMCB,FLDH,(1,WORK),C',=,-'                              
         CLI   4(R1),0                                                          
         BE    ERROR                                                            
         SPACE                                                                  
ACTED4   MVI   FERN,DATERR                                                      
         MVI   DDMMYYIP,NO                                                      
         GOTO1 VDATVAL,(R1),(0,WORK+12),DUB                                     
         OC    0(4,R1),0(R1)                                                    
         BZ    ACTED4A                                                          
         CLC   WORK(1),3(R1)       DOES DATE MAKE UP FIRST HALF OF FLD          
         BNE   ERROR               NO                                           
         CLC   DUB(6),ESTSTART     TEST IF DATE BEFORE EST START                
         BL    ERROR               YES                                          
         SR    R0,R0                                                            
         ICM   R0,1,BUYPROF+14                                                  
         BZ    ERROR                                                            
         GOTO1 VADDAY,(R1),ESTEND,DUB2,(R0)                                     
         CLC   DUB(6),DUB2         TEST IF OUTSIDE OF EST + PROF DAYS           
         BH    ERROR               NO-SAME YEAR AS EST START                    
         MVI   DDMMYYIP,YES        YES-MUST BE YEAR OF ESTIMATE END             
         B     ACTED5                                                           
         SPACE                                                                  
ACTED4A  GOTO1 VDATVAL,(R1),(1,WORK+12),DUB                                     
         OC    0(4,R1),0(R1)                                                    
         BZ    ERROR                                                            
         CLC   WORK(1),3(R1)       DOES DATE MAKE UP FIRST HALF OF FLD          
         BNE   ERROR               NO                                           
         MVC   DUB(2),ESTSTART     ESTIMATE START YEAR (INPUT=MMDD)             
         CLC   ESTSTART(2),ESTEND  TEST IF EST START/END IN SAME YEAR           
         BE    ACTED5              YES                                          
         CLC   DUB+2(4),ESTSTART+2 TEST IF INPUT MMDD LT EST ST MMDD            
         BNL   *+10                NO-SAME YEAR AS EST START                    
         MVC   DUB(2),ESTEND       YES-MUST BE YEAR OF ESTIMATE END             
         SPACE                                                                  
ACTED5   GOTO1 VDATCON,(R1),DUB,(2,DATE)                                        
         MVC   CHARDATE,DUB        SAVE YYMMDD DATE                             
         SPACE                                                                  
ACTED6   MVI   SUB,1               DEFAULT IS SUB-LINE=1                        
         CLI   WORK+1,0            TEST FOR SUB-LINE NOTATION                   
         BE    ACTED8                                                           
         MVI   FNDX,0                                                           
         MVI   FERN,INVERR                                                      
         MVC   XTRA,SPACES                                                      
         MVC   XTRA(8),=C'SUB-LINE'                                             
         CLI   WORK+1,3                                                         
         BH    ERROR                                                            
         TM    WORK+3,X'80'        TEST IF NUMERIC                              
         BO    *+12                                                             
         MVI   FERN,NUMERR                                                      
         B     ERROR                                                            
*                                                                               
         ICM   R0,15,WORK+8                                                     
         BZ    ERROR                                                            
         CH    R0,=H'255'                                                       
         BH    ERROR                                                            
         STC   R0,SUB                                                           
         B     ACTED8                                                           
         SPACE                                                                  
ACTED8   MVI   FNDX,3                                                           
ACTED8A  CLI   FSTOP,COMMA         TEST FOR COMMA                               
         BNE   ACTEDX                                                           
         XC    FTERM,FTERM                                                      
         MVI   FTERM,COMMA         SEARCH FOR COMMA TO NEXT PARAMETER           
         MVC   XTRA,SPACES                                                      
         GOTO1 AFVAL,0                                                          
         MVI   FERN,MISERR                                                      
         CLI   FLDH+5,0                                                         
         BE    ERROR                                                            
         MVI   FERN,INVERR         EDIT FOR RE LOOK-UP OF ESTIMATED             
         CLC   FLD(4),=C'EST='     DEMOS                                        
         BNE   ACTED10                                                          
         CLI   FLDH+5,5                                                         
         BL    ERROR                                                            
         CLI   FLDH+5,7                                                         
         BH    ERROR                                                            
         ZIC   R1,FLDH+5                                                        
         SH    R1,=H'5'                                                         
         EX    R1,YESCOMP                                                       
         BNE   ERROR                                                            
         MVI   FERN,PAKFERR                                                     
         L     RE,APACKREC                                                      
         USING NPRECD,RE                                                        
         TM    NPAKSTAT,X'80'      TEST FOR FROZEN PACKAGE                      
         BO    ERROR               CANNOT CHANGE ESTIMATED DEMOS                
         MVI   ESTLOOK,YES                                                      
         MVI   FNDX,4                                                           
         B     ACTED8A             CHECK NEXT PARAMETER                         
*                                                                               
ACTED10  MVI   FERN,INVERR                                                      
         CLC   FLD(3),=C'RS='      CHECK REASON CODE                            
         BNE   ERROR                                                            
         GOTO1 VSCANNER,DMCB,FLDH,(1,WORK),0                                    
         CLI   4(R1),0                                                          
         BE    ERROR                                                            
         CLI   WORK+1,4                                                         
         BH    ERROR                                                            
         MVC   AUDREASN,WORK+22                                                 
         GOTO1 VCKREASN,DMCB,AUDREASN                                           
         MVI   FNDX,4                                                           
         B     ACTED8A                                                          
         DROP  RE                                                               
         SPACE                                                                  
ACTM     MVI   FERN,MISERR                                                      
         MVC   XTRA(9),=C'UNIT DATE'                                            
         B     ERROR                                                            
         SPACE                                                                  
*                                                                               
ACTEDX   CLI   BUYACT,C'D'         IS ACTION DISPLAY                            
         BE    ACTEDX30            DONT CHECK REASON CODE                       
         MVI   FERN,AUDITERR                                                    
         L     RE,APACKREC                                                      
         USING NPRECD,RE                                                        
         TM    NPAKSTAT,X'02'      IS REASON CODE REQUIRED                      
         BO    ACTEDX10            YES                                          
         OC    AUDREASN,AUDREASN   WAS REASON CODE INPUTTED                     
         BZ    ACTEDX30                                                         
         B     ERROR                                                            
ACTEDX10 OC    AUDREASN,AUDREASN   WAS REASON CODE INPUTTED                     
         BZ    ERROR                                                            
*                                                                               
ACTEDX30 MVI   FNDX,0                                                           
         MVC   XTRA,SPACES                                                      
         L     RE,SAVEREG                                                       
         BR    RE                                                               
         DROP  RE                                                               
*                                                                               
YESCOMP  CLC   FLD+4(0),=C'YES'                                                 
         SPACE 2                                                                
* SUB-ROUTINE TO CHECK FOR LOCKED PACKAGE                                       
*                                                                               
LOCKPACK LR    R0,RE               SAVE RETURN POINT                            
         L     RE,APACKREC                                                      
         USING NPRECD,RE                                                        
         TM    NPAKSTAT,X'20'      TEST FOR LOCKED PACKAGE                      
         BZ    LOCKPACX                                                         
         LA    R2,BUYACTH                                                       
         ST    R2,FADDR                                                         
         MVI   FERN,PAKLERR                                                     
         B     ERROR                                                            
         SPACE                                                                  
LOCKPACX LR    RE,R0                                                            
         BR    RE                                                               
         DROP  RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO GET UNIT RECORD                                                
*                                                                               
GETUNIT  ST    RE,SAVEREG                                                       
         L     RE,APACKREC                                                      
         MVC   NBSELDP,NPAKDP-NPRECD(RE)                                        
         GOTO1 VDATCON,DMCB,(2,DATE),NBSELSTR                                   
         MVC   NBSELEND,NBSELSTR                                                
         MVC   NBSELPRG,PROG                                                    
         MVC   NBSELSUB,SUB                                                     
         MVI   NBDATA,C'U'                                                      
         MVI   NBSEQ,C'D'                                                       
         MVI   NBDIRECT,YES                                                     
         MVI   NBSELMOD,NBPROCUN                                                
         MVI   NBESTOPT,C'M'       LOOK UP HOMES VALUES                         
         MVI   NBUSER+13,NO        **FUDGE PROFILE TO RETURN PRE-EMPTS          
         MVC   NBAIO,AIOAREA1                                                   
         MVI   FERN,NOTFOUND                                                    
         SPACE                                                                  
GETUNIT2 GOTO1 VNETIO,DMCB,NEBLOCKD                                             
         CLI   NBERROR,NBGOOD                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   NBMODE,NBPROCUN     TEST FOR UNIT RETURNED                       
         BE    GETUNITX            YES                                          
         CLI   NBMODE,NBREQLST                                                  
         BE    ERROR                                                            
         B     GETUNIT2                                                         
         SPACE                                                                  
GETUNITX L     RE,SAVEREG                                                       
         BR    RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO RECONSTRUCT ACTION FIELD                                       
*                                                                               
REACT    NTR1                                                                   
         MVC   BUYACT,SPACES                                                    
         OI    BUYACTH+6,X'80'                                                  
         LA    R3,BUYACT                                                        
         MVC   0(2,R3),=CL2'DS'                                                 
         CLI   ACTION,DSR                                                       
         BE    RA100                                                            
         MVC   0(2,R3),=CL2'CS'                                                 
RA100    LA    R3,2(R3)                                                         
         MVI   0(R3),COMMA                                                      
         LA    R3,1(R3)                                                         
         CLI   DDMMYYIP,YES        IF YEAR INPUT YEAR OUTPUT                    
         BNE   RA200                                                            
         GOTO1 VDATCON,DMCB,(2,DATE),(5,(R3))                                   
         LA    R3,8(R3)                                                         
         B     RA220                                                            
*                                                                               
RA200    GOTO1 VDATCON,DMCB,(2,DATE),(4,(R3))                                   
         LA    R3,5(R3)                                                         
RA220    CLI   SUB,0                                                            
         BE    REACTX                                                           
         CLI   SUB,1                                                            
         BE    REACTX                                                           
         MVI   0(R3),C'-'                                                       
         ZIC   R2,SUB                                                           
         EDIT  (R2),(3,1(R3)),ALIGN=LEFT                                        
         SPACE                                                                  
REACTX   MVC   SVDATE,DATE         UPDATE SAVE DATE/SUB-LINE IN                 
         MVC   SVSUB,SUB           CASE CALENDARIZING TOOK PLACE.               
         B     EXXMOD                                                           
         EJECT                                                                  
* ROUTINE TO UPDATE ELEMENTS                                                    
* R2 POINTS TO FIRST FIELD ON A LINE                                            
*                                                                               
CRSCAN   NTR1                                                                   
         LA    R2,SPRTYP1H                                                      
         LA    R3,13                                                            
         LA    R4,1                                                             
*                                                                               
         MVI   NOELSW,X'FF'                                                     
         XC    WORK,WORK                                                        
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'03',AIOAREA1),0                    
         CLI   12(R1),6            TEST IF FOUND                                
         BE    CRS040              NO ELEMENT EXISTS                            
         CLI   12(R1),0            TEST IF FOUND                                
         BE    CRS020                                                           
         DC    H'0'                TAKE A HIT IF BAD RETURN CODE                
CRS020   XC    NOELSW,NOELSW                                                    
         L     RF,12(R1)           COPY THE STATUS OF ITS MISSING UNIT          
         BAS   RE,CHKPAY           SET INDICATOR IF LINE IS PAYABLE             
         BAS   RE,CHKBIL           SET INDICATOR IF LINE IS BILLABLE            
         MVC   WORK(12),0(RF)                                                   
*                                                                               
CRS040   CLC   8(2,R2),=CL2'DE'                                                 
         BE    CRS200                                                           
         CLC   8(2,R2),=CL2'  '                                                 
         BE    CRS070                                                           
         CLC   8(2,R2),=2X'00'                                                  
         BE    CRS070                                                           
         MVC   WORK(2),=XL2'030C'  ELEMENT COD/LENGTH                           
         STC   R4,WORK+2           LOAD SEQUENCE NUMBER                         
         LA    R4,1(R4)                                                         
         BAS   RE,BUILD                                                         
* GO TO NEXT LINE                                                               
CRS060   BAS   RE,PAYEDIT                                                       
         LA    R2,SPRTYP2H-SPRTYP1H(R2)                                         
* WRITE ELEMENT BACK                                                            
         CLI   NOELSW,X'FF'                                                     
         BE    CRS080                                                           
         MVC   0(12,RF),WORK                                                    
* GET NEXT ELEMENT                                                              
CRS070   LA    RF,12(RF)                                                        
         CLI   0(RF),X'03'                                                      
         BE    CRS100                                                           
         MVI   NOELSW,X'FF'                                                     
         B     CRS100                                                           
*                                                                               
CRS080   GOTO1 VHELLO,DMCB,(C'P',UNTFILE),AIOAREA1,WORK,0                       
CRS100   BAS   RE,CHKPAY           SET INDICATOR IF LINE IS PAYABLE             
         BAS   RE,CHKBIL           SET INDICATOR IF LINE IS BILLABLE            
         XC    WORK,WORK                                                        
         CLI   NOELSW,X'FF'                                                     
         BE    CRS120                                                           
         MVC   WORK(12),0(RF)                                                   
CRS120   BCT   R3,CRS040                                                        
         GOTO1 VHELLO,DMCB,(C'D',UNTFILE),(X'FF',AIOAREA1),0                    
         B     EXXMOD                                                           
* DELETING ELEMENTS                                                             
CRS200   CLI   NOELSW,X'FF'                                                     
         BNE   CRS220                                                           
         LA    R4,BLOCK            INITIALIZE EDIT BLOCK AND CALL               
         USING UNBLOCKD,R4         EDIT FOR INITIALIZATION                      
         MVC   FERN,UNERROR        NON EXISTENT ELEMENT CANNOT BE DEL           
         B     ERROR                                                            
CRS220   MVI   WORK,X'FF'                                                       
         MVC   0(12,RF),WORK                                                    
         B     CRS060                                                           
         EJECT                                                                  
* ROUTINE TO CONTROL EDIT ROUTINE                                               
* R2 POINTS TO FIRST FIELD ON A LINE                                            
*                                                                               
EDSCAN   NTR1                                                                   
*                                                                               
         LA    R2,SPRTYP1H                                                      
         LA    R3,13                                                            
*                                                                               
EDS020   CLC   8(2,R2),=CL2'DE'                                                 
         BE    EDS040                                                           
         CLC   8(2,R2),=CL2'  '                                                 
         BE    EDS040                                                           
         CLC   8(2,R2),=2X'00'                                                  
         BE    EDS040                                                           
         BAS   RE,BUILD                                                         
EDS040   LA    R2,SPRTYP2H-SPRTYP1H(R2)                                         
         BCT   R3,EDS020                                                        
         B     EXXMOD                                                           
         EJECT                                                                  
* ROUTINE TO EDIT SCREEN AND BUILD UNIT RECORD                                  
* R2 POINTS TO FIRST FIELD ON A LINE                                            
*                                                                               
BUILD    NTR1                                                                   
         LA    R4,BLOCK            INITIALIZE EDIT BLOCK AND CALL               
         USING UNBLOCKD,R4         EDIT FOR INITIALIZATION                      
* EDIT THE TYPE FIELD                                                           
         XC    WORK+3(1),WORK+3                                                 
         LA    R1,TYPTAB                                                        
         LA    R3,8                                                             
BLD020   CLC   8(2,R2),0(R1)                                                    
         BE    BLD040                                                           
         LA    R1,3(R1)                                                         
         BCT   R3,BLD020                                                        
         MVI   FERN,INVERR                                                      
         ST    R2,FADDR                                                         
         B     ERROR                                                            
BLD040   MVC   WORK+3(1),2(R1)                                                  
* BUMP R2 TO NEXT UNPROTECTED FIELD                                             
         BAS   RE,BUMPFLD          FIND NEXT FIELD                              
         BAS   RE,BUMPFLD          FIND NEXT FIELD                              
* EDIT THE AMOUNT FIELD                                                         
         TM    4(R2),X'20'         HAS FIELD BEEN PREVIOSLY VALIDATED           
         BO    BLD060                                                           
         BAS   RE,ASS                                                           
*                                                                               
BLD060   BAS   RE,BUMPFLD          FIND NEXT FIELD                              
*                                                                               
* EDIT THE SREP FIELD                                                           
         TM    4(R2),X'20'         HAS FIELD BEEN PREVIOSLY VALIDATED           
         BO    BLD120                                                           
         MVC   THREE,=3X'00'                                                    
         GOTO1 VCALLOV,DMCB,(X'35',0),ATWA                                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VEDIT,DMCB          GENERAL EDIT MODULE                          
         SPACE                                                                  
         XC    UNBLOCK,UNBLOCK                                                  
         ST    R9,UNAGLOB                                                       
         MVC   UNAREC,AIOAREA1                                                  
         LA    RE,TEMPD+1500                                                    
         ST    RE,UNALOCAL         LOCAL STORAGE TO EDIT MODULE                 
*                                                                               
         CLI   ACTION,CSR          TEST FOR ACTION CHANGE                       
         BNE   BLD080              NO                                           
         MVC   UNODATE,NBACTDAT    SET ORIGINAL RECORD VALUES                   
         MVC   UNOSUB,NBACTSUB                                                  
         MVC   UNOTIME,NBACTSQH                                                 
         MVC   UNODAY,NBDAY                                                     
         SPACE                                                                  
BLD080   GOTO1 VEDIT,DMCB,(C'I',(R4))                                           
         SPACE                                                                  
         LA    R3,EDLIST           POINT R3 AT EDIT LIST                        
         L     RF,VEDIT                                                         
         LA    R1,DMCB                                                          
         SPACE                                                                  
BLD100   CLI   0(R3),X'FF'         TEST FOR E-O-L                               
         BE    BLD100                                                           
         ST    R2,UNFLDH           FIELD HEADER POINTER                         
         ST    R2,FADDR                                                         
         MVC   UNEDATA,0(R3)       SET DATA TYPE                                
         GOTO1 VGETFLD                                                          
         CLI   FLDH+5,0                                                         
         BE    BLD110                                                           
         GOTO1 VEDIT,(R1),(C'S',(R4))                                           
         CLI   UNERROR,0           TEST FOR ERROR                               
         BE    *+14                NONE                                         
         MVC   FERN,UNERROR                                                     
         B     ERROR                                                            
*                                                                               
BLD110   MVC   WORK+9(3),THREE                                                  
*                                                                               
BLD120   LA    R3,1(R3)                                                         
         BAS   RE,BUMPFLD          FIND NEXT FIELD                              
         SPACE                                                                  
* EDIT THE COMMISION FIELD                                                      
         LA    R1,TYPTAB1                                                       
         LA    R3,2                                                             
BLD140   CLC   8(1,R2),0(R1)                                                    
         BE    BLD160                                                           
         LA    R1,3(R1)                                                         
         BCT   R3,BLD140                                                        
         MVI   WORK+4,C'C'                                                      
         B     EXXMOD                                                           
BLD160   MVC   WORK+4(1),2(R1)                                                  
         B     EXXMOD                                                           
TYPTAB   DC    CL2'CI',C'U'                                                     
         DC    CL2'BO',C'B'                                                     
         DC    CL2'CS',C'S'                                                     
         DC    CL2'OT',C'O'                                                     
         DC    CL2'TX',C'X'                                                     
         DC    CL2'SE',C'E'                                                     
         DC    CL2'AD',C'A'                                                     
         DC    CL2'LC',C'L'                                                     
TYPTAB1  DC    CL2'Y ',C'C'                                                     
         DC    CL2'N ',C' '                                                     
         DROP  R4                                                               
         EJECT                                                                  
* ASSIGNED COST                                                                 
*                                                                               
ASS      NTR1                                                                   
         LA    R4,BLOCK                                                         
         USING UNBLOCKD,R4                                                      
*                                                                               
         MVC   FLDH(18),0(R2)                                                   
*                                                                               
         CLI   FLDH+5,0            TEST FOR INPUT                               
         BE    ASSX                                                             
         ZIC   R1,FLDH+5                                                        
         LA    RE,FLD-1(R1)        LOOK AT LAST CHARACTER                       
         CLI   0(RE),C'-'          TEST FOR A DASH                              
         BNE   ASS4                                                             
         MVI   0(RE),C' '                                                       
         MVI   MINUSSW,YES                                                      
         SH    R1,=H'1'                                                         
         BZ    ASSR                ITS AN ERROR - ONLY DASH IN FIELD            
         SPACE 1                                                                
ASS4     CLI   FLD,C'N'                                                         
         BNE   ASS8                                                             
         MVI   NETSW,YES                                                        
         SH    R1,=H'1'                                                         
         BZ    ASSR                ITS AN ERROR - ONLY DASH IN FIELD            
         MVC   FLD(78),FLD+1                                                    
         SPACE 1                                                                
ASS8     LR    R0,R1               DATA LENGTH                                  
         GOTO1 VCASHVAL,DMCB2,FLD,(R0)                                          
         CLI   0(R1),X'FF'                                                      
         BE    ASSR                                                             
         ICM   RF,15,4(R1)         GET AMOUNT                                   
         CLI   NETSW,YES                                                        
         BNE   ASS10                                                            
         SR    RE,RE               GROSS UP THE NET VALUE                       
         LA    R1,100                                                           
         MR    RE,R1                                                            
         SR    RE,RE                                                            
         LA    R1,85                                                            
         DR    RE,R1                                                            
*                                                                               
ASS10    CLI   MINUSSW,YES         TEST FOR MINUS AMOUNT                        
         BNE   *+6                                                              
         LNR   RF,RF               YES-FORCE COST NEGATIVE                      
         STCM  RF,15,WORK+5                                                     
*                                                                               
ASSX     XC    MINUSSW(2),MINUSSW  CLEAR INDICATORS                             
         B     EXXMOD                                                           
         SPACE 1                                                                
ASSR     MVI   FERN,INVERR                                                      
         ST    R2,FADDR                                                         
         B     ERROR                                                            
         DROP  R4                                                               
         EJECT                                                                  
* SUB-ROUTINE TO DISPLAY UNIT RECORD VALUES                                     
*                                                                               
DISUNIT  NTR1                                                                   
*  CLEAR THE SCREEN                                                             
         BAS   RE,CLRSCRN                                                       
         LA    R2,SPRTYP1H                                                      
*        GOTO1 VCLEARF,DMCB,(R2),SPRLAST                                        
         SPACE                                                                  
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'03',AIOAREA1),0                    
         CLI   12(R1),6            TEST IF FOUND                                
         BE    DISEXIT             NO ELEMENT EXISTS EXIT ROUTINE               
         CLI   12(R1),0            TEST IF FOUND                                
         BE    DIS020                                                           
         DC    H'0'                TAKE A HIT IF BAD RETURN CODE                
DIS020   L     R4,12(R1)                                                        
         LA    R2,SPRTYP1H                                                      
         LA    R3,13                                                            
* FIND RECORD TYPE                                                              
DIS040   CLI   3(R4),C'U'                                                       
         BNE   DIS050                                                           
         MVC   8(2,R2),=C'CI'                                                   
         BAS   RE,TRANFLD                                                       
         MVC   8(6,R2),=C'CUT-IN'                                               
         B     DIS120                                                           
DIS050   CLI   3(R4),C'B'                                                       
         BNE   DIS060                                                           
         MVC   8(2,R2),=C'BO'                                                   
         BAS   RE,TRANFLD                                                       
         MVC   8(8,R2),=C'BLACKOUT'                                             
         B     DIS120                                                           
DIS060   CLI   3(R4),C'S'                                                       
         BNE   DIS070                                                           
         MVC   8(2,R2),=C'CS'                                                   
         BAS   RE,TRANFLD                                                       
         MVC   8(10,R2),=C'COPY-SPLIT'                                          
         B     DIS120                                                           
DIS070   CLI   3(R4),C'X'                                                       
         BNE   DIS080                                                           
         MVC   8(2,R2),=C'TX'                                                   
         BAS   RE,TRANFLD                                                       
         MVC   8(3,R2),=C'TAX'                                                  
         B     DIS120                                                           
DIS080   CLI   3(R4),C'E'                                                       
         BNE   DIS090                                                           
         MVC   8(2,R2),=C'SE'                                                   
         BAS   RE,TRANFLD                                                       
         MVC   8(9,R2),=C'SECTIONAL'                                            
         B     DIS120                                                           
DIS090   CLI   3(R4),C'A'                                                       
         BNE   DIS100                                                           
         MVC   8(2,R2),=C'AD'                                                   
         BAS   RE,TRANFLD                                                       
         MVC   8(5,R2),=C'ADMIN'                                                
         B     DIS120                                                           
DIS100   CLI   3(R4),C'L'                                                       
         BNE   DIS110                                                           
         MVC   8(2,R2),=C'LC'                                                   
         BAS   RE,TRANFLD                                                       
         MVC   8(9,R2),=C'LATE CHRG'                                            
         B     DIS120                                                           
DIS110   MVC   8(2,R2),=C'OT'                                                   
         BAS   RE,TRANFLD                                                       
         MVC   8(5,R2),=C'OTHER'                                                
DIS120   BAS   RE,TRANFLD          TRANSMIT FIELD                               
* AMOUNT FIELD                                                                  
         ICM   RE,15,5(R4)                                                      
         BZ    DIS140                                                           
         TM    5(R4),X'80'         TEST FOR MINUS UNIT                          
         BZ    *+6                                                              
         LNR   RE,RE               CONVERT COST TO NEGATIVE NUMBER              
         LR    R0,RE               SAVE COST VALUE                              
         SRDA  RE,32               PREPARE DIVIDEND                             
         D     RE,=F'100'          SEPARATE DOLLARS AND PENNIES                 
         LA    R1,8(R2)            SET UP OUTPUT FIELD                          
         LTR   RE,RE               TEST REMAINDER (PENNIES)                     
         BNZ   DIS130              YES                                          
         ST    RF,FULL                                                          
         EDIT  FULL,(10,(R1)),ALIGN=LEFT,MINUS=YES                              
         BAS   RE,TRANFLD          TRANSMIT FIELD                               
         B     DIS150                                                           
*                                                                               
DIS130   ST    R0,FULL             RESTORE COST VALUE W PENNIES                 
         EDIT  FULL,(10,(R1)),2,ALIGN=LEFT,MINUS=YES                            
DIS140   BAS   RE,TRANFLD          TRANSMIT FIELD                               
* SPECIAL REP FIELD                                                             
DIS150   MVC   8(3,R2),9(R4)                                                    
         BAS   RE,TRANFLD          TRANSMIT FIELD                               
* COMMISSION FIELD                                                              
         MVI   8(R2),C'N'                                                       
         CLI   4(R4),C'C'                                                       
         BNE   DIS160                                                           
         MVI   8(R2),C'Y'                                                       
DIS160   BAS   RE,TRANFLD          TRANSMIT FIELD                               
         BAS   RE,BUMPFLD          NEXT FIELD                                   
* GET NEXT ELEMENT                                                              
         LA    R4,12(R4)                                                        
         CLI   0(R4),X'03'                                                      
         BNE   DISEXIT             DATA TYPE                                    
         BCT   R3,DIS040           DISPLACEMENT FROM FIRST DISPLAY HDR.         
DISEXIT  B     EXXMOD                                                           
         EJECT                                                                  
* SUB-ROUTINE TO DISPLAY UNIT RECORD VALUES                                     
*                                                                               
CLRSCRN  NTR1                                                                   
*  CLEAR THE SCREEN                                                             
         LA    R1,13                                                            
         LA    R3,SPRTYP1H                                                      
*                                                                               
CLRS020  LR    R2,R3                                                            
         OI    4(R2),X'20'         SET PREVALID BIT                             
         XC    8(2,R2),8(R2)                                                    
         BAS   RE,TRANFLD                                                       
         OI    4(R2),X'20'         SET PREVALID BIT                             
         XC    8(10,R2),8(R2)                                                   
         BAS   RE,TRANFLD                                                       
         OI    4(R2),X'20'         SET PREVALID BIT                             
         XC    8(10,R2),8(R2)                                                   
         BAS   RE,TRANFLD                                                       
         OI    4(R2),X'20'         SET PREVALID BIT                             
         XC    8(3,R2),8(R2)                                                    
         BAS   RE,TRANFLD                                                       
         OI    4(R2),X'20'         SET PREVALID BIT                             
         XC    8(1,R2),8(R2)                                                    
         BAS   RE,TRANFLD                                                       
         LA    R3,SPRTYP2H-SPRTYP1H(R3)                                         
         BCT   R1,CLRS020                                                       
         B     EXXMOD                                                           
         EJECT                                                                  
* SUB-ROUTINE TO CHECK TO SEE IF PAYABLE EXISTS FOR SPECIAL RATE TYPE           
*  SRPAYED=PAID INDIOCATOR FIELD                                                
*  R3=A(PAYING ELEMENT)                                                         
*  R4=A(SPECIAL RATE ELEMENT)                                                   
CHKPAY   NTR1                                                                   
         XC    SRPAYED,SRPAYED                                                  
         CLI   NOELSW,X'FF'                                                     
         BE    CHKEXIT                                                          
         LR    R4,RF                                                            
         USING NUPAYD,R3                                                        
         USING NUSPRD,R4                                                        
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'12',AIOAREA1),0                    
         CLI   12(R1),6            TEST IF FOUND                                
         BE    CHKEXIT             NO ELEMENT EXISTS EXIT ROUTINE               
         CLI   12(R1),0            TEST IF FOUND                                
         BE    *+6                                                              
         DC    H'0'                TAKE A HIT IF BAD RETURN CODE                
         L     R3,12(R1)                                                        
CHKPY050 CLC   NUPAYTYP,NUSPRTYP   FIND NEXT UNPROTECTED FIELD                  
         BNE   CHKPY100                                                         
         CLC   NUPAYSRP,NUSPRREP                                                
         BNE   CHKPY100                                                         
         MVI   SRPAYED,X'FF'                                                    
         B     CHKEXIT                                                          
CHKPY100 ZIC   R1,1(R3)                                                         
         AR    R3,R1                                                            
         CLI   0(R3),X'12'                                                      
         BE    CHKPY050                                                         
CHKEXIT  B     EXXMOD                                                           
         DROP  R3,R4                                                            
         EJECT                                                                  
* SUB-ROUTINE TO CHECK TO SEE IF BILLABLE EXISTS FOR SPECIAL RATE TYPE          
*  SRBILED=BILL INDIOCATOR FIELD                                                
*  R3=A(BILLING ELEMENT)                                                        
*  R4=A(SPECIAL RATE ELEMENT)                                                   
CHKBIL   NTR1                                                                   
         XC    SRBILED,SRBILED                                                  
         XC    FULL,FULL                                                        
         CLI   NOELSW,X'FF'                                                     
         BE    CHKBEXIT                                                         
         LR    R4,RF                                                            
         USING NUBILD,R3                                                        
         USING NUSPRD,R4                                                        
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'10',AIOAREA1),0                    
         CLI   12(R1),6            TEST IF FOUND                                
         BE    CHKBEXIT            NO ELEMENT EXISTS EXIT ROUTINE               
         CLI   12(R1),0            TEST IF FOUND                                
         BE    *+6                                                              
         DC    H'0'                TAKE A HIT IF BAD RETURN CODE                
         L     R3,12(R1)                                                        
CHKBL050 CLC   NUBILTYP,NUSPRTYP   FIND NEXT UNPROTECTED FIELD                  
         BNE   CHKBL100                                                         
         ICM   RE,15,FULL                                                       
         ICM   RF,15,NUBILGRS                                                   
         AR    RE,RF                                                            
         STCM  RE,15,FULL                                                       
CHKBL100 ZIC   R1,1(R3)                                                         
         AR    R3,R1                                                            
         CLI   0(R3),X'10'                                                      
         BE    CHKBL050                                                         
CHKBL150 OC    FULL,FULL                                                        
         BZ    CHKBEXIT                                                         
         MVI   SRBILED,X'FF'                                                    
CHKBEXIT B     EXXMOD                                                           
         DROP  R3,R4                                                            
         EJECT                                                                  
* SUB-ROUTINE TO CHECK TO FLAG CHANGES TO PAID SPECIAL RATE CHARGES             
*  SRPAYED=PAID INDICATOR FIELD                                                 
*  RF=A(SPECIAL RATE ELEMENT)                                                   
*  WORK=UPDATED SPECIAL RATE ELEMENT                                            
PAYEDIT  NTR1                                                                   
         CLI   SRPAYED,X'FF'       DOES THIS SRATE HAVE PAYABLE AGAINST         
         BNE   PAYE050                                                          
*                                                                               
         MVI   FERN,REPDERR                                                     
         CLC   8(2,R2),=CL2'DE'    IS DELETE ACTION REQUESTED                   
         BE    ERROR                                                            
         MVI   FERN,REPCHERR                                                    
         CLC   9(3,RF),WORK+9      HAS REP BEEN CHANGED                         
         BNE   ERROR                                                            
         CLC   3(1,RF),WORK+3      HAS PAY TYPE BEEN CHANGED                    
         BNE   ERROR                                                            
*                                                                               
PAYE050  CLI   SRBILED,X'FF'       DOES THIS SRATE HAVE BILLING AGAINST         
         BNE   PAYEDEX                                                          
*                                                                               
         MVI   FERN,INVERR                                                      
         CLC   8(2,R2),=CL2'DE'    IS DELETE ACTION REQUESTED                   
         BE    ERROR                                                            
PAYEDEX  B     EXXMOD                                                           
         EJECT                                                                  
* SUB-ROUTINE TO TRANSMIT SCREEN FIELDS                                         
*                                                                               
*  CLEAR THE SCREEN                                                             
TRANFLD  OI    6(R2),X'80'         TRANSMIT FIELD                               
BUMPFLD  SR    R0,R0               FIND NEXT UNPROTECTED FIELD                  
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO OUTPUT MESSAGE AND TO SET CURSOR                               
*                                                                               
MSG      ST    RE,SAVEREG                                                       
         LA    RE,ACTTAB                                                        
         LA    R0,ACTS             COUNTER                                      
         CLC   ACTION,0(RE)                                                     
         BE    *+14                                                             
         LA    RE,L'ACTTAB(RE)                                                  
         BCT   R0,*-14                                                          
         DC    H'0'                                                             
*                                                                               
         MVC   BUYMSG(4),=C'UNIT'                                               
         MVC   BUYMSG+5(9),1(RE)                                                
         LA    R2,BUYACTH          SET CURSOR POSITION                          
         TM    MODE,DISPLAY        TEST FOR CHANCE OF FORCED DISPLAY            
         BZ    MSGX                NO                                           
         SPACE                                                                  
MSG2     CLI   ACTION,CSR          TEST FOR ACTION BUY                          
         BE    MSGX                ALL DONE                                     
         CLI   ACTION,DSR          TEST FOR DISPLAY                             
         BE    MSGX                YES                                          
         MVC   BUYMSG+5(9),=C'DISPLAYED'                                        
         LA    R3,BUYMSG+15                                                     
         LA    R2,SPRTYP1H                                                      
         MVC   0(15,R3),=C'- ENTER CHANGES'                                     
         CLI   ACTION,CSR                                                       
         BE    MSGX                                                             
         MVC   0(20,R3),=C'- NOW YOU MAY DELETE'                                
         OI    1(R2),X'01'         CONVERT CURSOR TO MODIFIED                   
         B     MSGX                                                             
         SPACE                                                                  
MSGX     ST    R2,FADDR                                                         
         L     RE,SAVEREG                                                       
         BR    RE                                                               
         EJECT                                                                  
* MODULE AND ROUTINE EXIT                                                       
*                                                                               
EXXMOD   XMOD1 1                                                                
         SPACE 2                                                                
* ERROR EXIT                                                                    
*                                                                               
ERROR    GOTO1 VERROR                                                           
         SPACE 2                                                                
* PATCH AREA                                                                    
*                                                                               
PATCH    DS    0H                                                               
         DC    XL32'00'                                                         
         SPACE 2                                                                
* CONSTANTS                                                                     
*                                                                               
UNTFILE  DC    CL8'UNTFILE'                                                     
NOCHGERR DC    C'** ERROR - MAKE-GOOD DETAILS CANNOT BE CHANGED FOR UNIX        
               T**'                                                             
         SPACE 2                                                                
* LIST OF FIELDS TO EDIT                                                        
*                                                                               
EDLIST   DS    0H                                                               
         DC    AL1(USREP)                                                       
         DC    X'FF'                                                            
         SPACE 2                                                                
* TABLE OF ACTIONS AND THEIR NAMES                                              
*                                                                               
ACTTAB   DS    0CL10                                                            
         DC    AL1(CSR),CL9'CHANGED'                                            
         DC    AL1(DSR),CL9'DISPLAYED'                                          
ACTS     EQU   (*-ACTTAB)/L'ACTTAB                                              
         SPACE 2                                                                
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE NEBUYWRK                                                       
         EJECT                                                                  
* UNIT SCREEN                                                                   
*                                                                               
         ORG   BUYLAST                                                          
       ++INCLUDE NEBUYF5D                                                       
         SPACE 2                                                                
* SAVE AREA                                                                     
*                                                                               
         ORG   TWAD+PAGELEN                                                     
SVDATA   DS    CL256                                                            
         ORG   SVDATA                                                           
SVDATE   DS    XL2                 SAVED AIR DATE                               
SVSUB    DS    X                   SAVED SUB-LINE                               
         EJECT                                                                  
* DSECT TO COVER LOCAL MORKING STORAGE                                          
*                                                                               
TEMPD    DSECT                                                                  
MYRELO   DS    A                                                                
MYPARM   DS    A                                                                
SAVEREG  DS    A                                                                
VDISPLAY DS    V                   V(GENERAL UNIT DISPLAY)                      
VEDIT    DS    V                   V(GENERAL UNIT EDIT)                         
*                                                                               
UNITDA   DS    XL4                 UNIT RECORD DISK ADDRESS                     
*                                                                               
DATE     DS    XL2                 DATE EXTRACTED FROM ACTION FIELD             
CHARDATE DS    CL6                 DATE FROM ACTION FIELD - YYMMDD              
SUB      DS    X                   SUBLINE EXTRACTED FROM ACTION FIELD          
TIME     DS    X                   SQH (SAVED)                                  
*                                                                               
*                                                                               
MGCHGSW  DS    C                   MAKE-GOOD DETAIL CHANGE (Y/N)                
ESTLOOK  DS    C                   FORCE LOOKUP OF ESTIMATED DEMOS (Y)          
DDMMYYIP DS    X                   MAKE-GOOD YEAR INPUT                         
SVBYPR14 DS    X                   TELL EDIT TO SKIP MG OUT OF EST CK           
CAACTSW  DS    CL1                 ALLOW CHANGE TO PAYED UNIT                   
MAKEGDSW DS    CL1                 THIS IS OR WAS A MAKEGOOD                    
FRCSW    DS    CL1                 IF SINGLE MG USE IT'S OWN DEMO'S             
NOELSW   DS    CL1                 SPECIAL RATE ELEMENT SW                      
MINUSSW  DS    CL1                 NEGATIVE AMOUNT SW                           
NETSW    DS    CL1                 NET VALUE SW                                 
SRPAYED  DS    CL1                 SPECIAL RATE HAS PAID AGAINST IT             
SRBILED  DS    CL1                 SPECIAL RATE HAS BILLING AGAINST IT          
SVINTTBL DS    CL70                SAVED INTEGRATION TABLES                     
MGDETS   DS    0XL104                                                           
MGCODE   DS    CL6                 MAKE-GOOD PROGRAM CODE                       
MGDATE   DS    XL2                 MAKE-GOOD DATE                               
MGSUB    DS    X                   MAKE-GOOD SUB-LINE                           
MGPNAM   DS    CL16                MAKE-GOOD PROGRAM NAME                       
MGPSTAT  DS    X                   MAKE-GOOD STATUS                             
         DS    CL78                                                             
         DS    XL1                 0 FOR NEW MAKEGOODS                          
OMGDETS  DS    0CL36               OLD TABLE OF UP TO 4 MAKEGOODS               
OMGCODE  DS    CL6                 OLD PROGRAM CODE                             
OMGDATE  DS    XL2                 OLD DATE                                     
OMGSUB   DS    XL1                 OLD NUMBER                                   
         DS    CL27                                                             
         DS    XL1                 0 FOR OLD MAKEGOODS                          
*                                                                               
         DS    0F                                                               
BLOCK    DS    CL256                                                            
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'080NEBUY26S  05/01/02'                                      
         END                                                                    
