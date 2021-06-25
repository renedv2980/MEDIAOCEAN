*          DATA SET NEBUY42    AT LEVEL 180 AS OF 02/10/11                      
*PHASE T31142B,+0                                                               
         TITLE 'NETPAK BUY PROGRAM - PRODUCT ALLOCATION - T31142'               
T31142   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**BPROD*,RA,RR=RE                                              
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING T31142+8192,RC                                                   
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
         L     R5,ABUYVALS                                                      
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
UNIT010  CLI   ACTION,CPR          TEST FOR ACTION CHANGE                       
         BNE   UNIT190                                                          
         TM    MODE,DISPLAY        TEST FOR FORCED DISPLAY                      
         BO    UNIT190             YES                                          
*                                                                               
         BAS   RE,LOCKPACK                                                      
         BAS   RE,GETUNIT                                                       
*                                                                               
         NI    MYFLAG,X'FF'-WASTRI                                              
         NI    MYFLAG,X'FF'-FORCZERO                                            
*                                                                               
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'14',AIOAREA1),0                    
         CLI   12(R1),0            TEST IF FOUND                                
         BNE   UNIT050                                                          
         L     RF,12(R1)                                                        
         TM    2(RF),X'80'         IS THIS A TRIGGY?                            
         BZ    UNIT050                                                          
         OI    MYFLAG,WASTRI                                                    
*                                                                               
UNIT050  BAS   RE,BUILD                                                         
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
         GOTO1 VBLDRQST                                                         
         SPACE                                                                  
         USING NURECD,R4                                                        
         SPACE                                                                  
UNIT130  BAS   RE,REACT            RE-CONSTRUCT ACTION FIELD                    
         GOTO1 VNETVAL,DMCB,NEBLOCKD                                            
         BAS   RE,DISUNIT          RE-DISPLAY CHANGED UNIT                      
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
         CLI   FSTOP,COMMA         TEST IF COMMA FOUND                          
         BNE   ACTM                NO                                           
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
ACTED8   CLI   FSTOP,COMMA         TEST FOR COMMA                               
         BNE   ACTEDX                                                           
         XC    FTERM,FTERM                                                      
         MVC   XTRA,SPACES                                                      
         GOTO1 AFVAL,0                                                          
         MVI   FNDX,3                                                           
         MVI   FERN,MISERR                                                      
         CLI   FLDH+5,0                                                         
         BE    ERROR                                                            
*                                                                               
         MVI   FERN,INVERR                                                      
         CLC   FLD(3),=C'RS='      CHECK REASON CODE                            
         BNE   ACTED10                                                          
         GOTO1 VSCANNER,DMCB,FLDH,(1,WORK),0                                    
         CLI   4(R1),0                                                          
         BE    ERROR                                                            
         CLI   WORK+1,4                                                         
         BH    ERROR                                                            
         MVC   AUDREASN,WORK+22                                                 
         GOTO1 VCKREASN,DMCB,AUDREASN                                           
         B     ACTED8                                                           
*                                                                               
ACTED10  MVI   FERN,INVERR         EDIT FOR RE LOOK-UP OF ESTIMATED             
         CLC   FLD(4),=C'EST='     DEMOS                                        
         BNE   ERROR                                                            
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
         B     ACTEDX                                                           
         DROP  RE                                                               
         SPACE                                                                  
ACTM     MVI   FERN,MISERR                                                      
         MVC   XTRA(9),=C'UNIT DATE'                                            
         B     ERROR                                                            
         SPACE                                                                  
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
         SPACE 2                                                                
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
         MVC   0(3,R3),=CL3'CPR'                                                
RA100    LA    R3,3(R3)                                                         
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
* ROUTINE TO EDIT SCREEN AND BUILD UNIT RECORD                                  
* R2 POINTS TO FIRST FIELD ON A LINE                                            
*                                                                               
BUILD    NTR1                                                                   
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'02',AIOAREA1),0                    
         CLI   12(R1),0            TEST IF FOUND                                
         BE    *+6                                                              
         DC    H'0'                TAKE A HIT IF BAD RETURN CODE                
         L     RF,12(R1)                                                        
         USING NUSDRD,RF                                                        
         NI    NUSDST3,X'FF'-X'01'   REMOVE FEED PCT HITS IMPS/RATS             
         MVC   SVSTAT3,NUSDST3                                                  
         DROP  RF                                                               
*                                                                               
         XC    FEEDEDA,FEEDEDA                                                  
         MVI   FEEDEDA,X'FF'                                                    
         MVI   FEEDPRNM,1                                                       
*                                                                               
         LA    R2,UNTCSPH                                                       
         ST    R2,FADDR                                                         
*                                                                               
         TM    MYFLAG,WASTRI       WAS IT ORIGINALLY A TRI?                     
         BZ    BLD5                                                             
         CLI   UNTCSP,C'T'         MUST STILL BE A TRIGGY                       
         BE    BLD5                                                             
*                                                                               
         MVC   BUYMSG(L'NOCHTRI),NOCHTRI                                        
         MVI   FERN,USERERR                                                     
         B     ERROR                                                            
*                                                                               
BLD5     DS    0H                                                               
         TM    UNTCSPH+4,X'80'     USER CHANGED THIS FIELD?                     
         BZ    *+8                                                              
         BAS   RE,CLRPRD           CLEAR THE BOTTOM SCREEN                      
*                                                                               
         CLI   UNTCSP,C'Y'         IS COPY SPLIT YES                            
         BE    BLD20                                                            
         CLI   UNTCSP,C'U'         IS COPY SPLIT U (UNALLOCATE)                 
         BNE   *+12                                                             
         BAS   RE,UNALLPRD         UNALLOCATE THE UNIT                          
         B     BLD560                                                           
         CLI   UNTCSP,C'C'         IS COPY SPLIT C (CLEAR PERCENTS)             
         BNE   *+12                                                             
         BAS   RE,CLRPCTS          CLEAR THE PERCENTS                           
         B     BLD20                                                            
*                                                                               
         CLI   UNTCSP,C'S'         IS COPY SPLIT S (SECTIONAL MAP)              
         BNE   BLD7                                                             
         CLC   =C'H9',AGENCY                                                    
         BE    BLD20                                                            
         CLC   =C'SJ',AGENCY                                                    
         BE    BLD20                                                            
         MVI   FERN,INVERR                                                      
         B     ERROR                                                            
*                                                                               
BLD7     DS    0H                                                               
         B     BLD10                                                            
*  TRIGGYBACK FUNCTION HAS BEEN DISABLED, IF                                    
*  THERE IS A NEED TO REACTIVATE REMOVE THE                                     
*  ABOVE INSTRUCTION "B   BLD10"                                                
         CLI   UNTCSP,C'T'         IS TRIGGY BACK?                              
         BNE   BLD10                                                            
         BAS   RE,CLRPCTS          CLEAR THE PERCENTS                           
*                                                                               
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'21',AIOAREA1),0                    
         CLI   12(R1),0            TEST IF FOUND                                
         BNE   BLD20                                                            
         L     RE,12(R1)                                                        
         USING NUCMLEL,RE                                                       
*                                                                               
         TM    NUCMLFLG,X'04'      IS THIS A BILLBOARD?                         
         BZ    BLD20                                                            
         MVC   BUYMSG(L'UNITBB),UNITBB                                          
         B     *+10                                                             
         DROP  RE                                                               
*                                                                               
BLD10    MVC   BUYMSG(L'COPSERR),COPSERR                                        
         MVI   FERN,USERERR                                                     
         B     ERROR                                                            
*                                                                               
BLD20    DS    0H                                                               
         CLI   UNTCSP,C'S'         SECTIONAL MAP?                               
         BE    BLD25                                                            
*                                                                               
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'18',AIOAREA1),0                    
         CLI   12(R1),0            TEST IF FOUND                                
         BNE   BLD25                                                            
         L     RE,12(R1)                                                        
         USING NUDTAD,RE                                                        
*                                                                               
         OC    NUDTAMYR,NUDTAMYR   WAS THIS PREVIOUSLY A SMAP?                  
         BNZ   *+14                IF YES, THEN CLEAR OUT SMAP INFO             
         OC    NUDTAMCD,NUDTAMCD   AND DEL FEEDS OFF X'03'                      
         BZ    BLD25                                                            
*                                                                               
         XC    NUDTAMYR,NUDTAMYR   WILL BE ZEROS IF NOT SMAP                    
         XC    NUDTAMCD,NUDTAMCD                                                
         OI    MYFLAG,DELFEED                                                   
         DROP  RE                                                               
*                                                                               
BLD25    DS    0H                                                               
         GOTO1 VCALLOV,DMCB,(X'35',0),ATWA                                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VEDIT,DMCB          GENERAL EDIT MODULE                          
         SPACE                                                                  
*                                                                               
         SPACE 1                                                                
         LA    R4,BLOCK            INITIALIZE EDIT BLOCK AND CALL               
         USING UNBLOCKD,R4         EDIT FOR INITIALIZATION                      
         XC    UNBLOCK,UNBLOCK                                                  
         ST    R9,UNAGLOB                                                       
         MVC   UNAREC,AIOAREA1                                                  
         LA    RE,TEMPD+1500                                                    
         ST    RE,UNALOCAL         LOCAL STORAGE TO EDIT MODULE                 
*                                                                               
         MVC   UNODATE,NBACTDAT    SET ORIGINAL RECORD VALUES                   
         MVC   UNOSUB,NBACTSUB                                                  
         MVC   UNOTIME,NBACTSQH                                                 
         MVC   UNODAY,NBDAY                                                     
         SPACE                                                                  
         GOTO1 VEDIT,DMCB,(C'I',(R4))                                           
*  EDIT LENGTH                                                                  
         LA    R2,UNTLENH                                                       
         ST    R2,UNFLDH           FIELD HEADER POINTER                         
         ST    R2,FADDR                                                         
         MVI   UNEDATA,ULEN        SET DATA TYPE                                
         LA    R1,DMCB                                                          
         GOTO1 VEDIT,(R1),(C'E',(R4))                                           
         CLI   UNERROR,0           TEST FOR ERROR                               
         BE    *+14                NONE                                         
         MVC   FERN,UNERROR                                                     
         B     ERROR                                                            
*  EDIT ACTUAL COST                                                             
         LA    R2,UNTACTH                                                       
         ST    R2,UNFLDH           FIELD HEADER POINTER                         
         ST    R2,FADDR                                                         
         MVI   UNEDATA,UACT        SET DATA TYPE                                
         LA    R1,DMCB                                                          
         GOTO1 VEDIT,(R1),(C'E',(R4))                                           
         CLI   UNERROR,0           TEST FOR ERROR                               
         BE    *+14                NONE                                         
         MVC   FERN,UNERROR                                                     
         B     ERROR                                                            
*  EDIT INTEGRATION                                                             
         LA    R2,UNTINTH                                                       
         ST    R2,UNFLDH           FIELD HEADER POINTER                         
         ST    R2,FADDR                                                         
         MVI   UNEDATA,UINT        SET DATA TYPE                                
         LA    R1,DMCB                                                          
         GOTO1 VEDIT,(R1),(C'E',(R4))                                           
         CLI   UNERROR,0           TEST FOR ERROR                               
         BE    *+14                NONE                                         
         MVC   FERN,UNERROR                                                     
         B     ERROR                                                            
*  EDIT COMMENTS                                                                
         GOTO1 VHELLO,DMCB,(C'D',UNTFILE),(X'04',AIOAREA1),0                    
         LA    R2,UNTCOM1H                                                      
         MVI   BYTE,NO             SET SECOND COMMENT LINE SWITCH               
         SPACE                                                                  
BLD30    GOTO1 VGETFLD                                                          
         L     R1,AIOAREA1                                                      
         USING NURECD,R1                                                        
*                                                                               
         TM    FLDH+4,X'80'        TEST IF FIELD SENT THIS TIME                 
         BZ    *+8                 NO                                           
         OI    NUACTWHY,X'10'      YES-NOTE COMMENT CHANGE                      
         CLI   FLDH+5,0            TEST FOR INPUT                               
         BE    BLD40               ALL DONE                                     
         DROP  R1                                                               
*                                                                               
         LA    R3,BLOCK+L'UNBLOCK                                               
         USING NUCOMD,R3                                                        
         MVI   NUCOMEL,X'04'                                                    
         ZIC   R1,FLDH+5                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   NUCOMMNT(0),FLD                                                  
         LA    R1,NUCOMMNT-NUCOMEL+1(R1)     DEVELOP EL LENGTH                  
         STC   R1,NUCOMLEN                                                      
         MVI   NUCOMTYP,C'C'                                                    
         MVI   NUCOMLIN,1                                                       
         CLI   BYTE,YES            TEST FOR SECOND LINE                         
         BNE   *+8                                                              
         MVI   NUCOMLIN,2                                                       
         GOTO1 VHELLO,DMCB,(C'P',UNTFILE),AIOAREA1,(R3),0                       
         CLI   12(R1),0            TEST FOR ERROR                               
         BE    BLD35               NO                                           
         MVI   FERN,TOOLARGE                                                    
         CLI   12(R1),5                                                         
         BE    ERROR                                                            
         DC    H'0'                                                             
         SPACE                                                                  
BLD35    CLI   BYTE,YES            TEST IF SECOND LINE DONE                     
         BE    BLD40                                                            
         LA    R2,UNTCOM2H         POINT TO SECOND LINE                         
         MVI   BYTE,YES            SET SWITCH                                   
         B     BLD30                                                            
*                                                                               
*--SAVE TRAFFIC ELEMENT STATUS BIT                                              
*                                                                               
BLD40    MVI   TRAFFLAG,0          INIT TRAFFIC FLAG                            
         USING NUCMLEL,RE                                                       
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'21',AIOAREA1),0                    
         CLI   12(R1),0            TEST IF FOUND                                
         BE    BLD50                                                            
*--BUILD 21 ELEMENT                                                             
         XC    SCANTAB(60),SCANTAB                                              
         LA    RE,SCANTAB                                                       
         MVI   NUCMLEID,X'21'                                                   
         MVI   NUCMLELN,X'50'                                                   
         GOTO1 VHELLO,DMCB,(C'P',UNTFILE),AIOAREA1,SCANTAB,0                    
         B     BLD60                                                            
*                                                                               
BLD50    L     RE,12(R1)                                                        
         MVC   TRAFFLAG,NUCMLFLG   SAVE TRAFFIC FLAG                            
         DROP  RE                                                               
*                                                                               
BLD60    DS    0H                  CHECK OPTIONS                                
         XC    MAPYEAR,MAPYEAR                                                  
         XC    MAPCODE,MAPCODE                                                  
*                                                                               
         LA    R2,UNTOPTH                                                       
         ST    R2,FADDR                                                         
*                                                                               
         MVI   FERN,MISERR                                                      
         CLI   5(R2),0                                                          
         BNE   *+16                                                             
         CLI   UNTCSP,C'S'         SECTIONAL MAP?                               
         BE    ERROR                                                            
         B     BLD69                                                            
         OI    MYFLAG,SMAP                                                      
*                                                                               
         MVI   FERN,INVERR                                                      
         GOTO1 VSCANNER,DMCB,(X'0B',UNTOPTH),WORK                               
         CLI   4(R1),0                                                          
         BE    ERROR                                                            
*                                                                               
         CLI   WORK+12,C'S'        OPTION: S=YY/CCCCCCCC?                       
         BNE   ERROR               SECTIONAL MAP?                               
         CLI   WORK+24,C'/'                                                     
         BNE   ERROR                                                            
*                                                                               
         CLI   UNTCSP,C'S'         MUST BE SECTIONAL MAP                        
         BNE   ERROR                                                            
*                                                                               
         XC    DUB,DUB                                                          
         MVC   DUB(8),=C'20  0101'                                              
         MVC   DUB+2(2),WORK+22                                                 
         GOTO1 VDATCON,DMCB,(9,DUB),(3,WORK+40)                                 
         MVC   MAPYEAR,WORK+40                                                  
*                                                                               
         MVC   MAPCODE,WORK+25                                                  
         OC    MAPCODE,SPACES                                                   
*                                                                               
         XC    KEY,KEY                                                          
         LA    RF,KEY                                                           
         USING SMAPRECD,RF                                                      
*                                                                               
         MVC   SMKTYPE,=X'0D3D'                                                 
         MVC   SMKAGY,AGYMED                                                    
         MVC   SMKYEAR,MAPYEAR     MAP YEAR                                     
         MVC   SMKCODE,MAPCODE     MAP CODE                                     
         DROP  RF                                                               
*                                                                               
         GOTO1 AIO,DMCB2,SPT+DIR+HIGH                                           
         CLC   KEY(13),KEYSAVE                                                  
         BNE   ERROR                                                            
*                                                                               
         GOTO1 AIO,DMCB,SPT+FILE+GET,AIOAREA4                                   
*                                                                               
         NI    MYFLAG,X'FF'-DELFEED    DELETE FEEDS OFF X'03' ELEMENTS          
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'18',AIOAREA1),0                    
         CLI   12(R1),0            TEST IF FOUND                                
         BNE   BLD60A                                                           
         L     R5,12(R1)                                                        
         USING NUDTAD,R5                                                        
*                                                                               
         CLC   NUDTAMYR,MAPYEAR                                                 
         BNE   BLD60A                                                           
         CLC   NUDTAMCD,MAPCODE                                                 
         BE    BLD60B                                                           
         DROP  R5                                                               
*                                                                               
BLD60A   OI    MYFLAG,DELFEED                                                   
*                                                                               
BLD60B   DS    0H                                                               
         LA    R2,UNTPRD1H                                                      
         LA    R5,6                                                             
*                                                                               
         TM    UNTOPTH+4,X'80'     USER CHANGED OPTIONS FIELD?                  
         BZ    BLD63                                                            
*                                                                               
BLD61    DS    0H                  CLEAR FIELDS                                 
         XC    8(L'UNTPRD1,R2),8(R2)                                            
         MVI   5(R2),0                                                          
         OI    6(R2),X'80'                                                      
*                                                                               
         BAS   RE,NEXTFLD                                                       
         XC    8(L'UNTPCT1,R2),8(R2)                                            
         OI    6(R2),X'80'                                                      
*                                                                               
         BAS   RE,NEXTFLD                                                       
         XC    8(L'UNTFED1,R2),8(R2)                                            
         OI    6(R2),X'80'                                                      
*                                                                               
         BAS   RE,NEXTFLD                                                       
         XC    8(L'UNTTFD1,R2),8(R2)                                            
         OI    6(R2),X'80'                                                      
*                                                                               
         BAS   RE,NEXTFLD                                                       
         BCT   R5,BLD61                                                         
*                                                                               
BLD63    DS    0H                                                               
         LA    R2,UNTPRD1H                                                      
         LA    R5,6                                                             
*                                                                               
         NI    MYFLAG,X'FF'-NOFDES                                              
*                                                                               
         L     RF,AIOAREA4                                                      
         USING SMAPRECD,RF                                                      
         LA    R3,SMELEM                                                        
         USING SMREGD,R3                                                        
         DROP  RF                                                               
*                                                                               
BLD65    DS    0H                                                               
         CLI   0(R3),X'01'         ANY MORE REGIONS?                            
         BNE   BLD69                                                            
*&&DO                                                                           
         XC    8(L'UNTPRD1,R2),8(R2)                                            
         MVI   5(R2),0                                                          
         OI    6(R2),X'80'                                                      
*&&                                                                             
         BAS   RE,NEXTFLD                                                       
         XC    8(L'UNTPCT1,R2),8(R2)                                            
*                                                                               
         GOTO1 DISFEED,DMCB,SMREPCT,8(R2)                                       
         OI    1(R2),X'20'         PROTECT THIS FIELD                           
         OI    6(R2),X'80'                                                      
*                                                                               
         BAS   RE,NEXTFLD                                                       
*&&DO                                                                           
         XC    8(L'UNTFED1,R2),8(R2)                                            
         OI    6(R2),X'80'                                                      
*&&                                                                             
         BAS   RE,NEXTFLD                                                       
         XC    8(L'UNTTFD1,R2),8(R2)                                            
         OI    1(R2),X'20'         PROTECT THIS FIELD                           
         OI    6(R2),X'80'                                                      
*                                                                               
         MVC   8(2,R2),SMREREG                                                  
*                                                                               
         MVC   SVFEED,SMREREG                                                   
         BRAS  RE,CHKFEEDR         FEED RECORDS SET UP?                         
         TM    MYFLAG,NOFDES       DO ALL FEEDS HAVE RECORDS?                   
         BO    NOFDESC             NO                                           
*                                                                               
         OC    SVFDESC,SVFDESC                                                  
         BZ    *+10                                                             
         MVC   12(30,R2),SVFDESC   FEED DESCRIPTION                             
*                                                                               
         BAS   RE,NEXTFLD                                                       
*                                                                               
         ZIC   RF,1(R3)                                                         
         AR    R3,RF                                                            
         BCT   R5,BLD65                                                         
*                                                                               
BLD69    DS    0H                                                               
         MVI   SVPRODTB,X'FF'      INIT TABLE                                   
*                                                                               
         LA    R2,UNTPRD1H                                                      
         LA    RF,SVPRODTB                                                      
         LA    R5,6                                                             
*                                                                               
BLD69A   CLI   5(R2),0                                                          
         BE    BLD70                                                            
*                                                                               
         MVC   5(3,RF),8(R2)       FILL IN TABLE WITH ALPHA PRODUCTS            
         OC    5(3,RF),SPACES      FROM SCREEN                                  
*                                                                               
         BAS   RE,NEXTFLD                                                       
         BAS   RE,NEXTFLD                                                       
         BAS   RE,NEXTFLD                                                       
         BAS   RE,NEXTFLD                                                       
*                                                                               
         LA    RF,SVPRODLN(RF)                                                  
         MVI   0(RF),X'FF'                                                      
         BCT   R5,BLD69A                                                        
*                                                                               
BLD70    LA    R2,UNTPRD1H         BEGIN EDIT AT PRODUCT 1                      
         MVI   PRODINP,C'Y'        SET PROD INPUT TO YES                        
         LA    R5,6                6 LINES OF DATA TO CHECK                     
BLD75    BAS   RE,CLRBUY           CLEAR THE BUY FIELDS                         
         LA    R3,EDLIST           POINT R3 AT EDIT LIST                        
         LA    R1,DMCB                                                          
         SPACE                                                                  
BLD80    CLI   0(R3),X'FF'         TEST FOR E-O-L                               
         BE    BLD130                                                           
         CLI   0(R3),UPRD          ARE WE VALIDATING PRODUCT                    
         BNE   BLD90               NO BYPASS INPUT CHECK                        
         STCM  R2,15,SVSCRPRD      SAVE ADDRESS OF PRODUCT FIELD                
         CLI   5(R2),0             WAS PRODUCT INPUTTED                         
         BNE   BLD100              YES CONTINE                                  
         MVI   PRODINP,C'N'        NO SET SWITCH                                
         B     BLD100                                                           
*                                                                               
BLD90    CLI   PRODINP,C'Y'        WAS PRODUCT INPUTTED                         
         BE    BLD100              YES, CONTINUE                                
         CLI   8(R2),0             IS DATA IN THIS FIELD                        
         BNE   BLD250              YES ERROR                                    
BLD100   ST    R2,UNFLDH           FIELD HEADER POINTER                         
         ST    R2,FADDR                                                         
         MVC   UNEDATA,0(R3)       SET DATA TYPE                                
*                                                                               
         LA    RF,UNTPCT1H         CHECK IF THIS IS THE 1ST PROD.               
         CR    R2,RF                                                            
         BNE   BLD110                                                           
*                                                                               
         CLI   8(R2),C'0'          USER FORCES 1ST PROD TO BE 0?                
         BE    BLD105                                                           
         CLC   =C'.00',8(R2)                                                    
         BNE   BLD110                                                           
*                                                                               
BLD105   L     RF,AIOAREA1         POINT TO UNIT RECORD                         
         USING NURECD,RF                                                        
         OI    NUUNST2,X'04'       1ST PROD IS 0                                
         OI    MYFLAG,FORCZERO                                                  
         XC    NUP1SHR,NUP1SHR                                                  
         DROP  RF                                                               
*                                                                               
BLD110   DS    0H                                                               
         GOTO1 VEDIT,(R1),(C'E',(R4))                                           
         CLI   UNERROR,0           TEST FOR ERROR                               
         BE    *+14                NONE                                         
         MVC   FERN,UNERROR                                                     
         B     ERROR                                                            
*                                                                               
         LA    R3,1(R3)                                                         
         CLI   UNTCSP,C'S'         SECTIONAL MAP?                               
         BNE   *+12                                                             
         BAS   RE,NEXTFLD                                                       
         B     *+8                                                              
*                                                                               
         BAS   RE,BUMPFLD          FIND NEXT UNPROTECTED FIELD                  
         B     BLD80                                                            
         SPACE                                                                  
BLD130   LA    RF,SVPRODTB                                                      
         LA    RE,6                                                             
         L     R1,AIOAREA1                                                      
         USING NURECD,R1                                                        
*                                                                               
BLD140   CLI   0(RF),X'FF'                                                      
         BE    BLD150                                                           
*          CLC   0(1,RF),NUPRD       CHECK FOR DUPS                             
*          BE    DUPERR                                                         
         LA    RF,SVPRODLN(RF)                                                  
         BCT   RE,BLD140                                                        
         DC    H'0'                                                             
BLD150   MVC   0(1,RF),NUPRD                                                    
         MVC   1(2,RF),NUP1SHR                                                  
         MVC   3(2,RF),NUFEED                                                   
         OC    0(SVPRODLN,RF),0(RF)  ANYTHING OUTPUTTED                         
         BNZ   *+12                                                             
         MVI   0(RF),X'FF'                                                      
         B     *+8                                                              
         MVI   SVPRODLN(RF),X'FF'                                               
         PRINT GEN                                                              
         CLI   UNTCSP,C'T'                                                      
         BNE   BLD180                                                           
         CLI   5(R2),0                                                          
         BE    BLD180                                                           
         CLI   5(R2),2                                                          
         BH    BLD160                                                           
         CLC   8(2,R2),=CL2'*N'    NATIONAL IS GOOD                             
         BE    BLD180                                                           
         CLC   8(2,R2),=CL2'**'    NO NATIONAL IS GOOD                          
         BE    BLD180                                                           
BLD160   ST    R2,FADDR                                                         
         MVC   BUYMSG(L'NOFDSALW),NOFDSALW                                      
         MVI   FERN,USERERR                                                     
         B     ERROR                                                            
BLD180   GOTO1 EDITFEED,DMCB,FEEDEDA,0(RF),0(R2)   BUILD FEED TABLE             
         PRINT NOGEN                                                            
         ZIC   RF,FEEDPRNM                                                      
         LA    RF,1(RF)                                                         
         STCM  RF,1,FEEDPRNM                                                    
*                                                                               
         BAS   RE,BUMPFLD          FIND NEXT UNPROTECTED FIELD                  
         BCT   R5,BLD75                                                         
         BAS   RE,CLRBUY                                                        
         DROP  R1                                                               
*                                                                               
         LA    RE,SVPRODTB                                                      
         LA    RF,6                                                             
         SR    R1,R1                                                            
BLD200   CLI   0(RE),X'FF'                                                      
         BE    BLD230                                                           
         OC    0(SVPRODLN,RE),0(RE)                                             
         BZ    BLD220                                                           
         LA    R1,1(R1)                                                         
         LA    RE,SVPRODLN(RE)                                                  
BLD220   BCT   RF,BLD200                                                        
*                                                                               
BLD230   STCM  R1,1,NUMPRD                                                      
*                                                                               
         LA    RE,SVPRODTB+1                                                    
         LA    R2,UNTPCT1H                                                      
         ST    R2,FADDR                                                         
         PRINT GEN                                                              
         GOTO1 CHKPCT,DMCB,(RE)                                                 
         PRINT NOGEN                                                            
         LA    RE,SVPRODTB+3                                                    
         LA    R2,UNTFED1H                                                      
         ST    R2,FADDR                                                         
         OI    MYFLAG,VALFEED                                                   
         GOTO1 CHKPCT,DMCB,(RE)                                                 
         NI    MYFLAG,X'FF'-VALFEED                                             
*                                                                               
         CLI   UNTCSP,C'T'         IF TRIGGY DONT SET COPY SPLIT BIT            
         BE    BLD240                                                           
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'02',AIOAREA1),0                    
         CLI   12(R1),0            TEST IF FOUND                                
         BE    *+6                                                              
         DC    H'0'                TAKE A HIT IF BAD RETURN CODE                
         L     R5,12(R1)                                                        
         USING NUSDRD,R5                                                        
         OI    NUSDST3,X'40'       TURN ON COPY SPLT BIT                        
         MVC   SVSTAT3,NUSDST3                                                  
*                                                                               
BLD240   GOTO1 VHELLO,DMCB,(C'D',UNTFILE),(X'14',AIOAREA1),0                    
*                                                                               
         CLI   UNTCSP,C'T'         IS THIS A TRIGGY?                            
         BNE   BLD245                                                           
         CLI   NUMPRD,3                                                         
         BE    BLD400                                                           
*--ERROR MUST HAVE 3 PRODS                                                      
         LA    R2,UNTPRD1H                                                      
         ST    R2,FADDR                                                         
         MVC   BUYMSG(L'PRD3ERR),PRD3ERR                                        
         MVI   FERN,USERERR                                                     
         B     ERROR                                                            
*                                                                               
BLD245   CLI   NUMPRD,2                                                         
         BH    BLD400                                                           
         BL    BLD246                                                           
*                                                                               
         CLI   UNTCSP,C'S'         SECTIONAL?                                   
         BE    BLD400                                                           
         B     BLD280                                                           
*                                                                               
*--ERROR NOT ENOUGH PRODS                                                       
BLD246   DS    0H                                                               
         LA    R2,UNTPRD1H                                                      
         ST    R2,FADDR                                                         
         MVC   BUYMSG(L'PRD2ERR),PRD2ERR                                        
         MVI   FERN,USERERR                                                     
         B     ERROR                                                            
*                                                                               
*--ERROR INFO WITH NO PRODUCT                                                   
BLD250   ST    R2,FADDR                                                         
         MVC   BUYMSG(L'NPRDERR),NPRDERR                                        
         MVI   FERN,USERERR                                                     
         B     ERROR                                                            
*                                                                               
*--FILL IN THE BUY FIELD                                                        
BLD280   L     R3,AIOAREA1                                                      
         USING NURECD,R3                                                        
*                                                                               
         MVI   NUPRD,0                                                          
         MVI   NUPRD2,0                                                         
         XC    NUP1SHR,NUP1SHR                                                  
         XC    NUFEED,NUFEED                                                    
         OC    SVPRODTB,SVPRODTB                                                
         BZ    BLD500                                                           
         LA    R4,SVPRODTB                                                      
         LA    R5,6                                                             
BLD300   DS    0H                                                               
*&&DO                                                                           
         CLI   0(R4),0                                                          
         BE    BLD320                                                           
         CLI   NUPRD,0                                                          
         BNE   BLD340                                                           
*&&                                                                             
         CLI   5(R4),0                                                          
         BE    BLD320                                                           
         CHI   R5,5                                                             
         BE    BLD340                                                           
         MVC   NUPRD,0(R4)                                                      
         MVC   NUP1SHR,1(R4)                                                    
         MVC   NUFEED,3(R4)                                                     
BLD320   LA    R4,SVPRODLN(R4)                                                  
         BCT   R5,BLD300                                                        
         DC    H'0'                                                             
BLD340   MVC   NUPRD2,0(R4)        MOVE SECOND PRODUCT OUT                      
         B     BLD500                                                           
         DROP  R3                                                               
*                                                                               
BLD400   XC    WORK(40),WORK                                                    
         MVI   WORK,X'14'                                                       
         CLI   UNTCSP,C'T'         CHECK FOR TRIGGY                             
         BNE   *+8                                                              
         OI    WORK+2,X'80'        SET TRIGGY ON                                
         CLI   UNTCSP,C'S'         CHECK FOR SECTIONAL MAP                      
         BNE   *+8                                                              
         OI    WORK+2,X'40'        SET SECTIONAL MAP ON                         
*                                                                               
         LA    RE,3                                                             
*                                                                               
         LA    R3,WORK+3                                                        
         LA    R4,SVPRODTB                                                      
         LA    R5,6                                                             
BLD420   OC    0(SVPRODLN,R4),0(R4)          BLANK ENTRY                        
         BZ    BLD440                                                           
         CLI   0(R4),X'FF'                   END OF TABLE                       
         BZ    BLD460                                                           
         MVC   0(1,R3),0(R4)                                                    
         MVC   2(2,R3),1(R4)                                                    
         MVC   4(2,R3),3(R4)                                                    
         LA    R3,6(R3)                                                         
         LA    RE,6(RE)                                                         
BLD440   LA    R4,SVPRODLN(R4)                                                  
         BCT   R5,BLD420                                                        
BLD460   STCM  RE,1,WORK+1         LENGTH OF ELEMENT                            
         GOTO1 VHELLO,DMCB,(C'P',UNTFILE),AIOAREA1,WORK,0                       
*                                                                               
BLD500   CLI   UNTCSP,C'P'         IF PIGGY DONT BUILD FEEDS                    
         BE    BLD520                                                           
         BAS   RE,BLDFEED          BUILD THE FEED RECORDS                       
*                                                                               
*--SET UPDATED FLAG BIT IN 21 ELEMENT                                           
BLD520   GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'21',AIOAREA1),0                    
         CLI   12(R1),0            TEST IF FOUND                                
         BE    *+6                                                              
         DC    H'0'                TAKE A HIT IF BAD RETURN CODE                
         L     RE,12(R1)                                                        
         USING NUCMLEL,RE                                                       
         MVC   NUCMLFLG,TRAFFLAG                                                
*                                                                               
         TM    MYFLAG,FORCZERO                                                  
         BZ    BLD550                                                           
         L     RF,AIOAREA1         POINT TO UNIT RECORD                         
         USING NURECD,RF                                                        
         OI    NUUNST2,X'04'       1ST PROD IS 0                                
         XC    NUP1SHR,NUP1SHR                                                  
         B     BLD550                                                           
         DROP  RE,RF                                                            
*                                                                               
*-- UPDATED UNIT W/ MAP YEAR/CODE                                               
BLD550   DS    0H                                                               
         CLI   UNTCSP,C'S'         SECTIONAL MAP?                               
         BNE   BLD560                                                           
*                                                                               
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'18',AIOAREA1),0                    
         CLI   12(R1),0            TEST IF FOUND                                
         BNE   BLD555                                                           
         L     RE,12(R1)                                                        
         USING NUDTAD,RE                                                        
*                                                                               
         MVC   NUDTAMYR,MAPYEAR    WILL BE ZEROS IF NOT SMAP                    
         MVC   NUDTAMCD,MAPCODE                                                 
         B     BLD560                                                           
*                                                                               
BLD555   DS    0H                                                               
         LA    RE,WORK                                                          
         XC    WORK,WORK                                                        
         MVI   WORK,X'18'                                                       
         MVI   WORK+1,NUDTELEN                                                  
*                                                                               
         MVC   NUDTAMYR,MAPYEAR                                                 
         MVC   NUDTAMCD,MAPCODE                                                 
*                                                                               
         GOTO1 VHELLO,DMCB,(C'P',UNTFILE),AIOAREA1,WORK,0                       
         DROP  RE                                                               
*                                                                               
BLD560   DS    0H                                                               
         TM    MYFLAG,DELFEED      DEL FEEDS OFF X'03' RECORD                   
         BZ    BLD570                                                           
         BAS   RE,UPD03                                                         
*                                                                               
BLD570   DS    0H                                                               
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'19',AIOAREA1),0                    
         CLI   12(R1),0            TEST IF FOUND                                
         BNE   BLD580                                                           
         GOTO1 VHELLO,DMCB,(C'D',UNTFILE),(X'19',AIOAREA1),0                    
*                                                                               
BLD580   DS    0H                                                               
         LA    R3,WORK                                                          
         USING NUPDED,R3                                                        
         XC    WORK,WORK                                                        
         MVI   WORK,X'19'                                                       
         MVI   WORK+1,X'03'         DEFAULT LENGTH TO 3                         
*                                                                               
         CLI   UNTCSP,C'T'         CHECK FOR TRIGGY                             
         BNE   *+8                                                              
         OI    WORK+2,X'80'        SET TRIGGY ON                                
         CLI   UNTCSP,C'S'         CHECK FOR SECTIONAL MAP                      
         BNE   *+8                                                              
         OI    WORK+2,X'20'        SET SECTIONAL MAP ON                         
*                                                                               
         TM    SVSTAT3,X'40'       COPY SPLIT?                                  
         BZ    *+8                                                              
         OI    WORK+2,X'40'                                                     
*                                                                               
         LA    RF,SVPRODTB                                                      
         CLI   5(RF),0                                                          
         BE    BLD650                                                           
         AHI   R3,3                                                             
*                                                                               
BLD590   DS    0H                                                               
         CLI   0(RF),X'FF'                                                      
         BE    BLD600                                                           
*                                                                               
         MVC   0(3,R3),5(RF)        PRODUCT ALPHA                               
         MVC   3(2,R3),1(RF)        PRODUCT %                                   
         MVC   5(2,R3),3(RF)        FEED %                                      
*                                                                               
         ZIC   R6,WORK+1            INCREASE LENGTH ON ELEMENT                  
         AHI   R6,7                                                             
         STC   R6,WORK+1                                                        
*                                                                               
         AHI   R3,7                 BUMP POINTER IN ELEMENT                     
         AHI   RF,SVPRODLN          BUMP TO NEXT PRODUCT IN TABLE               
*                                                                               
         B     BLD590                                                           
*                                                                               
*  CHECK IF 19 ELEMENT SHOULD BE CREATED (COMMENTED OUT)                        
*                                                                               
****BLD600   LA    RE,OVAGYTAB                                                  
****         L     RF,ABUYVALS                                                  
****         USING BUYVALD,RF                                                   
****BLD610   CLC   0(2,RE),AGYALPH                                              
****         BNE   BLD620                                                       
****         OC    2(2,RE),2(RE)        IS THERE A CLIENT RESTRICTION           
****         BZ    BLD650                                                       
****         CLC   2(2,RE),CLIPK                                                
****         BE    BLD650                                                       
****BLD620   LA    RE,4(RE)                                                     
****         CLI   0(RE),0                                                      
****         BE    EXXMOD                                                       
****         B     BLD610                                                       
*                                                                               
BLD600   DS    0H                                                               
         GOTO1 VHELLO,DMCB,(C'P',UNTFILE),(X'19',AIOAREA1),WORK,0               
*                                                                               
BLD650   DS    0H                                                               
         BRAS  RE,UPDACT                                                        
*                                                                               
BLDEX    B     EXXMOD                                                           
         DROP  RF                                                               
*                                                                               
*--DUIPLICATE PRODUCTS                                                          
DUPERR   MVC   BUYMSG(L'DUPRERR),DUPRERR                                        
         MVC   FADDR,SVSCRPRD                                                   
         MVI   FERN,USERERR                                                     
         B     ERROR                                                            
*                                                                               
*  OVAGYTAB  BYTE 1   AGENCY                                                    
*  OVAGYTAB  BYTE 2-3 CLIENT IF RESTRICTED                                      
OVAGYTAB DC    CL2'SJ',XL2'0000'     SJR/BGM                                    
         DC    CL2'*B',XL2'0000'     DDSB                                       
         DC    CL2'*1',XL2'0000'     DDS1                                       
         DC    CL2'DR',XL2'BCDE'     SMGTEST/PG5                                
         DC    CL2'DU',XL2'BCDE'     MVNYN/PG5                                  
         DC    X'00'                                                            
         EJECT                                                                  
*******************************************************************             
* USER CHANGE - DELETE FEEDS OFF EXISTING SPECIAL CHARGES                       
*******************************************************************             
UPD03    NTR1                                                                   
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'03',AIOAREA1),0                    
         CLI   12(R1),0            TEST IF FOUND                                
         BNE   UPD03X                                                           
         L     R3,12(R1)                                                        
         USING NUSPRD,R3                                                        
*                                                                               
UPD0310  DS    0H                                                               
         CLI   0(R3),X'03'                                                      
         BNE   UPD03X                                                           
*                                                                               
         CLI   NUSPRLEN,NUSPRLN3   ANY FEED?                                    
         BNH   UPD0330                                                          
*                                                                               
         ZIC   R1,NUSPRLEN                                                      
         SHI   R1,NUSPRLN3         SUBTRACT OFF A(NUSPRFED)-NUSPRD              
         BCTR  R1,0                                                             
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    NUSPRFED(0),NUSPRFED                                             
*                                                                               
UPD0330  ZIC   RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     UPD0310                                                          
*                                                                               
UPD03X   DS    0H                                                               
         B     EXXMOD                                                           
         DROP  R3                                                               
*                                                                               
* MAKE SURE PCTS DO NOT EXCEED 100                                              
*                                                                               
CHKPCT   NTR1                                                                   
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         LA    R3,6                                                             
         L     R4,0(R1)                                                         
         ST    R4,SAVER4                                                        
CHKPC20  ICM   RF,3,0(R4)                                                       
         AR    RE,RF                                                            
         LA    R4,SVPRODLN(R4)                                                  
         BCT   R3,CHKPC20                                                       
         LTR   RE,RE                                                            
         BZ    CHKPC100                                                         
*- PCT MUST ADD UP TO 100                                                       
         C     RE,=F'10000'                                                     
         BE    CHKPCEX                                                          
         MVC   BUYMSG(L'PCTFERR),PCTFERR                                        
         MVI   FERN,USERERR                                                     
         B     ERROR                                                            
*- IF NO PCT INPUTTED CALULATE THE PERCENT                                      
*- AND FILL IN THE PRODUCT TABLE                                                
CHKPC100 CLI   NUMPRD,0                                                         
         BE    CHKPCEX                                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         A     RF,=F'10000'                                                     
         ZIC   R3,NUMPRD                                                        
         DR    RE,R3                                                            
*-FILL IN THE TABLE                                                             
         TM    MYFLAG,FORCZERO     FORCE 1ST PRODUCT WITH 0%                    
         BO    CHKPC180                                                         
*                                                                               
         LA    R5,SVPRODTB                                                      
         LA    R3,6                                                             
         L     R4,SAVER4                                                        
CHKPC140 OC    0(SVPRODLN,R5),0(R5)                                             
         BZ    CHKPC160                                                         
         SR    R1,R1                                                            
         ICM   R1,3,0(R4)                                                       
         AR    R1,RE               ADD REMAINDER TO FIRST ENTRY                 
         AR    R1,RF               ADD QUOTIENT                                 
         SR    RE,RE               CLEAR REMAINDER                              
         STCM  R1,3,0(R4)                                                       
CHKPC160 LA    R4,SVPRODLN(R4)                                                  
         BCT   R3,CHKPC140                                                      
         B     CHKPCEX                                                          
*                                                                               
CHKPC180 DS    0H                                                               
         CLI   NUMPRD,2                                                         
         BNE   CHKPC200                                                         
*                                                                               
         LA    RF,SVPRODTB                                                      
         XC    1(2,RF),1(RF)       MAKE 1ST PRODUCT 0%                          
         LA    RF,SVPRODLN(RF)                                                  
         MVC   1(2,RF),=XL2'2710'  AND MAKE THE 2ND PRODUCT 100%                
*                                                                               
CHKPC200 DS    0H                                                               
         TM    MYFLAG,VALFEED                                                   
         BO    CHKPCEX                                                          
*                                                                               
* FIRST PRODUCT HAS 0 INPUT - MUST RECALCULATE TO MAKE SURE                     
* SUM IS 100%                                                                   
*                                                                               
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         LA    R3,6                                                             
         LA    R4,UNTPCT1H                                                      
         ST    R4,SAVER4                                                        
CHKPC210 ICM   RF,3,0(R4)                                                       
         AR    RE,RF                                                            
         LA    R4,SVPRODLN(R4)                                                  
         BCT   R3,CHKPC210                                                      
*- PCT MUST ADD UP TO 100                                                       
         C     RE,=F'10000'                                                     
         BE    CHKPCEX                                                          
         MVC   BUYMSG(L'PCTFERR),PCTFERR                                        
         MVI   FERN,USERERR                                                     
         B     ERROR                                                            
*                                                                               
CHKPCEX  B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
* CHECK IF FEED RECORD EXISTS FOR SMAP FEEDS                                    
*                                                                               
CHKFEEDR NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    RF,KEY                                                           
         USING FEEDRECD,RF                                                      
         L     RE,NBAIO                                                         
         USING NURECD,RE                                                        
*                                                                               
         MVC   FEEDKID,=X'0A2B'                                                 
         MVC   FEEDKAM,NUKAM       AGY/MEDIA                                    
         MVC   FEEDKNET,NUKNET     NETWORK                                      
         MVC   FEEDKCLT,NUKCLT     CLIENT                                       
         MVC   FEEDKFD(2),SVFEED      FEED                                      
         OC    FEEDKFD,SPACES                                                   
         DROP  RE,RF                                                            
*                                                                               
         XC    SVFDESC,SVFDESC                                                  
*                                                                               
         GOTO1 AIO,DMCB2,SPT+DIR+HIGH                                           
         CLC   KEY(13),KEYSAVE                                                  
         BNE   NOFDESC                                                          
*                                                                               
         GOTO1 AIO,DMCB,SPT+FILE+GET,AIOAREA3                                   
*                                                                               
         GOTO1 VHELLO,DMCB,(C'G',SPTFILE),(X'40',AIOAREA3),0                    
         CLI   12(R1),0            TEST IF FOUND                                
         BNE   CHKF10                                                           
         L     R3,12(R1)                                                        
         USING FEEDSMEL,R3                                                      
*                                                                               
         TM    FEEDSFLG,FEEDSDEL                                                
         BO    CHKF30                                                           
*                                                                               
CHKF10   DS    0H                                                               
         CLI   0(R3),X'40'                                                      
         BNE   CHKF30                                                           
*                                                                               
         CLC   FEEDSMYR,MAPYEAR                                                 
         BNE   *+14                                                             
         CLC   FEEDSMCD,MAPCODE                                                 
         BE    CHKF20                                                           
*                                                                               
         ZIC   RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     CHKF10                                                           
*                                                                               
CHKF20   MVC   SVFDESC,FEEDSMDS  DESCRIPTION                                    
         B     CHKFX                                                            
         DROP  R3                                                               
*                                                                               
CHKF30   DS    0H                                                               
         OI    MYFLAG,NOFDES                                                    
*                                                                               
CHKFX    DS    0H                                                               
         B     EXXMOD                                                           
*                                                                               
NOFDESC  DS    0H                  INVALID FEED CODE                            
         MVC   BUYMSG(L'NOFDESCQ),NOFDESCQ                                      
         MVC   BUYMSG+L'NOFDESCQ+1(2),SVFEED                                    
         MVI   FERN,USERERR                                                     
         B     ERROR                                                            
*                                                                               
* ASSIGNED COST                                                                 
*                                                                               
CLRBUY   NTR1                                                                   
         L     R4,AIOAREA1                                                      
         USING NURECD,R4                                                        
*                                                                               
         MVI   NUPRD,0                                                          
         MVI   NUPRD2,0                                                         
         XC    NUP1SHR,NUP1SHR                                                  
         XC    NUFEED,NUFEED                                                    
*                                                                               
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'02',AIOAREA1),0                    
         CLI   12(R1),0            TEST IF FOUND                                
         BE    *+6                                                              
         DC    H'0'                TAKE A HIT IF BAD RETURN CODE                
         L     R5,12(R1)                                                        
         USING NUSDRD,R5                                                        
         NI    NUSDST3,X'BF'       TURN OFF COPY SPLT BIT                       
         MVC   SVSTAT3,NUSDST3                                                  
*                                                                               
         B     EXXMOD                                                           
         DROP  R4,R5                                                            
         EJECT                                                                  
* BUILD FEED TABLE CHECK FOR DUPS                                               
*                                                                               
EDITFEED NTR1                                                                   
         L     R3,0(R1)            TABLE                                        
         ST    R3,SAVER3           SAVE REGISTER 3                              
         L     R4,4(R1)            CURRENT PRODUCT                              
         L     R2,8(R1)                                                         
*--CHECK IF SCREEN LENGTH INPUTTED                                              
         CLI   5(R2),0                                                          
         BNE   EDTF80                                                           
         LA    RE,35               MAXIMUM SCREEN LENGTH                        
         LA    RF,8(R2)            START OF INPUT                               
         SR    R1,R1               COUNTER                                      
EDTF20   CLI   0(RF),0                                                          
         BE    EDTF40                                                           
         LA    R1,1(R1)                                                         
         LA    RF,1(RF)                                                         
         BCT   RE,EDTF20                                                        
EDTF40   STCM  R1,1,5(R2)          FILL IN SCREEN LENGTH                        
*                                                                               
EDTF80   XC    SCANTAB,SCANTAB                                                  
         GOTO1 VSCANNER,DMCB,0(R2),(6,SCANTAB)                                  
         CLI   4(R1),0                                                          
         BE    EDTFEX                                                           
*--IF FIRST TIME MOVE NATIONAL IN                                               
         CLI   0(R3),X'FF'                                                      
         BNE   EDTF85                                                           
         MVC   0(1,R3),0(R4)       CURRENT PRODUCT                              
         MVC   1(1,R3),FEEDPRNM    PRODUCT NUMBER                               
         MVC   2(4,R3),=CL4'*N  '  NATIONAL                                     
         MVC   6(3,R3),5(R4)       PRODUCT ALPHA                                
         MVI   FEEDEDLN(R3),X'FF'                                               
*                                                                               
EDTF85   LA    RE,SCANTAB                                                       
EDTF90   LA    RF,6                                                             
*                                                                               
EDTF100  CLI   0(RE),0             ANY ENTRY                                    
         BE    EDTFEX              NO EXIT                                      
         CLC   12(2,RE),=CL2'*N'   CHECK FOR NATIONAL                           
         BE    EDTF260             BYPASS                                       
         CLC   12(2,RE),=CL2'**'   CHECK FOR NO NATIONAL                        
         BE    EDTF120             BYPASS                                       
         CLI   0(R3),X'FF'         CLEAR SLOT                                   
         BE    EDTF200                                                          
         CLC   2(4,R3),12(RE)      CHECK FOR DUPLICATE                          
         BNE   EDTF140                                                          
         ST    R2,FADDR                                                         
         MVI   FERN,DUPFEDC                                                     
         B     ERROR                                                            
*                                                                               
EDTF120  L     R3,SAVER3                                                        
         MVC   2(4,R3),=CL4'**  '  NO NATIONAL                                  
         B     EDTF260             GET NEXT                                     
*                                                                               
EDTF140  LA    R3,FEEDEDLN(R3)                                                  
         BCT   RF,EDTF100                                                       
         ST    R2,FADDR                                                         
         MVC   BUYMSG(L'MAXFEED),MAXFEED                                        
         MVI   FERN,USERERR                                                     
         B     ERROR                                                            
*                                                                               
EDTF200  MVC   0(1,R3),0(R4)       PRODUCT                                      
         MVC   1(1,R3),FEEDPRNM    PRODUCT NUMBER                               
         LA    RF,12(RE)           SET ADDRESS OF FEED IN RF                    
         MVC   2(4,R3),0(RF)       FEED                                         
         OC    2(4,R3),SPACES      SPACE FILL                                   
         MVC   6(3,R3),5(R4)       PRODUCT ALPHA                                
         MVI   FEEDEDLN(R3),X'FF'                                               
EDTF260  LA    RE,32(RE)           NEXT SCAN ENTRY                              
         L     R3,SAVER3                                                        
         B     EDTF90                                                           
*                                                                               
EDTFEX   B     EXXMOD                                                           
         EJECT                                                                  
* BUILD FEED ELEMENTS                                                           
*                                                                               
BLDFEED  NTR1                                                                   
         LA    R2,UNTTFD1H         FIELD                                        
         ST    R2,FADDR                                                         
*                                                                               
         CLC   FEEDEDA,SVFEEDA     CHECK FOR ANY FEED CHANGES                   
         BE    BLDFDEX                                                          
         OI    TRAFFLAG,X'40'                                                   
*-FIRST DO THE NATIONAL                                                         
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'21',AIOAREA1),0                    
         CLI   12(R1),0            TEST IF FOUND                                
         BE    *+6                                                              
         DC    H'0'                TAKE A HIT IF BAD RETURN CODE                
         L     R5,12(R1)                                                        
         USING NUCMLEL,R5                                                       
*                                                                               
         CLI   1(R5),X'50'                                                      
         BE    BLDFD050                                                         
         XC    BLOCK,BLOCK                                                      
         MVC   BLOCK(52),0(R5)      SAVE AWAY ELEMENT                           
*                                                                               
         GOTO1 VHELLO,DMCB,(C'D',UNTFILE),(X'21',AIOAREA1),0                    
         MVI   WORK+1,X'50'                                                     
         GOTO1 VHELLO,DMCB,(C'P',UNTFILE),(X'21',AIOAREA1),BLOCK,0              
*                                                                               
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'21',AIOAREA1),0                    
         CLI   12(R1),0            TEST IF FOUND                                
         BE    *+6                                                              
         DC    H'0'                TAKE A HIT IF BAD RETURN CODE                
         L     R5,12(R1)                                                        
         USING NUCMLEL,R5                                                       
*                                                                               
BLDFD050 MVI   NUCMLPRD,0          INIT NATIONAL FEED PRODUCT                   
         XC    NUCMPROD,NUCMPROD                                                
         CLI   FEEDEDA,X'FF'       ANY FEEDS INPUTTED                           
         BE    BLDFD220            DELETE THE FEEDS                             
*                                                                               
         LA    RF,FEEDEDA          NEW FEED PRODUCT TABLE                       
         LA    R3,6                                                             
BLDFD100 CLI   0(RF),X'FF'         END OF TABLE ERROR                           
         BE    BLDFD160                                                         
         CLC   2(2,RF),=CL2'*N'    NATIONAL FEED                                
         BE    BLDFD200                                                         
         CLC   2(2,RF),=CL2'**'    NO NATIONAL FEED                             
         BE    BLDFD210                                                         
         LA    RF,FEEDEDLN(RF)                                                  
         BCT   R3,BLDFD100                                                      
*                                                                               
BLDFD160 MVC   BUYMSG(L'NONATNL),NONATNL                                        
         MVI   FERN,USERERR                                                     
         B     ERROR                                                            
*                                                                               
BLDFD200 MVC   NUCMLPRD,0(RF)      MOVE COPY SPLT PROD (NATIONAL)               
         MVC   NUCMPROD,6(RF)      ALPHA PRODUCT                                
         MVC   NUCMPPOS,1(RF)      MOVE PRODUCT POSITION NUMBER                 
         NI    NUCMLFL2,X'FE'      SET NATIONAL BIT TO YES                      
         XC    2(4,RF),2(RF)       CLEAR NATIONAL ENTRY                         
         B     BLDFD215                                                         
*                                                                               
BLDFD210 MVC   NUCMLPRD,0(RF)      MOVE COPY SPLT PROD (NATIONAL)               
         MVC   NUCMPROD,6(RF)      ALPHA PRODUCT                                
         MVC   NUCMPPOS,1(RF)      MOVE PRODUCT POSITION NUMBER                 
         OI    NUCMLFL2,X'01'      SET NO NATIONAL FLAG                         
         XC    2(4,RF),2(RF)       CLEAR NATIONAL ENTRY                         
*                                                                               
BLDFD215 DS    0H                                                               
         TM    MYFLAG,SMAP                                                      
         BZ    *+8                                                              
         OI    NUCMLFL2,X'01'      SET NO NATIONAL FLAG                         
*                                                                               
         DROP  R5                                                               
         SPACE 2                                                                
*-DO THE FEEDS                                                                  
BLDFD220 GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'23',AIOAREA1),0                    
         CLI   12(R1),0            TEST IF FOUND                                
         BNE   BLDFD400                                                         
         L     R2,12(R1)                                                        
         USING NUFDCEL,R2                                                       
         B     BLDFD255                                                         
*                                                                               
BLDFD240 ZIC   RE,NUFDCLEN                                                      
         AR    R2,RE                                                            
BLDFD250 CLI   0(R2),X'23'                                                      
         BNE   BLDFD400                                                         
BLDFD255 TM    NUFDCFL2,X'40'                                                   
         BO    BLDFD240                                                         
*                                                                               
BLDFD260 LA    RF,FEEDEDA                                                       
         LA    R3,6                                                             
BLDFD280 CLI   0(RF),X'FF'                                                      
         BE    BLDFD320                                                         
         CLC   2(4,RF),NUFDCFED                                                 
         BNE   BLDFD300                                                         
         CLI   0(RF),0             WAS PRODUCT REMOVED                          
         BE    BLDFD320            DELETE FEED                                  
         CLC   6(3,RF),NUFDPROD    WAS PRODUCT CHANGED                          
         BNE   BLDFD360            YES CHANGE FEED                              
         CLC   0(1,RF),NUFDCPRD    WAS PRODUCT CHANGED                          
         BNE   BLDFD360            YES CHANGE FEED                              
         CLC   1(1,RF),NUFDPPOS    HAS PROIDUCT NUMBER CHANGED                  
         BNE   BLDFD360            YES CHANGE FEED                              
         XC    2(4,RF),2(RF)       FEED ALREADY EXIST LEAVE ALONE               
         B     BLDFD240            GET NEXT ELEMENT                             
BLDFD300 LA    RF,FEEDEDLN(RF)                                                  
         BCT   R3,BLDFD280                                                      
*                                                                               
*--FEED NO LONGER EXISTS REMOVE                                                 
BLDFD320 TM    NUFDCFL2,X'40'      DOES FEED HAVE INSTRUCTIONS                  
         BO    BLDFD340            YES CANNOT DELETE                            
         MVC   DUB(4),NUFDCFED                                                  
         GOTO1 VHELLO,DMCB,(C'D',UNTFILE),(X'23',AIOAREA1),(4,DUB)              
         B     BLDFD250                                                         
*                                                                               
BLDFD340 MVC   BUYMSG(L'NODELETE),NODELETE                                      
         MVC   BUYMSG+47(4),NUFDCFED                                            
         MVI   FERN,USERERR                                                     
         B     ERROR                                                            
*                                                                               
*--FEED NOW ASSIGNED TO A DIFFERENT PRODUCT CHANGE ELEMENT                      
BLDFD360 XC    NUFDCML1(16),NUFDCML1    CLEAR OUT COMMERCIAL INFO               
         MVC   NUFDCPRD,0(RF)                                                   
         MVC   NUFDPROD,6(RF)      ALPHA PRODUCT                                
         MVC   NUFDPPOS,1(RF)                                                   
         XC    2(4,RF),2(RF)       ELEMENT UPDATED REMOVE FEED                  
         B     BLDFD240                                                         
         DROP  R2                                                               
         SPACE 2                                                                
*--ANY FEEDS LEFT ON FEEDEDA WILL REQUIRE BUILDING NEW 23 ELEMENTS              
BLDFD400 LA    R2,FEEDEDA                                                       
         LA    R3,6                                                             
*                                                                               
BLDFD420 CLI   0(R2),X'FF'                                                      
         BE    BLDFDEX                                                          
*!!!     CLI   0(R2),0             IS PRODUCT THERE                             
*!!!     BE    BLDFD440            NO, DONT BUILD FEED                          
         OC    6(3,R2),6(R2)                                                    
         BZ    BLDFD440                                                         
         OC    2(4,R2),2(R2)                                                    
         BNZ   BLDFD460                                                         
BLDFD440 LA    R2,FEEDEDLN(R2)                                                  
         BCT   R3,BLDFD420                                                      
         B     BLDFDEX                                                          
*                                                                               
*--CHECK TO SEE IF NEW FEED IS VALID                                            
BLDFD460 DS    0H                                                               
         L     R5,ABUYVALS                                                      
         USING BUYVALD,R5                                                       
*                                                                               
         CLI   UNTCSP,C'S'         SECTIONAL MAP?                               
         BE    BLDFD470                                                         
*                                                                               
         XC    KEY,KEY             VALIDATE FEED CODE                           
         LA    R4,KEY                                                           
         USING FEEDRECD,R4                                                      
         MVC   FEEDKID,=X'0A2B'                                                 
         MVC   FEEDKAM,AGYMED                                                   
         MVC   FEEDKNET,NET                                                     
         MVC   FEEDKFD,2(R2)       FEED                                         
         GOTO1 AIO,DMCB2,SPT+DIR+HIGH                                           
         CLC   FEEDKEY,KEYSAVE     TEST IF CODE FOUND                           
         BE    BLDFD500            YES                                          
*                                                                               
         XC    FEEDKEY,FEEDKEY     CHECK FOR A CLIENT FEED CODE                 
         MVC   FEEDKID,=X'0A2B'                                                 
         MVC   FEEDKAM,AGYMED                                                   
         MVC   FEEDKNET,NET                                                     
         MVC   FEEDKCLT,CLIPK                                                   
         MVC   FEEDKFD,2(R2)       FEED                                         
         GOTO1 AIO,DMCB2,SPT+DIR+HIGH                                           
         CLC   FEEDKEY,KEYSAVE                                                  
         BE    BLDFD500                                                         
         B     INVLFEED                                                         
*                                                                               
BLDFD470 DS    0H                                                               
         XC    KEY,KEY                                                          
         LA    RF,KEY                                                           
         USING SMAPRECD,RF                                                      
*                                                                               
         MVC   SMKTYPE,=X'0D3D'                                                 
         MVC   SMKAGY,AGYMED                                                    
         MVC   SMKYEAR,MAPYEAR     MAP YEAR                                     
         MVC   SMKCODE,MAPCODE     MAP CODE                                     
         DROP  RF                                                               
*                                                                               
         GOTO1 AIO,DMCB2,SPT+DIR+HIGH                                           
         CLC   KEY(13),KEYSAVE                                                  
         BNE   ERROR                                                            
*                                                                               
         GOTO1 AIO,DMCB,SPT+FILE+GET,AIOAREA4                                   
*                                                                               
         LA    R5,6                                                             
*                                                                               
         L     RF,AIOAREA4                                                      
         USING SMAPRECD,RF                                                      
         LA    R3,SMELEM                                                        
         USING SMREGD,R3                                                        
         DROP  RF                                                               
*                                                                               
BLDFD480 DS    0H                                                               
         CLI   0(R3),X'01'         ANY MORE REGIONS?                            
         BNE   INVLFEED                                                         
*                                                                               
         CLC   SMREREG,2(R2)       REGIONS                                      
         BE    BLDFD500                                                         
*                                                                               
         ZIC   RF,1(R3)                                                         
         AR    R3,RF                                                            
         BCT   R5,BLDFD480                                                      
         DROP  R3                                                               
*                                                                               
INVLFEED DS    0H                  INVALID FEED CODE                            
         MVC   BUYMSG(L'INVFEED),INVFEED                                        
         MVC   BUYMSG+41(4),1(R2)                                               
         MVI   FERN,USERERR                                                     
         B     ERROR                                                            
         DROP  R4,R5                                                            
*                                                                               
*--BUILD NEW FEED RECORDS                                                       
BLDFD500 XC    SCANTAB,SCANTAB                                                  
         LA    R4,SCANTAB                                                       
         USING NUFDCEL,R4                                                       
*                                                                               
         MVI   NUFDCEL,X'23'                                                    
         MVI   NUFDCLEN,NUFDCELN                                                
         MVC   NUFDCFED,2(R2)                                                   
         MVC   NUFDCPRD,0(R2)                                                   
         MVC   NUFDPPOS,1(R2)                                                   
         MVC   NUFDPROD,6(R2)                                                   
         OI    NUFDCFL2,X'01'                                                   
*                                                                               
         GOTO1 VHELLO,DMCB,(C'P',UNTFILE),AIOAREA1,SCANTAB,0                    
         B     BLDFD440                                                         
         DROP  R4                                                               
*                                                                               
BLDFDEX  B     EXXMOD                                                           
*  CLEAR THE PERCENT FIELDS                                                     
*                                                                               
CLRPCTS  NTR1                                                                   
         LA    R2,UNTPCT1H                                                      
         LA    R3,6                                                             
*                                                                               
CLRPCT50 MVI   5(R2),0                                                          
         XC    8(6,R2),8(R2)                                                    
         OI    6(R2),X'80'                                                      
         BAS   RE,BUMPFLD                                                       
         MVI   5(R2),0                                                          
         XC    8(7,R2),8(R2)                                                    
         OI    6(R2),X'80'                                                      
         BAS   RE,BUMPFLD                                                       
         BAS   RE,BUMPFLD                                                       
         BAS   RE,BUMPFLD                                                       
         BCT   R3,CLRPCT50                                                      
*                                                                               
CLRPCTEX B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
CLRPRD   NTR1                                                                   
         LA    R2,UNTPRD1H                                                      
         LA    R3,6                                                             
*                                                                               
CLRPRD10 TM    4(R2),X'80'         TEST USER CHANGED ANY FIELDS                 
         JO    CLRPRDX                                                          
         BAS   RE,NEXTFLD                                                       
         TM    4(R2),X'80'                                                      
         JO    CLRPRDX                                                          
         BAS   RE,NEXTFLD                                                       
         TM    4(R2),X'80'                                                      
         JO    CLRPRDX                                                          
         BAS   RE,NEXTFLD                                                       
         TM    4(R2),X'80'                                                      
         JO    CLRPRDX                                                          
         BAS   RE,NEXTFLD                                                       
         BCT   R3,CLRPRD10                                                      
*                                                                               
         LA    R2,UNTPRD1H                                                      
         LA    R3,6                                                             
CLRPRD50 MVI   5(R2),0                                                          
         NI    1(R2),X'FF'-X'20'                                                
         XC    8(L'UNTPRD1,R2),8(R2)                                            
         OI    6(R2),X'80'                                                      
         BAS   RE,NEXTFLD                                                       
         NI    1(R2),X'FF'-X'20'                                                
         MVI   5(R2),0                                                          
         XC    8(L'UNTPCT1,R2),8(R2)                                            
         OI    6(R2),X'80'                                                      
         BAS   RE,NEXTFLD                                                       
         NI    1(R2),X'FF'-X'20'                                                
         MVI   5(R2),0                                                          
         XC    8(L'UNTFED1,R2),8(R2)                                            
         OI    6(R2),X'80'                                                      
         BAS   RE,NEXTFLD                                                       
         NI    1(R2),X'FF'-X'20'                                                
         MVI   5(R2),0                                                          
         XC    8(L'UNTTFD1,R2),8(R2)                                            
         OI    6(R2),X'80'                                                      
         BAS   RE,NEXTFLD                                                       
         BCT   R3,CLRPRD50                                                      
*                                                                               
CLRPRDX  B     EXXMOD                                                           
         EJECT                                                                  
*--UNALOCATE A UNIT                                                             
*                                                                               
UNALLPRD NTR1                                                                   
         L     R3,AIOAREA1                                                      
         USING NURECD,R3                                                        
*--CHECK FOR BILLED OR PAID UNIT                                                
         MVI   FERN,UNALLERR                                                    
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'10',AIOAREA1),0                    
         CLI   12(R1),0            TEST IF FOUND                                
         BE    ERROR               ERROR CANNOT UNALLOCATE BILLED UNIT          
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'12',AIOAREA1),0                    
         CLI   12(R1),0            TEST IF FOUND                                
         BE    ERROR               ERROR CANNOT UNALLOCATE PAID UNIT            
*                                                                               
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'02',AIOAREA1),0                    
         CLI   12(R1),0            TEST IF FOUND                                
         BE    *+6                 ERROR CANNOT UNALLOCATE PAID UNIT            
         DC    H'0'                                                             
         L     R5,12(R1)                                                        
         USING NUSDRD,R5                                                        
         NI    NUSDST3,X'BF'       TURN OFF COPY SPLIT BIT                      
         MVC   SVSTAT3,NUSDST3                                                  
         DROP  R5                                                               
*                                                                               
         CLI   NUPRD,0                                                          
         BE    UNAL200                                                          
*                                                                               
         MVI   NUPRD,0                                                          
         MVI   NUPRD2,0                                                         
         XC    NUP1SHR,NUP1SHR                                                  
         XC    NUFEED,NUFEED                                                    
         B     UNAL400                                                          
*                                                                               
*--DELETE PRODUCTS OFF THE 14 ELEMENTS                                          
UNAL200  GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'19',AIOAREA1),0                    
         CLI   12(R1),0            TEST IF FOUND                                
         BNE   UNAL210             UNIT HAS NO PRODUCTS EXIT                    
*                                                                               
         GOTO1 VHELLO,DMCB,(C'D',UNTFILE),(X'19',AIOAREA1),0                    
*                                                                               
UNAL210  GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'14',AIOAREA1),0                    
         CLI   12(R1),0            TEST IF FOUND                                
         BNE   UNAL450             UNIT HAS NO PRODUCTS EXIT                    
*                                                                               
         GOTO1 VHELLO,DMCB,(C'D',UNTFILE),(X'14',AIOAREA1),0                    
*                                                                               
*--DELETE THE TRAFFIC FEEDS AND UNALLOCATE THE 21 SET PROD CHANGE FLAG          
*                                                                               
UNAL400  GOTO1 VHELLO,DMCB,(C'D',UNTFILE),(X'23',AIOAREA1),0                    
*                                                                               
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'21',AIOAREA1),0                    
         CLI   12(R1),0            TEST IF FOUND                                
         BE    *+6                 ERROR DUMP MUST BE THERE                     
         DC    H'0'                                                             
         L     R5,12(R1)                                                        
         USING NUCMLEL,R5                                                       
         OI    NUCMLFLG,X'40'      TURN ON CHANGE PRODUCT STATUS                
         MVI   NUCMLPRD,0          REMOVE COPY SPLIT PRODUCT                    
         XC    NUCMPROD,NUCMPROD                                                
         XC    NUCML1(16),NUCML1   CLEAR COMMERCIAL OUT                         
         DROP  R5                                                               
*                                                                               
UNAL450  DS    0H                                                               
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'18',AIOAREA1),0                    
         CLI   12(R1),0            TEST IF FOUND                                
         BNE   UNALEX                                                           
         L     R5,12(R1)                                                        
         USING NUDTAD,R5                                                        
*                                                                               
         XC    NUDTAMYR,NUDTAMYR                                                
         XC    NUDTAMCD,NUDTAMCD                                                
         DROP  R5                                                               
*                                                                               
UNALEX   DS    0H                                                               
         OI    MYFLAG,DELFEED                                                   
         B     EXXMOD                                                           
         DROP  R3                                                               
         EJECT                                                                  
*  CLEAR THE SCREEN                                                             
* SUB-ROUTINE TO DISPLAY UNIT RECORD VALUES                                     
*                                                                               
DISUNIT  NTR1                                                                   
         XC    SVFEEDA,SVFEEDA                                                  
         MVI   FEEDPRNM,0                                                       
         MVI   SVFEEDA,X'FF'                                                    
*  CLEAR THE SCREEN                                                             
         BAS   RE,INITSCRN         INITIALIZE A NEW SCREEN                      
         BAS   RE,CLRSCRN                                                       
         BRAS  RE,BLDFTAB                                                       
*                                                                               
         GOTO1 DISLEN,DMCB,NBLEN,UNTLEN                                         
         OI    UNTLENH+6,X'80'     TRANSMIT                                     
         SPACE                                                                  
*                                                                               
         XC    FLD,FLD                                                          
         GOTO1 DISACT,DMCB,NBACTUAL                                             
         MVC   UNTACT,FLD                                                       
         OI    UNTACTH+6,X'80'     TRANSMIT                                     
         SPACE                                                                  
*                                                                               
         XC    FLD,FLD                                                          
         GOTO1 DISINT,DMCB,NBINTEG                                              
         MVC   UNTINT,FLD                                                       
         OI    UNTINTH+6,X'80'     TRANSMIT                                     
*                                                                               
         XC    FLD,FLD                                                          
         MVC   UNTOPT,FLD                                                       
         OI    UNTOPTH+6,X'80'                                                  
*                                                                               
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'14',AIOAREA1),0                    
         CLI   12(R1),0            TEST IF FOUND                                
         BNE   DIS20                                                            
         L     RF,12(R1)                                                        
         TM    2(RF),X'80'         IS THIS A TRIGGY?                            
         BZ    *+12                                                             
         MVI   UNTCSP,C'T'                                                      
         B     DIS25                                                            
         TM    2(RF),X'40'         IS IT A SECTIONAL MAP?                       
         BZ    DIS20                                                            
         MVI   UNTCSP,C'S'                                                      
*                                                                               
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'18',AIOAREA1),0                    
         CLI   12(R1),0            TEST IF FOUND                                
         BNE   DISEX                                                            
         L     RE,12(R1)                                                        
         USING NUDTAD,RE                                                        
*                                                                               
         MVC   MAPYEAR,NUDTAMYR                                                 
         MVC   MAPCODE,NUDTAMCD                                                 
*                                                                               
         MVC   UNTOPT(2),=C'S='                                                 
         MVI   UNTOPT+4,C'/'                                                    
         MVC   UNTOPT+5(8),NUDTAMCD                                             
         OI    UNTOPTH+6,X'80'                                                  
*                                                                               
         XC    DUB,DUB                                                          
         MVC   DUB(1),NUDTAMYR                                                  
         MVC   DUB+2(2),=X'0101'                                                
*                                                                               
         GOTO1 VDATCON,DMCB,(3,DUB),(5,WORK)                                    
*                                                                               
         MVC   UNTOPT+2(2),WORK+6                                               
         B     DIS25                                                            
         DROP  RE                                                               
*                                                                               
DIS20    MVI   UNTCSP,C'N'                                                      
         TM    NBUNST3,X'40'                                                    
         BZ    *+8                                                              
         MVI   UNTCSP,C'Y'                                                      
DIS25    OI    UNTCSPH+6,X'80'     TRANSMIT                                     
*                                                                               
*   DISPLAY COMMENT                                                             
*                                                                               
         LA    R2,UNTCOM1H                                                      
         GOTO1 VCLEARF,DMCB,(R2),UNTLAST                                        
         MVI   BYTE,NO             SET SECOND FIELD SWITCH                      
         MVI   HALF,C'C'           FILTER ON COMMENT ELEMENT                    
         MVI   HALF+1,1                                                         
         SPACE                                                                  
DIS50    GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'04',AIOAREA1),(2,HALF)             
         CLI   12(R1),0            TEST IF FOUND                                
         BNE   DIS80               NO-EXIT                                      
*                                                                               
         L     R3,12(R1)                                                        
         USING NUCOMD,R3                                                        
         LA    R0,L'UNTCOM1        SCREEN FIELD LENGTH                          
         ZIC   R1,NUCOMLEN                                                      
         SH    R1,=H'4'            COMMENT LENGTH                               
         CR    R0,R1               NO LONGER THAN SCREEN FIELD                  
         BNL   *+6                                                              
         LR    R1,R0                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),NUCOMMNT                                                 
*                                                                               
         CLI   BYTE,YES            TEST IF SECOND FIELD DONE                    
         BE    DIS80               YES-EXIT                                     
         MVI   BYTE,YES                                                         
         LA    R2,UNTCOM2H                                                      
         MVI   HALF+1,2            FILTER ON LINE 2                             
         B     DIS50                                                            
*                                                                               
DIS80    DS    0H                                                               
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'19',AIOAREA1),0                    
         CLI   12(R1),0            TEST IF FOUND                                
         BNE   *+12                                                             
         BRAS  RE,DIS19                                                         
         B     DIS500                                                           
*                                                                               
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'14',AIOAREA1),0                    
         CLI   12(R1),0            TEST IF FOUND                                
         BE    DIS300                                                           
*                                                                               
         CLI   NBPRD,0                                                          
         BE    DIS300              (DISPLAY OFF OF 14 ELEMENT)                  
*                                                                               
         GOTO1 DISPRD,DMCB,NBPRD,UNTPRD1                                        
         OI    UNTPRD1H+6,X'80'    TRANSMIT                                     
         SPACE                                                                  
         GOTO1 DISP1SHR,DMCB,NBP1SHR,UNTPCT1                                    
         OI    UNTPCT1H+6,X'80'    TRANSMIT                                     
         SPACE                                                                  
         GOTO1 DISFEED,DMCB,NBFEED,UNTFED1                                      
         OI    UNTFED1H+6,X'80'    TRANSMIT                                     
         SPACE                                                                  
         LA    R2,UNTTFD1H         FIELD                                        
         LA    R4,FEEDADIS+4       TABLE ENTRY                                  
         BAS   RE,DISFEDT                                                       
         GOTO1 EDITFEED,DMCB,SVFEEDA,NBPRD,0(R2)      BUILD FEED TABLE          
         OI    UNTTFD1H+6,X'80'    TRANSMIT                                     
         SPACE 2                                                                
*--SECOND PRODUCT LINE                                                          
         GOTO1 DISPRD,DMCB,NBPRD2,UNTPRD2                                       
         OI    UNTPRD2H+6,X'80'    TRANSMIT                                     
*                                                                               
DIS100   DS    0H                                                               
*        OC    NBP1SHR,NBP1SHR                                                  
*        BZ    DIS120                                                           
         CLI   UNTCSP,C'N'                                                      
         BNE   DIS110                                                           
         OC    NBPRD2,NBPRD2                                                    
         BNZ   DIS110                                                           
         OC    NBP1SHR,NBP1SHR                                                  
         BNZ   DIS120                                                           
         SR    RF,RF                                                            
         A     RF,=F'10000'                                                     
         STCM  RF,3,FULL                                                        
         GOTO1 DISP1SHR,DMCB,FULL,UNTPCT1                                       
         B     DIS120                                                           
*                                                                               
DIS110   SR    RE,RE                                                            
         ICM   RE,3,NBP1SHR                                                     
         SR    RF,RF                                                            
         A     RF,=F'10000'                                                     
         SR    RF,RE                                                            
         BM    DIS120                                                           
         STCM  RF,3,FULL                                                        
         GOTO1 DISP1SHR,DMCB,FULL,UNTPCT2                                       
         OI    UNTPCT2H+6,X'80'    TRANSMIT                                     
         SPACE                                                                  
DIS120   OC    NBFEED,NBFEED                                                    
         BZ    DIS200                                                           
         SR    RE,RE                                                            
         ICM   RE,3,NBFEED                                                      
         SR    RF,RF                                                            
         A     RF,=F'10000'                                                     
         SR    RF,RE                                                            
         BM    DIS200                                                           
         STCM  RF,3,FULL                                                        
         GOTO1 DISFEED,DMCB,FULL,UNTFED2                                        
         OI    UNTFED2H+6,X'80'    TRANSMIT                                     
         SPACE                                                                  
         LA    R2,UNTTFD2H         FIELD                                        
         LA    R4,FEEDADIS+4                                                    
         LA    R4,FEEDADLN(R4)     TABLE ENTRY                                  
         BAS   RE,DISFEDT                                                       
         GOTO1 EDITFEED,DMCB,SVFEEDA,NBPRD2,0(R2)     BUILD FEED TABLE          
         OI    UNTTFD2H+6,X'80'    TRANSMIT                                     
         SPACE                                                                  
DIS200   B     EXXMOD                                                           
         SPACE 3                                                                
*--GET PRODUCT INFO OFF OF THE X'14' ELEMENTS                                   
DIS300   LA    R2,UNTPRD1H                                                      
         LA    R4,FEEDADIS+4                                                    
         MVI   FEEDPRNM,1                                                       
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'14',AIOAREA1),0                    
         CLI   12(R1),6            TEST IF FOUND                                
         BE    DIS500              NO ELEMENT EXISTS EXIT ROUTINE               
         CLI   12(R1),0            TEST IF FOUND                                
         BE    *+6                                                              
         DC    H'0'                TAKE A HIT IF BAD RETURN CODE                
DIS310   L     R5,12(R1)                                                        
*-CALCULATE NUMBER OF PRODUCTS                                                  
         SR    RE,RE                                                            
         ZIC   RF,1(R5)            ELEMENT LENGTH                               
         SH    RF,=H'3'                                                         
         D     RE,=F'6'                                                         
         LR    R3,RF                                                            
         LA    R5,3(R5)            GET FIRST ENTRY                              
*                                                                               
DIS340   DS    0H                                                               
         GOTO1 DISPRD,DMCB,0(R5),8(R2)                                          
         CLI   UNTCSP,C'S'                                                      
         BNE   *+16                                                             
         BAS   RE,NEXTFLD                                                       
         OI    1(R2),X'20'                                                      
         B     *+8                                                              
         BAS   RE,TRANFLD                                                       
*                                                                               
         GOTO1 DISP1SHR,DMCB,2(R5),8(R2)                                        
         CLI   UNTCSP,C'S'                                                      
         BNE   *+12                                                             
         BAS   RE,NEXTFLD                                                       
         B     *+8                                                              
         BAS   RE,TRANFLD                                                       
*                                                                               
         OI    MYFLAG,DISPR                                                     
         GOTO1 DISFEED,DMCB,4(R5),8(R2)    FEED PERCENT                         
         CLI   UNTCSP,C'S'                                                      
         BNE   *+16                                                             
         BAS   RE,NEXTFLD                                                       
         OI    1(R2),X'20'                                                      
         B     *+8                                                              
         BAS   RE,TRANFLD                                                       
         NI    MYFLAG,X'FF'-DISPR                                               
*                                                                               
         BAS   RE,DISFEDT                                                       
         GOTO1 EDITFEED,DMCB,SVFEEDA,0(R5),0(R2)   BUILD FEED TABLE             
         ZIC   RF,FEEDPRNM                                                      
         LA    RF,1(RF)                                                         
         STCM  RF,1,FEEDPRNM                                                    
*                                                                               
         CLI   UNTCSP,C'S'                                                      
         BE    *+12                                                             
         BAS   RE,TRANFLD                                                       
         B     DIS450                                                           
*                                                                               
         XC    KEY,KEY                                                          
         LA    RF,KEY                                                           
         USING FEEDRECD,RF                                                      
         L     RE,NBAIO                                                         
         USING NURECD,RE                                                        
*                                                                               
         MVC   FEEDKID,=X'0A2B'                                                 
         MVC   FEEDKAM,NUKAM       AGY/MEDIA                                    
         MVC   FEEDKNET,NUKNET     NETWORK                                      
         MVC   FEEDKCLT,NUKCLT     CLIENT                                       
         MVC   FEEDKFD(2),8(R2)       FEED                                      
         OC    FEEDKFD,SPACES                                                   
         DROP  RE,RF                                                            
*                                                                               
         GOTO1 AIO,DMCB2,SPT+DIR+HIGH                                           
         CLC   KEY(13),KEYSAVE                                                  
         BNE   DIS430                                                           
*                                                                               
         GOTO1 AIO,DMCB,SPT+FILE+GET,AIOAREA3                                   
*                                                                               
         GOTO1 VHELLO,DMCB,(C'G',SPTFILE),(X'40',AIOAREA3),0                    
         CLI   12(R1),0            TEST IF FOUND                                
         BNE   DIS430                                                           
         L     RF,12(R1)                                                        
         USING FEEDSMEL,RF                                                      
*                                                                               
         TM    FEEDSFLG,FEEDSDEL                                                
         BO    DIS430                                                           
*                                                                               
DIS425   DS    0H                                                               
         CLI   0(RF),X'40'                                                      
         BNE   DIS430                                                           
*                                                                               
         CLC   FEEDSMYR,MAPYEAR                                                 
         BNE   *+14                                                             
         CLC   FEEDSMCD,MAPCODE                                                 
         BE    DIS427                                                           
*                                                                               
         ZIC   RE,1(RF)                                                         
         AR    RF,RE                                                            
         B     DIS425                                                           
*                                                                               
DIS427   MVC   12(30,R2),FEEDSMDS  DESCRIPTION                                  
         DROP  RF                                                               
*                                                                               
DIS430   DS    0H                                                               
         BAS   RE,NEXTFLD                                                       
*                                                                               
DIS450   DS    0H                                                               
         LA    R5,6(R5)                                                         
         LA    R4,FEEDADLN(R4)                                                  
         BCT   R3,DIS340                                                        
*                                                                               
DIS500   DS    0H                                                               
*                                                                               
DISEX    DS    0H                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
* DISPLAY THE FEED CODES OFF THE FEED TABLE                                     
*                                                                               
*-- R2 = ADDRESS OF OUTPUT FIELD                                                
*-- R4 = ADDRESS OF FEED TABLE                                                  
DISFEDT  NTR1                                                                   
         LR    R3,R2                                                            
         LA    R3,8(R3)                                                         
         OC    0(5,R4),0(R4)                                                    
         BZ    DISFDEX                                                          
         MVC   0(5,R3),0(R4)       MOVE FIRST FEED TO SCREEN                    
*                                                                               
*--MOVE THE REST OF THE FEEDS OUT MOVE COMMAS OUT WHEN NESCESARY                
DISFD60  LA    R4,5(R4)                                                         
         OC    0(5,R4),0(R4)                                                    
         BZ    DISFDEX                                                          
         LA    RE,5                FIND FIRST BLANK                             
*                                                                               
DISFD80  CLI   0(R3),X'40'                                                      
         BNH   DISFD100                                                         
         LA    R3,1(R3)                                                         
         BCT   RE,DISFD80                                                       
*                                                                               
DISFD100 MVI   0(R3),C','                                                       
         LA    R3,1(R3)                                                         
         MVC   0(5,R3),0(R4)                                                    
         B     DISFD60                                                          
*                                                                               
DISFDEX  B     EXXMOD                                                           
         EJECT                                                                  
* SUB-ROUTINE TO DISPLAY UNIT RECORD VALUES                                     
*                                                                               
CLRSCRN  NTR1                                                                   
         LA    R2,UNTLENH                                                       
*                                                                               
         OI    4(R2),X'20'         SET PREVALID BIT (LENGTH)                    
         XC    8(5,R2),8(R2)                                                    
         BAS   RE,TRANFLD                                                       
*                                                                               
         OI    4(R2),X'20'         SET PREVALID BIT (COPY SPLIT)                
         XC    8(1,R2),8(R2)                                                    
         BAS   RE,TRANFLD                                                       
*                                                                               
         CLI   UNTCSP,C'S'                                                      
         BNE   *+12                                                             
         BAS   RE,CLRPRD                                                        
         B     CLRSCRNX                                                         
*                                                                               
*  CLEAR THE SCREEN                                                             
         LA    R2,UNTPRD1H                                                      
         LA    R1,6                                                             
*                                                                               
CLRS020  OI    4(R2),X'20'         SET PREVALID BIT (PRODUCT)                   
         XC    8(4,R2),8(R2)                                                    
         BAS   RE,TRANFLD                                                       
         OI    4(R2),X'20'         SET PREVALID BIT (PRD PCT)                   
         XC    8(6,R2),8(R2)                                                    
         BAS   RE,TRANFLD                                                       
         OI    4(R2),X'20'         SET PREVALID BIT (FEED PCT)                  
         XC    8(7,R2),8(R2)                                                    
         BAS   RE,TRANFLD                                                       
         OI    4(R2),X'20'         SET PREVALID BIT (TRAFFIC FEEDS)             
         XC    8(35,R2),8(R2)                                                   
         BAS   RE,TRANFLD                                                       
*                                                                               
         BCT   R1,CLRS020                                                       
CLRSCRNX B     EXXMOD                                                           
*                                                                               
INITSCRN NTR1                                                                   
         LA    R2,UNTPRD1H                                                      
         LA    R1,6                                                             
*                                                                               
INITSC10 OI    4(R2),X'20'         SET PREVALID BIT (PRODUCT)                   
         XC    8(4,R2),8(R2)                                                    
         NI    1(R2),X'FF'-X'20'                                                
         OI    6(R2),X'80'                                                      
         BAS   RE,NEXTFLD                                                       
         OI    4(R2),X'20'         SET PREVALID BIT (PRD PCT)                   
         XC    8(6,R2),8(R2)                                                    
         NI    1(R2),X'FF'-X'20'                                                
         OI    6(R2),X'80'                                                      
         BAS   RE,NEXTFLD                                                       
         OI    4(R2),X'20'         SET PREVALID BIT (FEED PCT)                  
         XC    8(7,R2),8(R2)                                                    
         NI    1(R2),X'FF'-X'20'                                                
         OI    6(R2),X'80'                                                      
         BAS   RE,NEXTFLD                                                       
         OI    4(R2),X'20'         SET PREVALID BIT (TRAFFIC FEEDS)             
         XC    8(35,R2),8(R2)                                                   
         NI    1(R2),X'FF'-X'20'                                                
         OI    6(R2),X'80'                                                      
         BAS   RE,NEXTFLD                                                       
         BCT   R1,INITSC10                                                      
*                                                                               
INITSCRX B     EXXMOD                                                           
         EJECT                                                                  
* DISPLAY PRODUCT CODES                                                         
*                                                                               
DISPRD   NTR1                                                                   
         L     R3,0(R1)            ADDRESS TO PRODUCT                           
         L     R2,4(R1)            ADDRESS TO OUTPUT                            
*                                                                               
         CLI   0(R3),0                                                          
         BE    DISPRDX                                                          
         TM    NBUNST2,X'20'       TEST FOR FROZEN PROD. ALLOCATION             
         BZ    *+12                NO                                           
         MVI   0(R2),DASH          PREFIX FIELD WITH A DASH                     
         LA    R2,1(R2)            INCREMENT OUTPUT POINTER                     
         LA    R1,NBPRD                                                         
         BAS   RE,GETPRD                                                        
         SPACE                                                                  
DISPRDX  B     EXXMOD                                                           
         SPACE 2                                                                
* SUB-ROUTINE TO GET PRODUCT CODE FROM CLIENT LIST                              
*                                                                               
GETPRD   NTR1                                                                   
         L     R5,ABUYVALS                                                      
         USING BUYVALD,R5                                                       
*                                                                               
         LA    R0,255                                                           
         LA    RF,CLILIST                                                       
         CLC   0(1,R3),3(RF)       TEST FOR PRODUCT NUMBER                      
         BE    *+14                                                             
         LA    RF,4(RF)                                                         
         BCT   R0,*-14                                                          
         DC    H'0'                                                             
*                                                                               
         MVC   0(3,R2),0(RF)       EXTRACT PRODUCT CODE                         
         B     EXXMOD              RETURN NO CALLER                             
         DROP  R5                                                               
         SPACE 2                                                                
* DISPLAY FIRST PRODUCT SHARE                                                   
*                                                                               
DISP1SHR NTR1                                                                   
         L     R2,4(R1)            OUTPUT FIELD                                 
         L     RE,0(R1)            PROD PERCENT                                 
         SR    R3,R3                                                            
         ICM   R3,3,0(RE)          TEST FOR FIRST PRODUCT SHARE                 
*        BZ    EXXMOD              ZERO-EXIT                                    
         BAS   R8,EDTTWO           EDIT TO TWO DECIMAL PLACES                   
         B     EXXMOD                                                           
         SPACE 2                                                                
* DISPLAY SECONDS LENGTH                                                        
*                                                                               
DISLEN   NTR1                                                                   
         L     RE,0(R1)            LENGTH                                       
         SR    R3,R3                                                            
         ICM   R3,1,0(RE)          LENGTH                                       
         L     R2,4(R1)            R3=OUTPUT POINTER                            
*                                                                               
         TM    NBUNST3,X'40'       COPY-SPLIT BIT SET                           
         BZ    DISL02                                                           
         MVI   0(R2),C'C'          YES                                          
         LA    R2,1(R2)                                                         
DISL02   TM    NBUNST2,X'10'       TEST FOR FROZEN LENGTH                       
         BZ    *+12                                                             
         MVI   0(R2),DASH          YES                                          
         LA    R2,1(R2)                                                         
         BAS   R8,EDTLEN                                                        
         B     EXXMOD                                                           
         SPACE 2                                                                
* DISPLAY ACTUAL COST                                                           
*                                                                               
DISACT   NTR1                                                                   
         L     RE,0(R1)            ACTUAL                                       
         SR    R2,R2                                                            
         ICM   R2,15,0(RE)                                                      
         BNZ   DISACT2                                                          
         TM    NBUNITST,X'20'      ITS ZERO-TEST FOR COST OVERRIDE              
         BZ    DISACT3                                                          
         MVI   FLD,C'0'            FORCE A ZERO TO OUTPUT                       
         B     DISACT3                                                          
         SPACE                                                                  
DISACT2  BAS   R8,EDTMIN                                                        
DISACT3  CLI   NBRTTYPE,0                                                       
         BE    EXXMOD                                                           
         LA    R8,FLD                                                           
         MVC   DUB(8),0(R8)                                                     
         MVC   0(1,R8),NBRTTYPE    MOVE RATE TYPE                               
         LA    R8,1(R8)                                                         
         CLI   NBSDRTCV,0          IS THERE COVERAGE                            
         BE    *+14                NO                                           
         MVC   0(1,R8),NBSDRTCV                                                 
         LA    R8,1(R8)                                                         
         MVC   0(8,R8),DUB                                                      
         B     EXXMOD                                                           
         SPACE 2                                                                
* DISPLAY INTEGRATION RATE                                                      
*                                                                               
DISINT   NTR1                                                                   
         L     RE,0(R1)            INTEGRATION                                  
         SR    R2,R2                                                            
         ICM   R2,15,0(RE)                                                      
         BZ    DISI100                                                          
         BAS   R8,EDTMIN                                                        
         B     EXXMOD                                                           
         SPACE                                                                  
DISI100  TM    NBUNST4,X'80'       ZERO INPUTTED                                
         BO    DISI200                                                          
*                                                                               
         L     R2,APACKREC                                                      
         USING NPRECD,R2                                                        
         TM    NPAKCNTL,X'80'                                                   
         BNO   *+10                                                             
DISI200  MVC   FLD(1),=C'0'                                                     
         B     EXXMOD                                                           
         DROP  R2                                                               
         SPACE 2                                                                
* DISPLAY FEED                                                                  
*                                                                               
DISFEED  NTR1                                                                   
         L     R2,4(R1)            OUTPUT POINTER                               
         L     RE,0(R1)            FEED                                         
         SR    R3,R3                                                            
         ICM   R3,3,0(RE)                                                       
         BZ    EXXMOD                                                           
         BAS   R8,EDTTWO                                                        
         L     R8,ATWA                                                          
*                                                                               
         TM    NBUNST3,X'01'       FEED AFFECTS RATINGS                         
         BZ    DISFEEDX                                                         
         SPACE                                                                  
         LR    R1,R2                                                            
DISFEED2 CLI   0(R1),X'40'         FIND LAST POSITION ON FEED LINE              
         BNH   DISFEED3                                                         
         LA    R1,1(R1)                                                         
         B     DISFEED2                                                         
DISFEED3 DS    0H                                                               
*!!      CLI   UNTCSP,C'S'                                                      
*!!      BE    *+8                                                              
         TM    MYFLAG,DISPR                                                     
         BZ    *+8                                                              
         MVI   0(R1),C'R'          SET FEED AFFECTS RATING INDICATOR            
*                                                                               
DISFEEDX B     EXXMOD                                                           
         SPACE 2                                                                
* EDITING SUB-ROUTINES                                                          
*                                                                               
EDTLEN   EDIT  (R3),(3,(R2)),ALIGN=LEFT                                         
         BR    R8                                                               
         SPACE 1                                                                
EDTTWO   EDIT  (R3),(6,(R2)),2,ALIGN=LEFT                                       
         BR    R8                                                               
         SPACE 1                                                                
EDTMIN   TM    NBUNITST,X'80'      TEST FOR MINUS UNIT                          
         BZ    *+6                                                              
         LNR   R2,R2               CONVERT COST TO NEGATIVE NUMBER              
         LR    R0,R2               SAVE COST VALUE                              
         SRDA  R2,32               PREPARE DIVIDEND                             
         D     R2,=F'100'          SEPARATE DOLLARS AND PENNIES                 
         LTR   R2,R2               TEST REMAINDER (PENNIES)                     
         BNZ   EDTMIN2             YES                                          
         EDIT  (R3),(11,FLD),ALIGN=LEFT,MINUS=YES                               
         B     EDTMINX                                                          
         SPACE                                                                  
EDTMIN2  LR    R2,R0               RESTORE COST VALUE W PENNIES                 
         EDIT  (R2),(12,FLD),2,ALIGN=LEFT,MINUS=YES                             
         SPACE                                                                  
EDTMINX  BR    R8                                                               
         SPACE 2                                                                
         EJECT                                                                  
* SUB-ROUTINE TO TRANSMIT SCREEN FIELDS                                         
*                                                                               
*  CLEAR THE SCREEN                                                             
NEXTFLD  DS    0H                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BR    RE                                                               
*                                                                               
TRANFLD  OI    6(R2),X'80'         TRANSMIT FIELD                               
BUMPFLD  SR    R0,R0               FIND NEXT UNPROTECTED FIELD                  
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
*                                                                               
         TM    1(R2),X'20'         IS FIELD PROTECTED                           
         BO    BUMPFLD                                                          
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
MSG2     CLI   ACTION,DPR          TEST FOR ACTION BUY                          
         BE    MSGX                ALL DONE                                     
         CLI   ACTION,CPR          TEST FOR DISPLAY                             
         BE    MSGX                YES                                          
         MVC   BUYMSG+5(9),=C'DISPLAYED'                                        
         LA    R3,BUYMSG+15                                                     
         LA    R2,UNTLENH                                                       
         MVC   0(15,R3),=C'- ENTER CHANGES'                                     
         CLI   ACTION,CPR                                                       
         BE    MSGX                                                             
         MVC   0(20,R3),=C'- NOW YOU MAY DELETE'                                
         OI    1(R2),X'01'         CONVERT CURSOR TO MODIFIED                   
         B     MSGX                                                             
         SPACE                                                                  
MSGX     ST    R2,FADDR                                                         
         L     RE,SAVEREG                                                       
         BR    RE                                                               
         EJECT                                                                  
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
SPTFILE  DC    CL8'SPTFILE'                                                     
NONATNL  DC    C'** ERROR - NO NATIONAL FEED CODE DEFINED **'                   
NODELETE DC    C'** ERROR - FEED HAS INSTRUCTIONS CANNOT DELETE     '           
INVFEED  DC    C'** ERROR - THE FOLLOWING FEED IS INVALID     '                 
NOFDESCQ DC    C'** ERROR - FEED RECORD NOT SET UP - '                          
MAXFEED  DC    C'** ERROR - THE MAXIMUM NUMBER OF FEEDS ALLOWED IS 6'           
*****COPSERR  DC    C'** ERROR - FIELD MUST BE A (Y),(U),(C), OR (T)'           
COPSERR  DC    C'** ERROR - FIELD MUST BE A (Y),(U), OR (C)'                    
PRD2ERR  DC    C'** ERROR - MUST HAVE AT LEAST 2 PRODUCTS'                      
PRD3ERR  DC    C'** ERROR - MUST HAVE 3 PRODUCTS'                               
NPRDERR  DC    C'** ERROR - THERE IS NO PRODUCT FOR THIS LINE'                  
PCTFERR  DC    C'** ERROR - PERCENTS MUST ADD UP TO 100.00'                     
DUPRERR  DC    C'** ERROR - DUPLICATE PRODUCTS NOT ALLOWED'                     
NOFDSALW DC    C'** ERROR - FEEDS FOR TRI-BACK MUST BE SET IN TRAFFIC'          
UNITBB   DC    C'** ERROR - INVALID ACTION B/C UNIT HAS A BILLBOARD'            
NOCHTRI  DC    C'** ERROR - UNIT MUST REMAIN A TRI-BACK'                        
         SPACE 2                                                                
* LIST OF FIELDS TO EDIT                                                        
*                                                                               
EDLIST   DS    0H                                                               
         DC    AL1(UPRD)                                                        
         DC    AL1(UP1SHR)                                                      
         DC    AL1(UFEED)                                                       
         DC    X'FF'                                                            
         SPACE 2                                                                
* TABLE OF ACTIONS AND THEIR NAMES                                              
*                                                                               
ACTTAB   DS    0CL10                                                            
         DC    AL1(CPR),CL9'CHANGED'                                            
         DC    AL1(DPR),CL9'DISPLAYED'                                          
ACTS     EQU   (*-ACTTAB)/L'ACTTAB                                              
         SPACE 2                                                                
* LITERAL POOL                                                                  
*                                                                               
EXXMOD   XMOD1 1                                                                
         LTORG                                                                  
UPDACT   NTR1  BASE=*,LABEL=*                                                   
*  UPDATE ACTIVITY ELEMENT                                                      
         GOTO1 VDATCON,DMCB2,(2,TODAYC),(3,THREE)                               
         XC    WORK,WORK                                                        
         LA    RE,WORK             BUILD ACTIVITY ELEMENT                       
         USING NUACTD,RE                                                        
         MVI   NUACTEL,X'99'                                                    
         MVI   NUACTLEN,23                                                      
         MVC   NUACTADT,THREE      ADD DATE                                     
         MVC   NUACTCDT,THREE      LAST ACTIVITY DATE                           
         MVC   NUACTAID,SVPASSWD   ADD PERSONAL ID                              
         MVC   NUACTCID,SVPASSWD   LAST PERSONAL ID                             
         MVC   NUACTAGD,SECAGYA    SECURITY AGENCY                              
         MVC   NUACTRSN,AUDREASN   REASON CODE                                  
         MVC   NUACTOVL,OVLAYNUM   OVERLAY NUMBER                               
*                                                                               
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'99',AIOAREA1),0                    
         CLI   12(R1),0            TEST IF FOUND                                
         BE    *+6                                                              
         DC    H'0'                TAKE A HIT IF NOT THERE-BAD RECORD           
         L     RF,12(R1)           COPY THE 3RD STATUS BIT                      
         LA    RE,WORK             RESET RE TO WORK                             
         MVC   NUACTACD,2(RF)      COPY OVER ADD AUTH. CODE                     
         MVC   NUACTADT,4(RF)      AND THE CREATION DATE                        
         CLI   1(RF),13                                                         
         BL    UPDACT10                                                         
         MVC   NUACTAID,12(RF)     COPY OVER ADD PERSONAL ID                    
         MVC   NUACTAGD,16(RF)     AND SECURITY AGENCY                          
         SPACE                                                                  
UPDACT10 GOTO1 VHELLO,DMCB,(C'D',UNTFILE),(X'99',AIOAREA1)                      
         GOTO1 VHELLO,DMCB,(C'P',UNTFILE),(X'99',AIOAREA1),WORK,0               
         DROP  RE                                                               
                                                                                
         J     EXXMOD                                                           
         LTORG                                                                  
*                                                                               
* MODULE AND ROUTINE EXIT                                                       
DIS19    NTR1  BASE=*,LABEL=*                                                   
         LA    R2,UNTPRD1H                                                      
         LA    R4,FEEDADIS+4                                                    
         MVI   FEEDPRNM,1                                                       
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'19',AIOAREA1),0                    
         L     R5,12(R1)                                                        
*-CALCULATE NUMBER OF PRODUCTS                                                  
         SR    RE,RE                                                            
         ZIC   RF,1(R5)            ELEMENT LENGTH                               
         SH    RF,=H'3'                                                         
         D     RE,=F'7'                                                         
         LR    R3,RF                                                            
         LA    R5,3(R5)            GET FIRST ENTRY                              
*                                                                               
DIS19010 DS    0H                                                               
         MVC   8(3,R2),0(R5)                                                    
         CLI   UNTCSP,C'S'                                                      
         BNE   *+16                                                             
         BAS   RE,NEXTFLD                                                       
         OI    1(R2),X'20'                                                      
         B     *+8                                                              
         BAS   RE,TRANFLD                                                       
*                                                                               
         GOTO1 DISP1SHR,DMCB,3(R5),8(R2)                                        
         CLI   UNTCSP,C'S'                                                      
         BNE   *+12                                                             
         BAS   RE,NEXTFLD                                                       
         B     *+8                                                              
         BAS   RE,TRANFLD                                                       
*                                                                               
         OI    MYFLAG,DISPR                                                     
         GOTO1 DISFEED,DMCB,5(R5),8(R2)    FEED PERCENT                         
         CLI   UNTCSP,C'S'                                                      
         BNE   *+16                                                             
         BAS   RE,NEXTFLD                                                       
         OI    1(R2),X'20'                                                      
         B     *+8                                                              
         BAS   RE,TRANFLD                                                       
         NI    MYFLAG,X'FF'-DISPR                                               
*                                                                               
DIS19020 DS    0H                                                               
         CLI   UNTCSP,C'N'                                                      
         BNE   DIS19040                                                         
*                                                                               
         XC    FULL,FULL                                                        
         MVC   FULL(2),NBP1SHR                                                  
         OC    NBP1SHR,NBP1SHR                                                  
         BNZ   DIS19022                                                         
         SR    RF,RF                                                            
         A     RF,=F'10000'                                                     
         STCM  RF,3,FULL                                                        
DIS19022 GOTO1 DISP1SHR,DMCB,FULL,UNTPCT1                                       
*                                                                               
DIS19025 DS    0H                                                               
         OC    NBP1SHR,NBP1SHR                                                  
         BZ    DIS19040                                                         
         SR    RE,RE                                                            
         ICM   RE,3,NBP1SHR                                                     
         SR    RF,RF                                                            
         A     RF,=F'10000'                                                     
         SR    RF,RE                                                            
         BM    DIS19030                                                         
         STCM  RF,3,FULL                                                        
         GOTO1 DISP1SHR,DMCB,FULL,UNTPCT2                                       
         OI    UNTPCT2H+6,X'80'    TRANSMIT                                     
*                                                                               
DIS19030 DS    0H                                                               
         OC    NBFEED,NBFEED                                                    
         BZ    DIS19040                                                         
         GOTO1 DISFEED,DMCB,NBFEED,UNTFED1  FEED PERCENT                        
         OI    UNTFED1H+6,X'80'                                                 
         SR    RE,RE                                                            
         ICM   RE,3,NBFEED                                                      
         SR    RF,RF                                                            
         A     RF,=F'10000'                                                     
         SR    RF,RE                                                            
         BM    DIS19040                                                         
         STCM  RF,3,FULL                                                        
         GOTO1 DISFEED,DMCB,FULL,UNTFED2                                        
         OI    UNTFED2H+6,X'80'    TRANSMIT                                     
*                                                                               
DIS19040 DS    0H                                                               
         BAS   RE,DISFEDT                                                       
         GOTO1 EDITFEED,DMCB,SVFEEDA,0(R5),0(R2)   BUILD FEED TABLE             
         ZIC   RF,FEEDPRNM                                                      
         LA    RF,1(RF)                                                         
         STCM  RF,1,FEEDPRNM                                                    
*                                                                               
         CLI   UNTCSP,C'N'                                                      
         BNE   DIS19045                                                         
         MVC   UNTTFD1(2),=C'*N'                                                
         OI    UNTTFD1H+6,X'80'                                                 
*                                                                               
         CLI   UNTCSP,C'S'                                                      
         BE    *+12                                                             
         BAS   RE,TRANFLD                                                       
         B     DIS19100                                                         
*                                                                               
DIS19045 XC    KEY,KEY                                                          
         LA    RF,KEY                                                           
         USING FEEDRECD,RF                                                      
         L     RE,NBAIO                                                         
         USING NURECD,RE                                                        
*                                                                               
         MVC   FEEDKID,=X'0A2B'                                                 
         MVC   FEEDKAM,NUKAM       AGY/MEDIA                                    
         MVC   FEEDKNET,NUKNET     NETWORK                                      
         MVC   FEEDKCLT,NUKCLT     CLIENT                                       
         MVC   FEEDKFD(2),8(R2)       FEED                                      
         OC    FEEDKFD,SPACES                                                   
         DROP  RE,RF                                                            
*                                                                               
         GOTO1 AIO,DMCB2,SPT+DIR+HIGH                                           
         CLC   KEY(13),KEYSAVE                                                  
         BNE   DIS19070                                                         
*                                                                               
         GOTO1 AIO,DMCB,SPT+FILE+GET,AIOAREA3                                   
*                                                                               
         GOTO1 VHELLO,DMCB,(C'G',SPTFILE),(X'40',AIOAREA3),0                    
         CLI   12(R1),0            TEST IF FOUND                                
         BNE   DIS19070                                                         
         L     RF,12(R1)                                                        
         USING FEEDSMEL,RF                                                      
*                                                                               
         TM    FEEDSFLG,FEEDSDEL                                                
         BO    DIS19070                                                         
*                                                                               
DIS19050 DS    0H                                                               
         CLI   0(RF),X'40'                                                      
         BNE   DIS19070                                                         
*                                                                               
         CLC   FEEDSMYR,MAPYEAR                                                 
         BNE   *+14                                                             
         CLC   FEEDSMCD,MAPCODE                                                 
         BE    DIS19060                                                         
*                                                                               
         ZIC   RE,1(RF)                                                         
         AR    RF,RE                                                            
         B     DIS19050                                                         
*                                                                               
DIS19060 MVC   12(30,R2),FEEDSMDS  DESCRIPTION                                  
         DROP  RF                                                               
*                                                                               
DIS19070 DS    0H                                                               
         BAS   RE,NEXTFLD                                                       
*                                                                               
DIS19100 DS    0H                                                               
         LA    R5,7(R5)                                                         
         LA    R4,FEEDADLN(R4)                                                  
         BCT   R3,DIS19010                                                      
*                                                                               
DIS19X   DS    0H                                                               
         J     EXXMOD                                                           
         LTORG                                                                  
*                                                                               
* SUB-ROUTINE TO BUILD FEED TABLE                                               
*                                                                               
BLDFTAB  NTR1  BASE=*,LABEL=*                                                   
         XC    FEEDADIS,FEEDADIS                                                
         LA    R3,FEEDADIS                                                      
*                                                                               
*--BUILD TABLE USING 19 ELEMENTS                                                
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'19',AIOAREA1),0                    
         CLI   12(R1),0            TEST IF FOUND                                
         BNE   BLDF100                                                          
         L     R4,12(R1)                                                        
*-CALCULATE NUMBER OF PRODUCTS                                                  
         SR    RE,RE                                                            
         ZIC   RF,1(R4)            ELEMENT LENGTH                               
         SH    RF,=H'3'                                                         
         D     RE,=F'7'                                                         
         LR    R5,RF                                                            
*                                                                               
         NI    MYFLAG,X'FF'-SMAP                                                
         TM    2(R4),X'20'         SECTIONAL MAP?                               
         BZ    *+8                                                              
         OI    MYFLAG,SMAP                                                      
*                                                                               
         LA    R4,3(R4)            POINT TO FIRST PRODUCT                       
*                                                                               
BLDF40   MVI   0(R3),0                                                          
         MVC   1(3,R3),0(R4)                                                    
         LA    R3,FEEDADLN(R3)                                                  
         LA    R4,7(R4)            GET NEXT PRODUCT                             
         MVI   0(R3),X'FF'                                                      
         BCT   R5,BLDF40                                                        
         B     BLDF300                                                          
*                                                                               
*--BUILD TABLE USING 14 ELEMENTS                                                
BLDF100  GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'14',AIOAREA1),0                    
         CLI   12(R1),6            TEST IF FOUND                                
         BE    BLDF200             NO ELEMENT EXISTS EXIT ROUTINE               
         CLI   12(R1),0            TEST IF FOUND                                
         BE    *+6                                                              
         DC    H'0'                TAKE A HIT IF BAD RETURN CODE                
         L     R4,12(R1)                                                        
*-CALCULATE NUMBER OF PRODUCTS                                                  
         SR    RE,RE                                                            
         ZIC   RF,1(R4)            ELEMENT LENGTH                               
         SH    RF,=H'3'                                                         
         D     RE,=F'6'                                                         
         LR    R5,RF                                                            
*                                                                               
         NI    MYFLAG,X'FF'-SMAP                                                
         TM    2(R4),X'40'         SECTIONAL MAP?                               
         BZ    *+8                                                              
         OI    MYFLAG,SMAP                                                      
*                                                                               
         LA    R4,3(R4)            POINT TO FIRST PRODUCT                       
*                                                                               
BLDF140  MVC   0(1,R3),0(R4)                                                    
         LA    R3,FEEDADLN(R3)                                                  
         LA    R4,6(R4)            GET NEXT PRODUCT                             
         MVI   0(R3),X'FF'                                                      
         BCT   R5,BLDF140                                                       
         B     BLDF300                                                          
*                                                                               
BLDF200  CLI   NBPRD,0                                                          
         BE    BLDFEX                                                           
*                                                                               
         MVC   0(1,R3),NBPRD                                                    
         LA    R3,FEEDADLN(R3)                                                  
         MVC   0(1,R3),NBPRD2                                                   
         LA    R3,FEEDADLN(R3)                                                  
         MVI   0(R3),X'FF'                                                      
         B     BLDF300                                                          
*--READ FEED ELEMENTS AND FILL IN REST OF THE TABLE                             
*--FIRST GET THE NATIONAL                                                       
BLDF300  GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'21',AIOAREA1),0                    
         CLI   12(R1),6            TEST IF FOUND                                
         BE    BLDFEX              NO ELEMENT EXISTS EXIT ROUTINE               
         CLI   12(R1),0            TEST IF FOUND                                
         BE    *+6                                                              
         DC    H'0'                TAKE A HIT IF BAD RETURN CODE                
         L     R4,12(R1)                                                        
         USING NUCMLEL,R4                                                       
*                                                                               
         LA    R3,FEEDADIS                                                      
BLDF320  CLI   0(R3),X'FF'                                                      
         BE    BLDF400                                                          
         OC    NUCMPROD,NUCMPROD   ALPHA PRODUCT?                               
         BZ    BLDF325                                                          
         CLC   NUCMPROD,1(R3)                                                   
         BE    BLDF340                                                          
         B     BLDF330                                                          
*                                                                               
BLDF325  CLC   NUCMLPRD,0(R3)                                                   
         BE    BLDF340                                                          
BLDF330  LA    R3,FEEDADLN(R3)                                                  
         B     BLDF320                                                          
*                                                                               
BLDF340  LA    R3,4(R3)                                                         
         TM    MYFLAG,SMAP         SECTIONAL MAP?                               
         BO    BLDF400                                                          
*                                                                               
         TM    NUCMLFL2,X'01'      TEST NO NATIONAL FEED SWITCH                 
         BZ    *+14                                                             
         MVC   0(2,R3),=CL2'**'                                                 
         B     BLDF400                                                          
         MVC   0(2,R3),=CL2'*N'                                                 
         DROP  R4                                                               
*--NOW GET THE FEEDS                                                            
BLDF400  GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'23',AIOAREA1),0                    
         CLI   12(R1),6            TEST IF FOUND                                
         BE    BLDFEX              NO ELEMENT EXISTS EXIT ROUTINE               
         CLI   12(R1),0            TEST IF FOUND                                
         BE    *+6                                                              
         DC    H'0'                TAKE A HIT IF BAD RETURN CODE                
         L     R4,12(R1)                                                        
         USING NUFDCEL,R4                                                       
*                                                                               
BLDF420  CLI   NUFDPPOS,0          DOES PROD POS NUMBER EXIST                   
         BNE   BLDF520             YES WORK OFF PROD POSITION                   
         LA    R3,FEEDADIS                                                      
*                                                                               
BLDF430  CLI   0(R3),X'FF'                                                      
         BE    BLDFEX                                                           
         OC    NUFDPROD,NUFDPROD   ALPHA PRODUCT?                               
         BZ    BLDF435                                                          
         CLC   NUFDPROD,1(R3)                                                   
         BE    BLDF540                                                          
         B     BLDF440                                                          
*                                                                               
BLDF435  CLC   NUFDCPRD,0(R3)      MATCH ON PRODUCT EQUATE                      
         BE    BLDF540                                                          
BLDF440  LA    R3,FEEDADLN(R3)                                                  
         B     BLDF430                                                          
*                                                                               
BLDF520  LA    R3,FEEDADIS                                                      
         ZIC   RE,NUFDPPOS         GET PRODUCT NUMBER                           
         BCTR  RE,0                SUBTRACT ONE                                 
         LTR   RE,RE               CHECK IF FIRST                               
         BZ    BLDF540             MOVE CODE OUT                                
*                                                                               
BLDF530  CLI   0(R3),X'FF'                                                      
         BE    BLDFEX                                                           
         LA    R3,FEEDADLN(R3)                                                  
         BCT   RE,BLDF530                                                       
*                                                                               
BLDF540  LA    R3,4(R3)                                                         
         LA    RE,6                MAXIMUM NUMBER OF FEEDS                      
BLDF560  OC    0(5,R3),0(R3)                                                    
         BZ    BLDF580                                                          
         LA    R3,5(R3)                                                         
         BCT   RE,BLDF560                                                       
         DC    H'0'                TOO MANY FEEDS FOR THIS PRODUCT              
*                                                                               
BLDF580  MVC   0(4,R3),NUFDCFED                                                 
*                                                                               
*--GET NEXT ELEMENT                                                             
         ZIC   RE,NUFDCLEN                                                      
         AR    R4,RE                                                            
         CLI   NUFDCEID,X'23'                                                   
         BE    BLDF420                                                          
*                                                                               
BLDFEX   J     EXXMOD                                                           
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
       ++INCLUDE NEBUYWRK                                                       
         EJECT                                                                  
* UNIT SCREEN                                                                   
*                                                                               
         ORG   BUYLAST                                                          
       ++INCLUDE NEBUYE1D                                                       
         SPACE 2                                                                
* SAVE AREA                                                                     
*                                                                               
         ORG   TWAD+PAGELEN                                                     
SVDATA   DS    CL256                                                            
         ORG   SVDATA                                                           
SVDATE   DS    XL2                 SAVED AIR DATE                               
SVSUB    DS    X                   SAVED SUB-LINE                               
SVFEEDA  DS    CL36                FEED TABLE FOR EDIT (BEFORE CHANGE)          
         DS    CL1                                                              
         EJECT                                                                  
* DSECT TO COVER LOCAL WORKING STORAGE                                          
*                                                                               
TEMPD    DSECT                                                                  
MYRELO   DS    A                                                                
MYPARM   DS    A                                                                
SAVEREG  DS    A                                                                
VDISPLAY DS    V                   V(GENERAL UNIT DISPLAY)                      
VEDIT    DS    V                   V(GENERAL UNIT EDIT)                         
*                                                                               
SAVER3   DS    F                   SAVE REGISTER 3                              
SAVER4   DS    F                   SAVE REGISTER 4                              
*                                                                               
SVSCRPRD DS    F                   SAVE PRODUCT SCREEN FIELD                    
*                                                                               
MYFLAG   DS    XL1                                                              
WASTRI   EQU   X'01'               WAS ORIGINALLY A TRIBACK                     
FORCZERO EQU   X'02'               FORCE 1ST PRODUCT TO 0 %                     
VALFEED  EQU   X'04'               VALIDATING FEEDS                             
SMAP     EQU   X'08'               SECTIONAL MAP                                
DISPR    EQU   X'10'               DISPLAY R FOR RATS (FEED PCT ONLY)           
NOFDES   EQU   X'20'               NO FEED DESCRIPTION                          
DELFEED  EQU   X'40'               DEL FEEDS OFF X'03' ELEM                     
FLPRDA   EQU   X'80'               USE X'19' ELEM FOR ALPHA PRODUCTS            
*                                                                               
UNITDA   DS    XL4                 UNIT RECORD DISK ADDRESS                     
*                                                                               
DATE     DS    XL2                 DATE EXTRACTED FROM ACTION FIELD             
CHARDATE DS    CL6                 DATE FROM ACTION FIELD - YYMMDD              
SUB      DS    X                   SUBLINE EXTRACTED FROM ACTION FIELD          
TIME     DS    X                   SQH (SAVED)                                  
*                                                                               
TRAFFLAG DS    X                   TRAFFIC FLAG (21 ELEMENT)                    
*                                                                               
DDMMYYIP DS    C                   MAKE-GOOD YEAR INPUT                         
NUMPRD   DS    X                   NUMBER OF PRODUCTS                           
PRODINP  DS    C                   WAS PROD INPUTTED FOR THIS LINE              
*                                                                               
SVPRODTB DS    CL48                PRODUCT SAVE TABLE                           
         DS    CL1                 END OF TABLE MARKER                          
SVPRODLN EQU   8                   LENGTH OF EACH ENTRY                         
*--BYTE 1 PRODUCT                                                               
*--BYTE 2-3 PRODUCT PCT                                                         
*--BYTE 4-5 FEED PCT                                                            
*                                                                               
FEEDDIS  DS    CL204               FEED TABLE FOR DISPLAY                       
         DS    CL1                 END OF TABLE MARKER                          
FEEDDSLN EQU   34                  LENGTH OF EACH ENTRY                         
*                                                                               
*--BYTE 0 PRODUCT EQUATE                                                        
*--BYTE 1-3 PRODUCT                                                             
*--BYTE 4-5 PRODUCT PCT                                                         
*--BYTE 6-7 FEED PCT                                                            
*                                                                               
FEEDADIS DS    CL222               FEED TABLE FOR DISPLAY                       
         DS    CL1                 END OF TABLE MARKER                          
FEEDADLN EQU   37                  LENGTH OF EACH ENTRY                         
*--BYTE 1 PRODUCT                                                               
*--BYTE 2-31 FEED CODES                                                         
*                                                                               
FEEDEDA  DS    CL54                FEED TABLE FOR EDIT (AFTER CHANGE)           
         DS    CL1                 END OF TABLE MARKER                          
FEEDEDLN EQU   9                   LENGTH OF EACH ENTRY                         
FEEDPRNM DS    CL1                 PRODUCT NUMBER                               
*--BYTE 1 PRODUCT                                                               
*--BYTE 4 FEED                                                                  
SCANTAB  DS    CL200                                                            
*                                                                               
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
*                                                                               
SVSTAT3  DS    XL1                                                              
*                                                                               
MAPYEAR  DS    XL1                 BINARY SECTIONAL MAP YEAR                    
MAPCODE  DS    CL8                 SECTIONAL MAP CODE                           
*                                                                               
SVFEED   DS    CL2                                                              
SVFDESC  DS    CL30                                                             
*                                                                               
         SPACE 2                                                                
FEEDRECD DSECT                                                                  
       ++INCLUDE SPTRNFEED                                                      
       ++INCLUDE SPGENSMAP                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'180NEBUY42   02/10/11'                                      
         END                                                                    
