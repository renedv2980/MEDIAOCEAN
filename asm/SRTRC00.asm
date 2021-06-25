*          DATA SET SRTRC00    AT LEVEL 021 AS OF 02/27/15                      
*PHASE T12B00A                                                                  
*INCLUDE TWANG                                                                  
         TITLE '$TRACE - DISPLAY TRACE TABLE'                                   
         PRINT NOGEN                                                            
TRACE    CSECT                                                                  
         NMOD1 WRKX-WRKD,*$TRC**,R8                                             
         USING WRKD,RC                                                          
         LR    R2,R1                                                            
         ST    R2,APARMS           SAVE ADDRESS OF PARAMETER LIST               
         USING SRPARMD,R2          R2=A(S/R PARAM LIST)                         
         L     RA,SRPARM1                                                       
         USING SYSFACD,RA          RA=A(SYS FAC LIST)                           
         L     R3,SRPARM6                                                       
         L     RE,SRPARM2          SAVE A(TIA)                                  
         ST    RE,ATIA             ATIA                                         
         L     RE,VSSB                                                          
         MVC   RECLEN,SSBTWAL-SSBD(RE)                                          
         USING SRTRCFFD,R3         R3=A(TWA)                                    
         L     R4,SRPARM4                                                       
         USING COMFACSD,R4         R4=A(COM FAC LIST)                           
         MVC   VHEXIN,CHEXIN                                                    
         MVC   VHEXOUT,CHEXOUT                                                  
         MVC   VSCANNER,CSCANNER                                                
         MVC   VHELLO,CHELLO                                                    
         MVC   VGLOBBER,CGLOBBER                                                
         MVC   VDATCON,CDATCON                                                  
*                                                                               
         LA    R4,SRVMSGH          GUESS WHAT HAPPENS WITHOUT THIS LA           
         USING FLDHDRD,R4                                                       
         NI    SRVIDH+6,X'BF'                                                   
         XC    MSG,MSG                                                          
*                                                                               
         L     RE,VSSB                                                          
         USING SSBD,RE                                                          
*        TM    SSBSYSFL,X'80'      TRACE VALID ON TEST/MEL SYS ONLY             
*        BNO   ERR7                TRACE ONLY FOR TST/MEL SYSTEMS               
         DROP  RE                                                               
*                                                                               
PFKEYS   XC    PFKEY,PFKEY         INIT TO ENTER KEY PRESSED                    
         L     R1,28(R1)           LOAD A(TIOB)                                 
         USING TIOBD,R1            OVERLAY THE A(TIOB)                          
         CLI   TIOBAID,0           IS IT AN ENTER KEY?                          
         BE    SETADDR             YES, PROCEED AS NORMAL                       
         ZIC   R0,TIOBAID          LOAD THE PFKEY PRESSED                       
         CH    R0,=H'12'           PF13 - PF24?                                 
         BNH   *+8                 NO, PF1 - PF12                               
         SH    R0,=H'12'           PF13 -> PF1 .. PF24 -> PF12                  
         STC   R0,PFKEY            STORE THE PFKEY PRESSED                      
*                                                                               
SETADDR  GOTO1 VHELLO,DMCB,C'AFIL'   GET ADDR OF KEY TABLE FOR FILES            
         MVC   VKEYFIL,DMCB          RTNS 1ST PARM=ADDRESS OF TABLE             
         GOTO1 VHELLO,DMCB,C'ADIR'   GET ADDR OF KEY TABLE FOR DIRS             
         MVC   VKEYDIR,DMCB          RTNS 1ST PARM=ADDRESS OF TABLE             
*                                                                               
         XC    INPNUM,INPNUM         SAVED INPUT TRACE NUMBER                   
         XC    UTLID,UTLID           CLEAR USER ID CONNECTED TO                 
         XC    SUBID,SUBID           CLEAR SUBID                                
         XC    DESCR,DESCR           CLEAR DESCRIPTION                          
         MVC   ID,SPACES             TEST ID USER ENTERS                        
*                                                                               
         EJECT                                                                  
***********************************************************************         
*VALIDATE INPUT PARAMETERS                                                      
***********************************************************************         
*                                                                               
* VALIDATE PARAMETER 1:                                                         
*-----------------------                                                        
VALPONE  LA    R4,SRVP1H             PT TO 1ST PARAMETER FIELD                  
         CLI   FLDILEN,0             IF NO INPUT IN 1ST PARM, SKIP              
         BE    VALPTWO               GO TEST 2ND PARAMETER                      
         TM    4(R4),X'08'           NUMERIC INPUT? (TRACE NUMBER?)             
         BNO   VALP05                NO                                         
         CLI   FLDILEN,5             IF NO INPUT IN 1ST PARM, SKIP              
         BH    ERRINV                INVALID INPUT (TOO LARGE)                  
         ZIC   R1,FLDILEN            LENGTH OF INPUT                            
         BCTR  R1,0                  DECR FOR EX-PACK                           
         EX    R1,*+8                CONVERT INPUT FROM CHAR->HEX               
         B     *+10                  SKIP EX PACK                               
         PACK  DUB,SRVP1(0)          PACK                                       
         CVB   R1,DUB                CONVERT TO BINARY                          
         STH   R1,INPNUM             SAVE INPUT TRACE NUMBER                    
         B     VALPTWO               VALIDATE PARM2                             
*                                                                               
* VALID INPUTS FOR SRVP1 LENGTH=1                                               
VALP05   CLI   FLDILEN,1             IS INPUT LENGTH=1 CHARACTER?               
         BH    VALP10                INPUT LENGTH >1                            
         CLI   SRVP1,C'I'            REQUEST FOR A TEST BUFFER?                 
         BE    VALPTWOB              YES                                        
         CLI   SRVP1,C'P'            REQUEST FOR PRINTING?                      
         BE    VALPTWOC              YES                                        
         B     ERRINV                ANY OTHER INPUT IS INVALID                 
*                                                                               
VALP10   CLI   FLDILEN,2             IS PARM1 LENGTH=2 CHARS?                   
         BNE   VALP15                NO INPUT IS GREATER                        
         CLC   =C'ON',SRVP1          TURN ON TRACE BIT                          
         BNE   ERRINV                ANY OTHER INPUT IS INVALID                 
         MVI   ONOFF,X'00'           SET BIT                                    
         MVC   MSG,SPACES            CLEAR OLD MESSAGE                          
         MVC   MSG(22),=C'TRACE HAS BEEN ENABLED'                               
         B     VALP17                                                           
*                                                                               
VALP15   CLI   FLDILEN,3             3 CHARS INPUT IN PARM1?                    
         BNE   VALP20                NO, INP LENGTH > 3                         
         CLC   =C'OFF',SRVP1         TRACE OFF REQUESTED?                       
         BNE   VALP20                NO, MAYBE TEST ID SPECIFIED                
         MVI   ONOFF,HDRSUSQ         SET TRACE BIT OFF                          
         MVC   MSG,SPACES            CLEAR OLD MESSAGE                          
         MVC   MSG(24),=C'TRACE HAS BEEN SUSPENDED'                             
*                                                                               
VALP17   BAS   RE,TESTADR            GET ADDR OF TEST BUFFER                    
         MVC   DMCB(4),=C'SUSP'      SET SUSPEND BIT                            
         BAS   RE,SETBITS            SET THE SUSPEND BIT                        
         MVC   SRVMSG,MSG                                                       
         OI    SRVMSGH+6,X'80'       TRANSMIT MSG                               
         OI    SRVMSGH+6,X'08'       HIGH INTENSITY                             
         B     VALPTWO               DISPLAY WHAT'S IN TRACE BUFFER NOW         
*                                                                               
VALP20   CLI   FLDILEN,6             MAX INPUT IS 'I=XXXX' =6 CHARS             
         BH    ERRINV                IF MORE, INVALID INPUT                     
         CLC   =C'I=',SRVP1          1ST 2 CHARS MUST BE 'I='                   
         BNE   ERRINV                IF NOT, ERROR                              
         ZIC   R1,FLDILEN            LENGTH OF INPUT TO GET ID                  
         SH    R1,=H'3'              -2 FOR 'I=' -1 FOR EXMVC                   
         EXMVC R1,ID,SRVP1+2         SAVE  REQST TEST ID                        
         OC    ID,SPACES             BLANK PAD WITH SPACES                      
         BAS   RE,IDSRCH             LOOK FOR ID IN TEST TABLE                  
         OC    ID,ID                 IF ID IS EMPTY, REQUEST NOT FOUND          
         BZ    ERRDEF                ID NOT DEFINED                             
         XC    SRVP1,SRVP1           CLEAR 1ST PARAM FIELD                      
         MVI   FLDILEN,0             RESET LENGTH                               
         OI    FLDOIND,X'80'         TRANSMIT CLEARED FIELD                     
         B     PROCESS               GO DISPLAY THE TRACE                       
         SPACE 2                                                                
*                                                                               
* VALIDATE PARAMETER 2:                                                         
*------------------------                                                       
VALPTWO  LA    R4,SRVP2H             PT TO 2ND PARAMETER                        
         CLI   FLDILEN,0             ANY INPUT IN 2ND PARAMETER?                
         BE    PROCESS               PROCESS TRACE                              
         CLI   FLDILEN,1             EXTENDED MODE REQUESTED?                   
         BH    VALP70                NO                                         
         CLI   SRVP2,C'X'            EXTENDED MODE REQUESTED                    
         BNE   ERRINV                NO, INVALID INPUT                          
         MVI   EXTN,HDREXTQ          X'10'= EXTENDED                            
         B     VALP73                PROCESS BIT CHANGE IN TRC TABLE            
*                                                                               
VALP70   CLI   FLDILEN,2             WAS EXTENDED OFF REQUESTED?                
         BNE   ERRINV                NO, ANYTHING ELSE IS INVALID               
         CLC   =C'NX',SRVP2          EXTENDED OFF?                              
         BNE   ERRINV                NO, INVALID INPUT                          
         MVI   EXTN,X'00'            X'10'= EXTENDED                            
*                                                                               
VALP73   BAS   RE,TESTADR            GET ADDR OF TEST BUFFER                    
         MVC   DMCB(4),=C'EXTN'      SET EXTENDED BIT                           
         BAS   RE,SETBITS            SET THE SUSPEND BIT                        
         XC    SRVP2,SRVP2           CLEAR 2ND PARAM FIELD                      
         OI    FLDOIND,X'80'         TRANSMIT CLEARED FIELD                     
         B     PROCESS               PROCESS TRACE                              
*                                                                               
VALPTWOB DS    0H                    GET HERE IF PARM2=TEST ID                  
         LA    R4,SRVP2H             PT TO 2ND PARAMETER                        
         CLI   FLDILEN,0             ANY INPUT?                                 
         BE    ERRMIS                NO, THIS IS 'I', INPUT REQUIRED            
*                                                                               
VALP80   CLI   FLDILEN,4             MAX 4 CHARS                                
         BH    ERRINV                INVALID INPUT                              
         ZIC   R1,FLDILEN            GET LENGTH OF INPUT                        
         BCTR  R1,0                  DECR FOR EXMVC                             
         EXMVC R1,ID,SRVP2           SAVE TEST ID REQUESTED                     
         OC    ID,SPACES             BLANK PAD WITH SPACES                      
         BAS   RE,IDSRCH             LOOK FOR ID IN TEST TABLE                  
         OC    ID,ID                 IF ID IS EMPTY, REQUEST NOT FOUND          
         BZ    ERRDEF                ID NOT DEFINED                             
         XC    SRVP2,SRVP2           CLEAR 2ND PARAM FIELD                      
         MVI   FLDILEN,0             RESET LENGTH                               
         OI    FLDOIND,X'80'         TRANSMIT CLEARED FIELD                     
         B     PROCESS               GO DISPLAY THE TRACE                       
*                                                                               
VALPTWOC DS    0H                    GET HERE IF PARM1=P- PARM2=USR ID          
         LA    R4,SRVP2H             PT TO 2ND PARAMETER                        
         CLI   FLDILEN,0             ANY INPUT?                                 
         BNE   VALP85                YES,                                       
         L     R9,SRPARM3            INPUT REQRD IF NOT CNCTD                   
         USING UTLD,R9               USE UTL DSECT                              
         OC    TUSER,TUSER           MUST BE CONNECTED                          
         BZ    ERRCNCT               ERROR, MUST CONNECT                        
         BAS   RE,TESTADR            ARE WE CONNECTED TO A TEST BUFFER?         
         B     VALPTHR               YES,  SEE IF DESCRIPTION ENTERED           
         DROP  R9                                                               
*                                                                               
VALP85   GOTO1 VSCANNER,DMCB,SRVP2H,SCANTBL                                     
         MVC   USRIDNAM,SCANTBL+12   GET PARM2=USERID                           
         BAS   RE,USRVAL             VALIDATE USER ID IN CTFILE                 
         OC    USRIDNAM,USRIDNAM     IF USR ID EMPTY:REQUEST NOT FOUND          
         BZ    ERRUSID               ID NOT DEFINED                             
*                                                                               
         CLI   SCANTBL+32,0          WAS USRID,TESTID INPUT?                    
         BNE   VALP87                NO, CHECK 3RD PARAMETER                    
*                                                                               
         BAS   RE,TESTADR            SEE IF WE HAVE A TEST ID                   
         B     VALPTHR               YES WE DO, SEE IF THERE'S A DESC           
*                                                                               
VALP87   CLI   SCANTBL+32,4          MAX 4 CHARS                                
         BH    ERRINV                INVALID INPUT                              
         MVC   ID,SCANTBL+32+12      SAVE TEST ID                               
         BAS   RE,IDSRCH             LOOK FOR ID IN TEST TABLE                  
         OC    ID,ID                 IF ID IS EMPTY, REQUEST NOT FOUND          
         BZ    ERRDEF                ID NOT DEFINED                             
         MVC   SUBID,ID              SAVE SUBID                                 
         B     VALPTHR               ID OKAY, TEST NEXT PARAM                   
*                                                                               
* VALIDATE PARAMETER 3:                                                         
*------------------------                                                       
VALPTHR  LA    R4,SRVP3H             PT TO 3RD PARAMETER- TEST ID?              
         CLI   FLDILEN,0             ANY INPUT IN 3RD PARAMETER?                
         BE    PROCESS               NO INPUT, EXIT                             
         ZIC   R1,FLDILEN            LENGTH OF DESCRIPTION                      
         EXMVC R1,DESCR,SRVP3        MOVE IN DESCR                              
         B     PROCESS                                                          
*                                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESS - VALIDATE INPUT TRACE NUMBER AGAINST BUFFER TRACE NUMBER             
*         - IF ABOVE 2 CHECK OUT, DECIDE WHETHER THIS IS:                       
*         - PRINT REQUEST   - GO TO PROCPRT                                     
*         - DISPLAY REQUEST - DO    PROCDSP                                     
***********************************************************************         
         SPACE 1                                                                
PROCESS  LA    R4,SRVP1H             PT TO 1ST PARM (IN CASE ERROR)             
         BAS   RE,TESTADR            MAKE SURE WE HAVE TEST BUFFER              
         OC    INPNUM,INPNUM         WAS A VALUE REQUESTED?                     
         BNZ   *+10                  YES                                        
         MVC   INPNUM,TRCNUM         NO, DEFAULT IS START OF BUFFER             
         CLC   INPNUM,TRCNUM         IS REQUEST VALUE < BUFFER START?           
         BL    ERR5                  YES, ERROR, NOT IN BUFFER                  
         LH    R1,INPNUM             NO, GET OFFSET INTO BUFFER                 
         SH    R1,TRCNUM             R1 NOW IS DISPLACEMENT IN BUFFER           
         STH   R1,START              SAVE AWAY REQSTD ENTRY(OR 0)               
*                                                                               
         CLI   SRVP1,C'P'            PRINT REQUEST?                             
         BE    PROCPRT               YES- GO HANDLE IT                          
*                                                                               
*PROCDSP- DISPLAY TRACE ON SCREEN                                               
*--------------------------------                                               
PROCDSP  DS    0H                                                               
         BAS   RE,GETHDR             GET HEADER-IS REQSTD ENT IN BUFFR          
         LH    RE,START                                                         
         OC    INTVL,INTVL           INTERVAL DEFINED?                          
         BZ    *+12                                                             
         L     RF,INTVL                                                         
         MR    RE,RE                                                            
         LR    RE,RF                                                            
         AH    RE,TRCNUM             ADD ON STARTING OFFFSET                    
         ST    RE,TEMP                                                          
         XC    SRVP1,SRVP1           NO, JUST A DISPLAY.CLEAR FLD 1             
         EDIT  (2,TEMP+2),(4,SRVP1),0,ALIGN=LEFT,ZERO=NOBLANK                   
         OI    SRVP1H+6,X'80'        TRANSMIT FIELD                             
         OI    SRVP1H+6,X'40'        POSITION CURSOR OVER INVALID FLD           
         CLI   SYST,X'06'            FOR ACC, DISPLAY=EXTENDED                  
         BNE   PROC05                NOT ACC                                    
         MVI   EXTN,HDREXTQ                                                     
         MVC   DMCB(4),=C'EXTN'      ACC, EXTENDED MODE                         
         BAS   RE,SETBITS            SET BIT IN TABLE                           
*                                                                               
PROC05   BAS   RE,INFOLN             SET INFORMATION LINE                       
         SPACE 2                                                                
*--TEST PF KEYS HIT (PF7=UP PF8=DOWN REST=ENTER)                                
         CLI   PFKEY,7               PAGE UP?                                   
         BNE   PROC20                NO, ALL OTHER PFKEYS=ENTER KEY             
         LH    R1,START              GET THE INITIAL OFFSET                     
         ZIC   R0,ENTYS              NUMBER ENTRIES ON SCREEN                   
         SR    R1,R0                 DECR BY # ENTRIES ON SCREEN                
         SR    R1,R0                 DECR AGAIN TO DO PG BACK                   
         BNM   *+6                   IF NON-NEG,NEW OFFSET IS OK                
         SR    R1,R1                 ELSE, START DISPL FROM TOP BUFFER          
         STH   R1,START              SAVE NEW OFFSET                            
         SPACE 2                                                                
*--CLEAR SCREEN AND INIT SOME VARS FOR DISPLP ACTIVITY                          
PROC20   LA    R7,SRVLN1H            PT TO 1ST OUTPUT LINE ON SCRN              
         ST    R7,SCRADR             SAVE SCREEN ADDRESS                        
         MVI   PRNTD,0               # ENTRIES PRINTED ALREADY=0                
         BAS   RE,CLRSCR             CLEAR THE SCREEN                           
         B     DISPLP                NOW DO THE REAL STUFF                      
         EJECT                                                                  
*                                                                               
* *******************************************************************           
* PROCPRT- SEND TRACE TO PRINT QUEUE                                            
* *******************************************************************           
         SPACE 1                                                                
PROCPRT  CLI   PFKEY,0               IF A PFKEY WAS HIT, COMPLAIN               
         BE    *+12                  ENTER KEY HIT, OKAY                        
         LA    R4,SRVP4H             PT PAST INPUT FIELDS                       
         B     ERRPFKY               'PLEASE HIT ENTER TO PRT TRACE'            
*                                                                               
         LA    R7,PRTLINE            PT TO THE PRINT LINE                       
         MVI   LINES,3               REPORT ALWAYS PRINTS EXTENDED              
         MVI   ENTYS,3               PRINT EXTENDED                             
*                                                                               
         MVI   PQCTL,0               OPEN QUEUE                                 
         BAS   RE,PRINT              OPEN QUEUE                                 
         BE    PROCPT1               NO ERROR OPENING PRT QUEUE                 
         CLI   ERRNUM,3                                                         
         BE    ERREOF                END OF FILE ERROR                          
         CLI   ERRNUM,4              FORMAT ERROR                               
         BE    ERRFMT                                                           
         CLI   ERRNUM,5              DISK ERROR                                 
         BE    ERRDSK                                                           
         B     ERRPRT1               ERROR OPENING PRT QUEUE                    
*                                                                               
PROCPT1  BAS   RE,GETHDR             GET TRACE BUFFER HEADER                    
         BAS   RE,INFOLN             SET INFORMATION LINE                       
         BAS   RE,PRTLOOP            PRINT TRACE ENTRIES IN BUFFER              
*                                                                               
         MVI   PQCTL,X'FF'           CLOSE QUEUE                                
         BAS   RE,PRINT              CLOSE QUEUE                                
         BNE   ERRPRT3               ERROR CLOSING PRT QUEUE                    
*                                                                               
         XC    DQUID,DQUID                                                      
         MVC   MSG,SPACES                                                       
         MVC   MSG(8),=C'REPORT ('                                              
         MVC   DQUID+1(3),SUBID                                                 
         MVI   DQUID+4,C','                                                     
         LH    R0,REPNO                                                         
         EDIT  (R0),(5,DQUID+5),ALIGN=LEFT                                      
         LA    RF,5                                                             
         AR    RF,R0                 LENGTH OF EDITTED VALUE                    
         STC   RF,DQUID              SAVE LENGTH IN 1ST BYTE OF DQUID           
*                                                                               
         EXMVC RF,MSG+8,DQUID+1                                                 
         LA    R1,MSG+8              PT TO MSG FIELD                            
         AR    R1,RF                                                            
         MVC   0(10,R1),=C') SENT TO '                                          
         MVC   10(6,R1),PRTQID                                                  
         MVC   SRVMSG,MSG                                                       
         OI    SRVMSGH+6,X'80'       TRANSMIT MESSAGE                           
         OI    SRVP1H+6,X'40'        POSITION CURSOR OVER 1ST PARM              
         BAS   RE,READTWA            SAVE OFF GLOBAL VAR (FOR DQU)              
*                                                                               
PROCPRTX B     XIT                   DONE                                       
* *******************************************************************           
* READTWA -- THIS WAS TAKEN FROM SRCPY00 AND WAS SLIGHLY ALTERED                
* *******************************************************************           
*                                                                               
READTWA  ST    RE,RTNADR           SAVE RETURN ADDRESS                          
         L     R2,APARMS             RESTORE ADDRESS OF PARM LIST               
         L     R9,SRPARM3          ADDRESS OF UTL                               
         USING UTLD,R9             USE DSECT                                    
         MVI   ERRNUM,2                                                         
         OC    TUSER,TUSER         MUST BE CONNECTED                            
         BZ    READTWX                                                          
         MVI   ERRNUM,6                                                         
         OC    TPRNT,TPRNT         MUST BE A TERMINAL                           
         BNZ   READTWX                                                          
*                                                                               
         XC    FILLS,FILLS         FILLS                                        
         XC    DMCB+8(4),DMCB+8    READ TWA 0                                   
         MVC   DMCB+10(2),TNUM     SET TERM NUMBER                              
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),RECLEN                                                
         DROP  R9                                                               
         GOTO1 VDATAMGR,DMCB,=C'DMREAD',=C'TEMPSTR',,TWA0                       
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                DIE IF CANT READ                             
         L     R0,ATIA             R0=TIA                                       
         LA    RE,TWA0             RE=TWA FROM TEMPSTR0                         
         LH    R1,RECLEN                                                        
         LR    RF,R1               MOVE TWA0 TO TIA                             
         MVCL  R0,RE                                                            
         L     R0,ATIA             SAVE A(GLOBALS)                              
         AH    R0,=Y(CHKPTGLD)                                                  
         ST    R0,AGLOBALS                                                      
*                                                                               
CHKTWA   TM    TWA0+65,X'20'                                                    
         BZ    ERRTWA              FIRST FIELD MUST BE PROTECTED                
         CLI   TWA0+64,X'44'                                                    
         BNE   *+12                                                             
         LA    RE,TWA0+132         AND 60 BYTES LONG                            
         B     CHKTWA1                                                          
         CLI   TWA0+64,X'4C'                                                    
         BNE   ERRTWA                                                           
         LA    RE,TWA0+140                                                      
CHKTWA1  CLI   0(RE),X'19'         SECOND FIELD MUST BE 17 BYTES LONG           
         BE    *+12                                                             
         CLI   0(RE),X'21'                                                      
         BNE   ERRTWA                                                           
*                                                                               
         MVC   DMCB+8(2),=C'PU'                                                 
         MVC   DMCB+10(2),FILLS                                                 
         CLI   DMCB+10,C' '                                                     
         BNE   *+8                                                              
         MVI   DMCB+10,0                                                        
         CLI   DMCB+11,C' '                                                     
         BNE   *+8                                                              
         MVI   DMCB+11,0                                                        
*                                  TRANSLATE TWA                                
         GOTO1 =V(TWANG),DMCB,TWA0,TBLOCK,RR=RB                                 
*                                  TRANSLATE TWA                                
*                                                                               
INFO2    ZIC   RF,DQUID              LENGTH OF REPT,# FIELD                     
         GOTO1 VGLOBBER,DMCB,(X'80',=C'PUTD'),DQUID+1,(RF),1,          X        
               (X'80',AGLOBALS)                                                 
         CLI   DMCB+8,0                                                         
         BE    INFO2A                                                           
         DC    H'0'                                                             
*                                                                               
INFO2A   L     RE,SRPARM3                                                       
         USING UTLD,RE                                                          
         XC    DMCB+8(4),DMCB+8    WRITE TWA 0                                  
         MVC   DMCB+10(2),TNUM     SET TERM NUMBER                              
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),RECLEN                                                
         L     R5,ATIA                                                          
         DROP  RE                                                               
         GOTO1 VDATAMGR,DMCB,=C'DMWRT',=C'TEMPSTR',,(R5)                        
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                DIE IF CANT READ                             
*                                                                               
READTWX  L     RE,RTNADR             RESTORE RETURN ADDRESS                     
         BR    RE                    RETURN AND XIT CLEANLY                     
         EJECT                                                                  
* *******************************************************************           
* DISPLP- MAJOR ACTIVITY OF PROGRAM: LOOPS THRU BUFFER AND DISPLAYS             
*         THE ENTRIES.                                                          
* *******************************************************************           
         SPACE 1                                                                
DISPLP   CLC   START,HEADER+HDRENTQ+2  ALL ENTRIES DSPLYD?                      
         BNL   XIT                   IF ALL ENTRIES DISPLAYED,DONE              
         CLC   PRNTD,ENTYS           IS THE SCREEN FULL?                        
         BNL   XIT                   YES, WE'RE DONE                            
         CLI   LINES,2               SEE IF ENTRY WILL FIT ON SCREEN            
         BNE   *+12                  NOT A 4- LINE MAX ENTRY                    
         LA    RF,SRVLN16H           ADDR OF LAST START POSTN FOR ENTRY         
         B     *+8                   SKIP 6-LINE ASSIGNMENT                     
         LA    RF,SRVLN14H           ADDR OF LAST STR POSTN FOR 6LN ETY         
         C     RF,SCRADR             IS NEXT AVAIL LINE BEYOND LIMIT?           
         BL    XIT                   YES, DON'T PRINT ANY MORE ENTYS            
*                                                                               
         BAS   RE,GETENTY            IF WE GET HERE,RQST OK,GET ENTY            
         BAS   RE,GETDISP            GET SCREEN DISPS F(CMD)                    
         BAS   RE,DISPSCR            DISPLAY ENTRY ON SRCEEN                    
*                                                                               
         LH    R1,START              TEST IF LAST ENTRY IN BUFFER               
         LA    R1,1(R1)              SET TO NEXT ENTRY                          
         STH   R1,START              SAVE AWAY                                  
         CH    R1,HEADER+HDRENTQ+2   ALL ENTRIES DSPLYD?                        
         BL    *+6                   NO, CHANGE SRVP1 TO NEXT ENTRY             
         BCTR  R1,0                  YES, KEEP LAST ENTRY ON SCRN               
         LR    RE,R1                                                            
         OC    INTVL,INTVL           INTERVAL DEFINED?                          
         BZ    *+12                                                             
         L     RF,INTVL                                                         
         MR    RE,RE                                                            
         LR    RE,RF                                                            
         AH    RE,TRCNUM             ADD ON STARTING OFFFSET                    
         ST    RE,TEMP                                                          
         EDIT  (2,TEMP+2),(4,SRVP1),0,ALIGN=LEFT,ZERO=NOBLANK                   
         OI    SRVP1H+6,X'80'        TRANSMIT FIELD                             
         OI    SRVP2H+6,X'40'        POSITION CURSOR PAST FIELD                 
         ZIC   R1,PRNTD              INCREMENT NUMBER ENTRIED PRINTED           
         LA    R1,1(R1)              INCR                                       
         STC   R1,PRNTD              SAVE COUNTER                               
         B     DISPLP                LOOP TO DISPLAY FULL SCREEN                
         EJECT                                                                  
*                                                                               
***********************************************************************         
* PRTLOOP - LOOP THRU AND PRINT OUT ENTRIES IN TRACE BUFFER.                    
***********************************************************************         
         SPACE 1                                                                
PRTLOOP  NTR1                                                                   
         MVI   PQCTL,X'01'           ADD LINE TO PRT QUEUE                      
         XC    LNCNT,LNCNT           CLEAR LINE COUNTER                         
         BAS   RE,GETTIME            GET TIME OF DAY                            
         BAS   R3,HEADUP                                                        
*                                                                               
PRTLP    CLC   START,HEADER+HDRENTQ+2 ALL ENTRIES PRINTED?                      
         BNL   PRTLOOPX              YES, RETURN TO CALLER(PROCPRT)             
         BAS   RE,GETENTY            GET A TRACE ENTRY FROM BUFFER              
         BAS   RE,GETDISP            GET # BYTES TO PRT DEPNDG ON CMD           
         BAS   RE,PRTENTY            SET UP PRT LINE AND PRINT ENTY             
         LA    R7,PRTLINE            PT TO PRINT LINE                           
         XC    PRTLINE,PRTLINE       CLEAR PRT LINE                             
         CLI   LNCNT,55              MAX NUMBER LINES ON A PAGE                 
         BL    PRTLP10               NO, KEEP PRINTING ENTRIES                  
         MVI   LNCNT,X'FF'           FORCE A PAGE BREAK                         
         BAS   RE,PRINT              NEW PAGE                                   
         BAS   R3,HEADUP                                                        
*                                                                               
PRTLP10  BAS   RE,PRINT              BLANK LINE BETWN TRC ENTRIES               
*                                                                               
         LH    R1,START              BUMP # ENTRIES PROCESSED SO FAR            
         LA    R1,1(R1)              BUMP ENTRY COUNTER                         
         STH   R1,START              SAVE FOR NEXT READ                         
         B     PRTLP                 KEEP LOOPING TILL ALL ENTYS PRNTD          
*                                                                               
PRTLOOPX B     XIT                   DONE PRT (RETURN TO CLOSE QUEUE)           
         EJECT                                                                  
*                                                                               
***********************************************************************         
* TESTADR - DO WE HAVE ADDRESS OF TEST TABLE ENTRY?                             
***********************************************************************         
TESTADR  L     R9,SRPARM3            MAKE SURE/GET A(TSTAB ENTRY)               
         USING UTLD,R9               USE UTL DSECT                              
         TM    TTEST,X'10'           DO WE HAVE A(TSTAB)                        
         BNO   ERR1                  NO, 'I=XXX' NOT SET- ERROR                 
         ICM   R5,15,TACCS           R4=ADDRESS OF TSTTAB ENTRY                 
         OC    UTLID,UTLID           IF UTL EMPTY, SET IT                       
         BNZ   *+10                  ALREADY SET DON'T OVERWRITE                
         MVC   UTLID,TUSER           SAVE USER ID (PRG CNCTD TO)                
         MVC   SUBID,2(R5)           SAVE NAME OF TEST ID                       
         MVC   SYST,TOVSYS           SAVE SYSTEM NUMBER                         
         DROP  R9                                                               
*                                                                               
         USING TSTTABD,R5            NOW USE TEST TABLE DSECT                   
         MVC   ATSTRC,TSTTRC         SAVE A(TRACE TABLE)                        
         MVC   TRCNUM,TSTTRCIO       START COUNT FOR IO TRACE                   
         MVC   TSTBUFNM,TSTTACCS     SAVE NAME OF TEST BUFFER                   
         DROP  R5                                                               
         BR    RE                    RETURN TO CALLER                           
         EJECT                                                                  
*                                                                               
* *********************************************************************         
*GETHDR: GETS THE BUFFER HEADER AND PUTS IT IN TRCHDR. ALSO TESTS TO            
*        SEE IF THE ENTRY REQUESTED (IN START) IS IN THE BUFFER.                
* *********************************************************************         
         SPACE 1                                                                
GETHDR   ST    RE,RTNADR             SAVE RETURN ADDRESS                        
         L     R6,ATSTRC             ADDR OF TRACE TABLE                        
         LA    R2,HDRLNQ             TRACE TABLE HEADER LENGTH                  
         XC    INTVL,INTVL           INIT INTERVAL HOLDER                       
         BAS   RE,RDTRTBL            GO READ TRACE TABLE 31-BIT MODE            
         MVC   HEADER,WORK           SAVE HEADER                                
         LA    R6,HEADER             PT TO HEADER TO PUT DSECT OVER             
         USING HDRD,R6               USE HEADER DSECT                           
         ICM   R1,15,HDRENTY         R1=# OF LAST ITEM IN BUFFER                
         BZ    ERR6                  NO ENTRIES IN BUFFER                       
         OC    TRCNUM,TRCNUM         WAS THERE A TRACE START AT?                
         BZ    GETHDR2               NO. HDRENTY=# ENTRIES IN BUFFER            
         SH    R1,TRCNUM             COMPUTE # ENTRIES IN BUFFER                
         ST    R1,HDRENTY            SAVE AWAY                                  
*                                                                               
GETHDR2  LH    R1,START              GET REQUESTED ENTRY                        
*                                                                               
*****    LA    RF,5                                                             
*****    STCM  RF,15,HDRINVL                                                    
*                                                                               
         OC    HDRINVL,HDRINVL       WAS AN INTERVAL REQUESTED                  
         BZ    GETHDR3                                                          
         LR    RF,R1                                                            
         SR    RE,RE                 YES, CALC REAL START (W/OUT INVL)          
         ICM   R1,15,HDRINVL                                                    
         ST    R1,INTVL                                                         
         DR    RE,R1                                                            
         LR    R1,RF                 PUT DIVISOR (ROUNDED) INTO START           
         STH   R1,START                                                         
*                                                                               
GETHDR3  LA    R1,1(R1)              ADJUST FOR TESTING                         
         C     R1,HDRENTY            MAKE SURE IN BUFFER                        
         BH    ERR5                  NO, ERROR, NOT IN BUFFER                   
*                                                                               
         MVI   LINES,2               DEFAULT=2 LINES PER ENTRY                  
         MVI   ENTYS,4               REGULAR DISPLAY=4 ENTRIES                  
         TM    HDREXTN,HDREXTQ       EXTENDED DISPLAY?                          
         BNO   GETHDR4               NO                                         
         MVI   LINES,3               YES SET TO MORE LINES                      
         MVI   ENTYS,3               EXTEND  DISPLAY=3 ENTRIES                  
*                                                                               
GETHDR4  XC    ONOFF,ONOFF           TRACE ON IS DEFAULT                        
         TM    HDRSUSP,HDRSUSQ       IS TRACE SUSPENDED?                        
         BNO   GETHDR8               NO                                         
         MVI   HDRSUSP,HDRSUSQ       YES, INDICATE SO                           
*                                                                               
GETHDR8  L     RE,RTNADR             RESTORE RETRURN ADDRESS                    
         BR    RE                    RETURN TO CALLER                           
         DROP  R6                    RELEASE HEADER DSECT                       
         EJECT                                                                  
*                                                                               
* ********************************************************************          
*GETENTY:GETS THE ENTRY DEFINED BY START AND PUTS IT IN TRCENTY                 
* ********************************************************************          
         SPACE 1                                                                
GETENTY  ST    RE,RTNADR             SAVE RETURN ADDRESS                        
         L     R6,ATSTRC             PT TO BEG OF TABLE                         
         AH    R6,=Y(HDRLNQ)         R6=ADDR OF 1ST ENTRY IN BUFFER             
         LA    R1,ENTLNQ             LENGTH OF TRACE ENTRY                      
         MH    R1,START              COMPUTE OFFSET                             
         AR    R6,R1                 ADJUST ADDR TO PT TO ENTRY                 
         LA    R2,ENTLNQ             LENGTH OF TRACE ENTRY                      
         BAS   RE,RDTRTBL            READ THE ENTRY                             
         MVC   ENTRY,WORK            SAVE TRACE ENTRY                           
         L     RE,RTNADR             RESTORE RETURN ADDRESS                     
         BR    RE                    RETURN TO CALLER                           
         EJECT                                                                  
*                                                                               
* ********************************************************************          
*GETDISP:GETS THE INPUT AND OUTPUT DISPLAY LENGTHS DEPENDING ON WHAT            
*        THE COMMAND IN THE ENTRY IS. THESE LENGTHS DEFINE THE NUMBER           
*        OF BYTES WHICH WILL BE READ FROM THE ENTRY.                            
*        COMMAND          INPUT DISPLAY             OUTPUT DISPLAY              
*        GETREC/PUTREC    DISK ADDRESS=4 BYTES      RECORD=40/60(EXTN)          
*        ADDREC           RECORD=42/63(EXTN)        DISK ADDRESS=4 BYT          
*        DMRDHI/DMRSEQ    KEY-IN=32 BYTES           KEY-OUT=32 BYTES            
*        DMREAD/DMWRT     PAGE-NUMBER=1 BYTE        PAGE-DATA=42/63             
*        TEMPEST/TEMPSTR  PAGE-NUMBER=1 BYTE        PAGE-DATA=42/63             
* ********************************************************************          
         SPACE 1                                                                
GETDISP  ST    RE,RTNADR             SAVE RETURN ADDRESS                        
         LA    R6,ENTRY              PT TO ENTRY                                
         USING ENTD,R6               USE DSECT FOR THE ENTRY                    
         MVI   INDISP,60             DEFAULT-MAX KEY LENGTH                     
         MVI   OUTDISP,60            DEFAULT-MAX KEY LENGTH                     
*                                                                               
         CLC   =CL3'DIR',ENTFILE+3   SEE IF THIS IS A DIR                       
         BE    GETDSP1A              YES, ONLY SEARCH DIR TABLE                 
*                                                                               
* SEARCH FILENAME LIST                                                          
         L     R5,VKEYFIL            PT TO KEY TABLE- FILENAME                  
GETDSP1  CLI   0(R5),X'FF'           END OF TABLE REACHED?                      
         BE    GETDSP2               YES, FILE NAME NOT IN LIST                 
         ZIC   R1,0(R5)              GET VARIABLE LENGTH OF FILENAME            
         EX    R1,*+8                EX-COMPARE AGAINST ENTRY                   
         B     *+10                  BRANCH AROUND EX                           
         CLC   1(0,R5),ENTFILE       DOES NAME MATCH                            
         BE    *+12                  YES, GET KEY LENGTH                        
         LA    R5,13(R5)             NO, BUMP TO NEXT TBL ENTRY                 
         B     GETDSP1               LOOP UNTIL FOUND/END OF TABLE              
         MVC   ENTFILE,1(R5)         SAVE PROPER NAME                           
         MVC   INDISP,11(R5)         SAVE KEY LENGTH                            
         MVC   OUTDISP,11(R5)        SAVE KEY LENGTH                            
         B     GETDSP2                                                          
* SEARCH DIR LIST                                                               
GETDSP1A L     R5,VKEYDIR            GET ADDRESS OF KEY-DIR TABLE               
GETDSP1B CLI   0(R5),X'FF'           END OF TABLE REACHED?                      
         BE    GETDSP2               YES, FILE NAME NOT IN LIST                 
         ZIC   R1,0(R5)              GET VARIABLE LENGTH OF FILENAME            
         EX    R1,*+8                EX-COMPARE AGAINST ENTRY                   
         B     *+10                  BRANCH AROUND EX                           
         CLC   1(0,R5),ENTFILE       DOES NAME MATCH                            
         BE    *+12                  YES, GET KEY LENGTH                        
         LA    R5,11(R5)             NO, BUMP TO NEXT TBL ENTRY                 
         B     GETDSP1B              LOOP UNTIL FOUND/END OF TABLE              
         MVC   INDISP,9(R5)          SAVE KEY LENGTH                            
         MVC   OUTDISP,9(R5)         SAVE KEY LENGTH                            
         MVC   ENTFILE,1(R5)         SAVE FULL DIRECTORY NAME                   
*                                                                               
GETDSP2  LA    R5,CMDTABLE           GET ADDRESS OF COMMAND TABLE               
GETDSP2B CLI   0(R5),X'FF'           END OF TABLE REACHED?                      
         BE    GETDSP2C              YES, FILE NAME NOT IN LIST                 
         CLC   0(5,R5),ENTCMD        DOES NAME MATCH                            
         BE    *+12                  YES, GET KEY LENGTH                        
         LA    R5,9(R5)              NO, BUMP TO NEXT TBL ENTRY                 
         B     GETDSP2B              LOOP UNTIL FOUND/END OF TABLE              
         MVC   ENTCMD(7),0(R5)       SAVE CORRECT FULL COMMAND                  
         MVI   ENTCMD+7,C' '         ENTCMD=8BYTES,TABLE HAS 7BYTE CMDS         
*                                                                               
GETDSP2C CLC   =C'STATI',ENTFILE     STATIONS FILE?                             
         BNE   GETDSP2D              NO, SKIP SETTING KEY LENGTH                
         MVI   INDISP,17             SET 17 BYTE KEY                            
         MVI   OUTDISP,17            SET 17 BYTE KEY                            
*                                                                               
GETDSP2D CLC   =C'DMTRACE',ENTCMD    SPECIAL TYPE TRACE-DATA DISPLAY?           
         BE    GETDSP6               YES, HANDLE SPECIALLY                      
         CLC   =C'GETRE',ENTCMD      GETREC COMMAND?                            
         BE    GETDSP3               YES                                        
         CLC   =C'PUTRE',ENTCMD      PUTREC COMMAND?                            
         BE    GETDSP3               YES                                        
         CLC   =C'DMADD',ENTCMD      ADDREC COMMAND?                            
         BE    GETDSP4               YES                                        
         CLC   =C'TEMPE',ENTFILE     TEMPEST COMMAND?                           
         BE    GETDSP5               YES                                        
         CLC   =C'TEMPS',ENTFILE     TEMPSTR COMMAND?                           
         BE    GETDSP5               YES                                        
         B     GETDSPX                                                          
*                                                                               
GETDSP3  MVI   INDISP,4              DISK ADDR=4 BYTES                          
         B     GETDSPX                                                          
*                                                                               
GETDSP4  MVI   OUTDISP,0             NO OUTPUT FOR ADDREC                       
         B     GETDSPX                                                          
*                                                                               
GETDSP5  MVI   INDISP,1              PAGE-NUMBER=1 BYTE                         
         MVI   ENTFILE+7,C' '        DELETE GARBAGE IN INPUT STRING             
         B     GETDSPX                                                          
*                                                                               
GETDSP6  MVC   INDISP,ENTDMC1        LENGTH OF TRACE DATA                       
         MVI   OUTDISP,0             NO OUTPUT BYTES TO PRINT                   
         ZIC   RF,LINES              EXTENDED OR NORMAL # DISPLAY LINES         
         AR    RF,RF                 DBLE DISPLY LINES SINCE OUTDISP=0          
         STC   RF,DATALN             SAVE SPECIAL LINE LENGTH ALLOWANCE         
         B     GETDSPX                                                          
*                                                                               
GETDSPX  L     RE,RTNADR             LOAD RETURN ADDRESS                        
         BR    RE                    RETURN TO CALLER                           
         EJECT                                                                  
*                                                                               
* ********************************************************************          
* RDTRTBL: GETS INTO 31-BIT MODE TO READ THE TRACE TABLE.                       
*          R2=NUMBER OF BYTES TO MOVE INTO WORK.                                
*          R6=A(TRACE TABLE)                                                    
*          WHEN SUBROUTINE FINISHES,IT RETURNS TO CALLER (RE)                   
* ********************************************************************          
         SPACE 1                                                                
RDTRTBL  NTR1                        SAVE REGISTERS                             
         LA    RE,GETIN              GET INTO 31-BIT MODE                       
         O     RE,=X'80000000'       TURN ON HIGH ORDER BIT                     
         BSM   0,RE                  SET THE MODE                               
GETIN    DS    0H                                                               
*                                                                               
         EX    R2,*+8                MOVE IN R2 # BYTES INTO WORK               
         B     *+10                  BRANCH AROUND THE EX MVC                   
         MVC   WORK(0),0(R6)         MOVE IN ENTRY/HDR BYTES                    
*                                                                               
         LA    RE,GETOUT             GET OUT OF 31-BIT MODE                     
         BSM   0,RE                  TURN OFF 31-BIT MODE                       
GETOUT   DS    0H                                                               
         B     XIT                   RETURN TO CALLER                           
         EJECT                                                                  
* ********************************************************************          
* USRVAL: VALIDATE USER ID REQUESTED IN CTFILE.                                 
*         IF FOUND, SAVE USER ID NUMBER IN UTLID                                
* ********************************************************************          
         SPACE 1                                                                
USRVAL   NTR1                                                                   
         XC    KEY,KEY               CLEAR CTFILE KEY                           
         MVI   KEY,C'I'              KEY:C'I',14, 14X'0',USRID                  
         MVC   KEY+15(L'USRIDNAM),USRIDNAM                                      
         MVC   KEYSAVE,KEY           SAVE KEY                                   
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',KEY,IO                       
         CLC   KEYSAVE(24),IO                                                   
         BE    USRVL10               MATCH, EXTRACT USER ID CODE                
         XC    USRIDNAM,USRIDNAM     CLEAR ID TO INDICATE NO MATCH              
         B     USRVLX                EXIT                                       
*                                                                               
USRVL10  LA    R6,IO                 PT TO RECORD                               
         MVI   ELCODE,X'02'          GET SYSTEM NUMBER ELEMENT                  
         BAS   RE,GETEL              GET X'02' ELEMENT                          
         BE    *+6                   GOT THE ELEMENT                            
         DC    H'0'                  ELEMENT MISSING--DIE!                      
         MVC   UTLID,2(R6)           GET USERID NUMBER                          
*                                                                               
USRVLX   B     XIT                                                              
*                                                                               
* ********************************************************************          
* IDSRCH: SEARCH FOR TEST ID REQUESTED IN THE TEST TABLE LISTING                
*         IF FOUND, SET UTLD BITS AND SAVE THE ADDRESS.                         
* ********************************************************************          
         SPACE 1                                                                
IDSRCH   NTR1                                                                   
         L     R5,VTSTTAB            LOOK FOR TEST ID IN TABLE                  
         USING TSTTABD,R5                                                       
         LH    R6,0(R5)                                                         
         L     R7,2(R5)                                                         
         LA    R5,6(R5,R6)                                                      
*                                                                               
IDSRCH2  OC    0(6,R5),0(R5)         IS THIS AN AVAIL ENTRY                     
         BZ    IDSRCH3               YES. DON'T TEST ID=TSTACCS                 
         CLC   ID,TSTACCS            NO. DOES REQESTED TEST ID MATCH?           
         BE    IDSRCH4                                                          
*                                                                               
IDSRCH3  BXLE  R5,R6,IDSRCH2         KEEP LOOKING                               
         XC    ID,ID                 CLEAR ID TO INDICATE NOT FOUND             
         B     IDSRCHX                                                          
*                                                                               
IDSRCH4  L     RE,SRPARM3            ID FOUND- SAVE ADDR, SET BIT               
         USING UTLD,RE                                                          
         STCM  R5,15,TACCS           SAVE ADDRESS OF TEST TABLE                 
         OI    TTEST,X'10'           SET BIT TO INDICATE WE HAVE ADDR           
*                                                                               
IDSRCHX  B     XIT                                                              
         DROP  R5                                                               
         DROP  RE                                                               
         EJECT                                                                  
*                                                                               
* ********************************************************************          
* SETBITS: GETS INTO 31-BIT MODE TO WRITE THE TRACE TABLE HEADER                
* DMCB(4)=                                                                      
*   =C'SUSP'--  VARIABLE 'ONOFF' IS EITHER X'00'=ON OR X'40'=SUSPEND            
*   =C'EXTN'--  VARIABLE  'EXTN' IS EITHER X'00'=NORMAL X'80'=EXTENDED          
* ********************************************************************          
         SPACE 1                                                                
SETBITS  NTR1                        SAVE REGISTERS                             
         L     R6,ATSTRC             GET ADDRESS OF TRACE TABLE                 
         USING HDRD,R6               USE DSECT FOR THE HEADER                   
*                                                                               
         LA    RE,SET10              GET INTO 31-BIT MODE                       
         O     RE,=X'80000000'       TURN ON HIGH ORDER BIT                     
         BSM   0,RE                  SET THE MODE                               
SET10    DS    0H                                                               
*                                                                               
         CLC   DMCB(4),=C'SUSP'      WAS SUSPEND BIT SETTING REQSTD?            
         BE    SUSPND                YES                                        
         CLC   DMCB(4),=C'EXTN'      WAS EXTEND BIT SETTING REQSTD?             
         BE    EXTEND                YES                                        
         BNE   SET20                 INVALID SETTING-JUST GET OUT               
*                                                                               
SUSPND   CLI   ONOFF,HDRSUSQ         WAS A SUSPEND (ON ) REQUESTED?             
         BNE   *+12                  NO, A RELEASE (OFF) WAS REQUESTED.         
         OI    HDRSUSP,HDRSUSQ       SET BIT TO SUSPEND THE TRACE               
         B     SET20                 BYPASS SETTING BIT OFF.                    
         NI    HDRSUSP,X'FF'-HDRSUSQ ZERO THE BIT TO ALLOW THE TRACE            
         B     SET20                 GET OUT OF 31-BIT MODE                     
*                                                                               
EXTEND   CLI   EXTN,HDREXTQ          WAS EXTENDED MODE REQUESTED?               
         BNE   *+12                  NO, NORMAL MODE (NX) WAS REQUESTED         
         OI    HDREXTN,HDREXTQ       SET BIT TO SUSPEND THE TRACE               
         B     SET20                 BYPASS SETTING BIT OFF.                    
         NI    HDREXTN,X'FF'-HDREXTQ  ZERO THE BIT TO SET TO NORMAL             
         B     SET20                 GET OUT OF 31-BIT MODE                     
*                                                                               
SET20    LA    RE,SET30              GET OUT OF 31-BIT MODE                     
         BSM   0,RE                  TURN OFF 31-BIT MODE                       
SET30    DS    0H                                                               
         B     XIT                   RETURN TO CALLER                           
         EJECT                                                                  
* ********************************************************************          
* CLRSCR: CLEAR THE LINES ON THE SCREEN.                                        
* ********************************************************************          
         SPACE 1                                                                
CLRSCR   LA    RF,SRVLN1H            PT TO 1ST LINE ON SCREEN                   
         LA    R0,SRVINFOH           PT TO LAST PRINTABLE LINE                  
CLR10    ZIC   R1,0(RF)              LENGTH OF FIELD                            
         SH    R1,=H'9'              -8 FOR FLD HDR, -1 FOR EX                  
         EXMVC R1,8(RF),SPACES       CLEAR THE LINE                             
         OI    6(RF),X'80'           TRANSMIT FIELD                             
         ZIC   R1,0(RF)              GET FULL LENGTH OF FIELD                   
         AR    RF,R1                 BUMP TO NEXT FIELD                         
         CR    RF,R0                 HAVE WE CLEARED ALL FIELDS YET?            
         BL    CLR10                 NOT YET, LOOP BACK                         
         BR    RE                    YEP, RETURN TO CALLER                      
         EJECT                                                                  
*                                                                               
* ********************************************************************          
* HEADUP - TITLE FOR PAGE                                                       
* ********************************************************************          
HEADUP   MVC   PRTLINE(7),=C'RUN ON '  PAGE HEADER                              
         GOTO1 VDATCON,DMCB,(5,WORK),(5,DATE)                                   
         MVC   PRTLINE+7(8),DATE     DATE RUN                                   
         MVC   PRTLINE+16(3),=C'AT '                                            
         MVC   PRTLINE+19(8),TIME    TIME RUN                                   
         BAS   RE,PRINT                                                         
         XC    PRTLINE,PRTLINE       CLEAR PRT LINE (BLANK LINE)                
         BAS   RE,PRINT              PRINT A BLANK LINE                         
         MVC   PRTLINE(40),DESCR     MOVE IN TITLE DESCRIPTION                  
         BAS   RE,PRINT              PRINT A BLANK LINE                         
         XC    PRTLINE,PRTLINE       CLEAR PRT LINE (BLANK LINE)                
         BAS   RE,PRINT              PRINT A BLANK LINE                         
         XC    PRTLINE,PRTLINE       CLEAR PRT LINE (BLANK LINE)                
         BR    R3                    RETURN ADDRESS                             
         EJECT                                                                  
* ********************************************************************          
* INFOLN: SET INFORMATION INDICATORS AT BOTTOM OF SCREEN                        
* ********************************************************************          
         SPACE 1                                                                
         DROP  R6                                                               
INFOLN   LA    R1,HEADER              PT TO TRACE BUFFER INFO                   
         USING HDRD,R1                USE IT'S DSECT                            
         MVC   SRVINFO+24(3),=C'ON '  DEFAULT=TRACE ON                          
         TM    HDRSUSP,HDRSUSQ        IS TRACE SUSPENDED?                       
         BNO   *+10                   BRANCH AROUND MOVE                        
         MVC   SRVINFO+24(3),=C'OFF'  BIT SAYS TRACE OFF                        
         MVC   SRVINFO+18(6),=C'TRACE='                                         
         MVC   SRVINFO+29(2),=C'I='   SET TEST ID                               
         MVC   SRVINFO+31(4),SUBID    SET TEST ID                               
         MVC   SRVINFO+37(6),=C'COUNT='                                         
         EDIT  (4,HDRENTY),(8,WORK+20),0,ALIGN=LEFT,ZERO=NOBLANK                
         MVC   SRVINFO+43(3),WORK+20                                            
         EDIT  (2,TRCNUM),(4,WORK+20),0,ALIGN=LEFT,ZERO=NOBLANK                 
         MVC   SRVINFO+48(6),=C'START='                                         
         MVC   SRVINFO+54(4),WORK+20                                            
         MVC   SRVINFO+58(6),=C'INTVL='                                         
         EDIT  (4,HDRINVL),(8,WORK+20),0,ALIGN=LEFT,ZERO=NOBLANK                
         MVC   SRVINFO+64(8),WORK+20                                            
*                                                                               
****     LA    R1,30                                                            
****     STCM  R1,15,HDRLGIO                                                    
*                                                                               
         MVC   SRVINFO+69(3),=C'IO='                                            
         EDIT  (4,HDRLGIO),(8,WORK+20),0,ALIGN=LEFT,ZERO=NOBLANK                
         MVC   SRVINFO+72(6),WORK+20                                            
*                                                                               
         OI    SRVINFOH+6,X'80'       TRANSMIT FIELD                            
         OI    SRVINFOH+6,X'08'       HIGH INTENSITY                            
*                                                                               
         BR    RE                     RETURN TO CALLER                          
         DROP  R1                                                               
         EJECT                                                                  
*                                                                               
* ********************************************************************          
* DISPSCR: DISPLAY ENTRY ON SCREEN.  SCRADR--PTS TO AVAIL PRINT LINE.           
* ********************************************************************          
         SPACE 1                                                                
DISPSCR  NTR1                        SAVE REGISTERS                             
         LA    R6,ENTRY              POINT TO ENTRY                             
         USING ENTD,R6               USE IT'S DSECT                             
         L     R7,SCRADR             GET ADDRESS OF NEXT LINE ON SCREEN         
         USING SCRND,R7              USE SCREEN-LINE DSECT                      
*                                                                               
         MVI   FLAG,C'I'             SET IN/OUT FLAG TO INPUT                   
         XC    READ,READ             BYTES ALREADY READ=0                       
         MVC   IODSP,INDISP          USE INPUT LENGTH                           
         BAS   RE,DISP               DISPLAY INPUT PARAMETERS                   
*                                                                               
         MVI   FLAG,C'O'             SET IN/OUT FLAG TO OUTPUT                  
         XC    READ,READ             BYTES ALREADY READ=0                       
         CLC   =C'DMTRACE',ENTCMD    SPECIAL DMTRACE CASE?                      
         BE    DISPX                 YES, NO MORE OUTPUT TO DISPLAY             
         MVC   IODSP,OUTDISP         USE OUTPUT LENGTH                          
         BAS   RE,DISP               DISPLAY OUTPUT PARAMETERS                  
*                                                                               
         B     DISPX                 DONE WITH DISPLAY, EXIT                    
*                                                                               
DISP     ST    RE,RTNADR             SAVE RETURN ADDRESS                        
         LA    RF,SRVINFO            SEE IF SCRN IS FULL                        
         C     RF,SCRADR             ARE WE STILL WITHIN SCREEN?                
         BNH   DISP50                NO, EXIT                                   
         SR    R2,R2                 CLEAR COUNTER                              
*                                                                               
         CLI   FLAG,C'I'             THIS IS FOR INPUT ONLY                     
         BNE   DISP07                NO, GO DO STUFF FOR OUTPUT                 
         MVC   SCRNAME,ENTCMD        LOAD IN COMMAND                            
         GOTO1 VHEXOUT,DMCB,ENTDMC1,SCRDMCB,1,=C'MIX'                           
         LH    RE,START              PRINT ENTRY #                              
         OC    INTVL,INTVL           INTERVAL DEFINED?                          
         BZ    *+12                                                             
         L     RF,INTVL                                                         
         MR    RE,RE                                                            
         LR    RE,RF                                                            
         AH    RE,TRCNUM             ADD ON STARTING OFFFSET                    
         ST    RE,TEMP                                                          
         EDIT  (2,TEMP+2),(4,SCRENTY),0,ALIGN=LEFT,ZERO=NOBLANK                 
         B     DISP10                PROCESS LINE                               
*                                                                               
DISP07   MVC   SCRNAME,ENTFILE       LOAD IN FILENAME                           
         GOTO1 VHEXOUT,DMCB,ENTDMC3,SCRDMCB,1,=C'MIX'                           
         CLI   IODSP,0               IF THIS IS AN ADDREC, DONE                 
         BE    DISP50                                                           
*                                                                               
         SPACE 2                                                                
DISP10   ZIC   R1,IODSP              GET # BYTES OF INPUT DATA TO DISP          
         CLI   IODSP,0               ANY MORE BYTES TO DISPLAY?                 
         BE    DISP50                NO MORE BYTES TO OUTPUT                    
         CLI   IODSP,OUTQ            SEE IF > MAX,<MAX, OR 0                    
         BH    *+12                  MORE THAN 1 LINE-MUST ADJUST               
         MVI   IODSP,0               1 LINE WILL DO-SET TO 0 FOR LP END         
         B     DISP20                BRANCH AROUND ADJUSTMENT                   
*                                                                               
         SH    R1,=Y(OUTQ)           REDUCE INDISP FOR NEXT PASS                
         STC   R1,IODSP              SAVE AWAY                                  
         LA    R1,OUTQ               DISPLAY FULL LINE NOW                      
*                                                                               
DISP20   ST    R1,TEMP               TEMP=# BYTES TO BE HEX-OUT-TED             
         BCTR  R1,0                  DECR FOR EX                                
         CLC   =C'DMADD',ENTFILE     IS THIS AN ADDREC?                         
         BE    DISP30                YES, GET BYTES FROM DMCB4                  
         CLI   FLAG,C'I'             IF INPUT, GET BYTES FROM ENTINP            
         BNE   DISP25                NO, SO GET FROM ENTOUT                     
         LA    R5,ENTINP             USE DMCB FOR INPUT                         
         AH    R5,READ               ADJ BY BYTS ALREADY READ/DISPLY            
         EXMVC R1,SCREBCD,0(R5)      DISPLAY EBCIDIC CHARS- IN                  
         B     DISP40                SKIP SPECIAL-CASE-'ADDREC' PART            
*                                                                               
DISP25   LA    R5,ENTOUT             USE DMCB FOR OUTPUT                        
         AH    R5,READ               ADJ BY BYTS ALREADY READ/DISPLYD           
         EXMVC R1,SCREBCD,0(R5)      DISPLAY EBCIDIC CHARS- OUT                 
         B     DISP40                                                           
*                                                                               
DISP30   LA    R5,ENTOUT             USE DMCB FOR OUTPUT                        
         AH    R5,READ               ADJ BY BYTS ALREADY READ/DISPLY            
         EXMVC R1,SCREBCD,ENTOUT     GET ADDREC 'INPUT' FROM DMCBW4             
*                                                                               
DISP40   L     R1,TEMP               STORE NUMBER OF BYTES TO MOVE              
         ST    R1,DMCB+8             SAVE AWAY                                  
         AH    R1,READ               BYTES DISPLAYED HERE+PREV DISPLAY          
         STH   R1,READ               READ USD NXT TIME TO GET NEW BYTES         
         GOTO1 VHEXOUT,DMCB,SCREBCD,SCRDATA,,=C'MIX'                            
         TR    SCREBCD,TRTABLE       TRANSLATE VIEWABLE CHARS                   
         CLI   IODSP,0               HAVE WE DISPLAYED ALL IN/OUT BYTS?         
         BE    DISP50                YES, RETURN TO CALLER                      
         LA    R2,1(R2)              BUMP LINE COUNTER                          
         STC   R2,CNT                SAVE COUNTER                               
         CLC   =C'DMTRACE',ENTCMD    IF DMTRACE,MORE DISPLY LINES ALLWD         
         BNE   DISP40A               NO, USE ONLY 2-REG OR 3-EXTN               
         CLC   CNT,DATALN            HAVE WE PRINTED ALL 4/6 LINES?             
         BE    DISP50                YES, EXIT                                  
         B     DISP40B               CONTINUE PROCESSING LINES                  
*                                                                               
DISP40A  CLC   CNT,LINES             HAVE WE PRINTED ALL LINES?                 
         BE    DISP50                WE'VE PRINTED ALL WE ARE SUP TO            
*                                                                               
DISP40B  ZIC   R1,0(R7)              LENGTH OF FIELD                            
         AR    R7,R1                 BUMP TO NEXT LINE                          
         ST    R7,SCRADR             SAVE SCREEN ADDRESS                        
         LA    RF,SRVINFO            IS THE SCREEN FULL?                        
         C     RF,SCRADR             ARE WE STILL WITHIN SCREEN?                
         BH    DISP10                YES, PRINT REST OF DATA                    
*                                                                               
DISP50   ZIC   R1,0(R7)              LENGTH OF FIELD                            
         AR    R7,R1                 BUMP TO NEXT LINE--LEAVE BLNK LINE         
         ST    R7,SCRADR             SAVE SCREEN ADDRESS                        
         L     RE,RTNADR             RESTORE RETURN ADDRESS                     
         BR    RE                    RTN TO CALLR-DO OUPUT OR EXIT              
*                                                                               
DISPX    AR    R7,R1                 SKIP A LINE                                
         ST    R7,SCRADR                                                        
         B     XIT                   EXIT                                       
*                                                                               
         EJECT                                                                  
* ********************************************************************          
* PRTENTY: PRINT ENTRY ON REPORT LINE                                           
* ********************************************************************          
         SPACE 1                                                                
PRTENTY  NTR1                        SAVE REGISTERS                             
         LA    R6,ENTRY              POINT TO ENTRY                             
         USING ENTD,R6               USE IT'S DSECT                             
         LA    R7,PRTLINE            PR TO PRINT LINE                           
         USING PRTLND,R7             USE PRINT LINE DSECT                       
         XC    PRTLINE,PRTLINE       CLEAR PRT LINE                             
*                                                                               
         MVI   FLAG,C'I'             SET IN/OUT FLAG TO INPUT                   
         XC    READ,READ             BYTES ALREADY READ=0                       
         MVC   IODSP,INDISP          USE INPUT LENGTH                           
         BAS   RE,PRT                PRINT ENTRY                                
*                                                                               
         MVI   FLAG,C'O'             SET IN/OUT FLAG TO OUTPUT                  
         XC    READ,READ             BYTES ALREADY READ=0                       
         CLC   =C'DMTRACE',ENTCMD    SPECIAL DMTRACE CASE?                      
         BE    PRTX                  YES, NO MORE OUTPUT TO PRINT               
         MVC   IODSP,OUTDISP         USE OUTPUT LENGTH                          
         BAS   RE,PRT                PRINT OUTPUT PARAMETERS                    
*                                                                               
         B     PRTX                  DONE WITH PRINT, EXIT                      
*                                                                               
PRT      ST    RE,RTNADR             SAVE RETURN ADDRESS                        
         SR    R2,R2                 CLEAR COUNTER                              
*                                                                               
         CLI   FLAG,C'I'             THIS IS FOR INPUT ONLY                     
         BNE   PRT07                 NO, GO DO STUFF FOR OUTPUT                 
         MVC   PNAME,ENTCMD          LOAD IN COMMAND                            
         GOTO1 VHEXOUT,DMCB,ENTDMC1,PDMCB,1,=C'MIX'                             
         LH    RE,START              PRINT ENTRY #                              
         OC    INTVL,INTVL           INTERVAL DEFINED?                          
         BZ    *+12                                                             
         L     RF,INTVL                                                         
         MR    RE,RE                                                            
         LR    RE,RF                                                            
         AH    RE,TRCNUM             ADD ON STARTING OFFFSET                    
         ST    RE,TEMP                                                          
         EDIT  (2,TEMP+2),(4,PENTY),0,ALIGN=LEFT,ZERO=NOBLANK                   
         B     PRT10                 PROCESS LINE                               
*                                                                               
PRT07    MVC   PNAME,ENTFILE         LOAD IN FILENAME                           
         GOTO1 VHEXOUT,DMCB,ENTDMC3,PDMCB,1,=C'MIX'                             
         CLI   IODSP,0               IF THIS IS AN ADDREC, DONE                 
         BNE   PRT10                 NO, THERE'S MORE DATA TO OUTPUT            
         BAS   RE,PRINT              YES. NO MORE DATA. PRT THE LINE            
         B     PRT50                 EXIT                                       
*                                                                               
PRT10    ZIC   R1,IODSP              GET # BYTES OF INPUT DATA TO PRT           
         CLI   IODSP,0               ANY MORE BYTES TO PRINT?                   
         BE    PRT50                 NO MORE BYTES TO OUTPUT                    
         CLI   IODSP,OUTQ            SEE IF > MAX,<MAX, OR 0                    
         BH    *+12                  MORE THAN 1 LINE-MUST ADJUST               
         MVI   IODSP,0               1 LINE WILL DO-SET TO 0 FOR LP END         
         B     PRT20                 BRANCH AROUND ADJUSTMENT                   
*                                                                               
         SH    R1,=Y(OUTQ)           REDUCE INDISP FOR NEXT PASS                
         STC   R1,IODSP              SAVE AWAY                                  
         LA    R1,OUTQ               PRINT   FULL LINE NOW                      
*                                                                               
PRT20    XC    PDATA(65),PDATA       CLEAR PRT LINE                             
         ST    R1,TEMP               TEMP=# BYTES TO BE HEX-OUT-TED             
         BCTR  R1,0                  DECR FOR EX                                
         CLC   =C'DMADD',ENTFILE     IS THIS AN ADDREC?                         
         BE    PRT30                 YES, GET BYTES FROM DMCB4                  
         CLI   FLAG,C'I'             IF INPUT, GET BYTES FROM ENTINP            
         BNE   PRT25                 NO, SO GET FROM ENTOUT                     
         LA    R5,ENTINP             USE DMCB FOR INPUT                         
         AH    R5,READ               ADJ BY BYTS ALREADY READ/PRT               
         EXMVC R1,PEBCD,0(R5)        PRINT   EBCIDIC CHARS- IN                  
         B     PRT40                 SKIP SPECIAL-CASE-'ADDREC' PART            
*                                                                               
PRT25    LA    R5,ENTOUT             USE DMCB FOR OUTPUT                        
         AH    R5,READ               ADJ BY BYTS ALREADY READ/PRINT             
         EXMVC R1,PEBCD,0(R5)        PRINT   EBCIDIC CHARS- OUT                 
         B     PRT40                                                            
*                                                                               
PRT30    LA    R5,ENTOUT             USE DMCB FOR OUTPUT                        
         AH    R5,READ               ADJ BY BYTS ALREADY READ/PRT               
         EXMVC R1,PEBCD,ENTOUT       GET ADDREC 'INPUT' FROM DMCBW4             
*                                                                               
PRT40    L     R1,TEMP               STORE NUMBER OF BYTES TO MOVE              
         ST    R1,DMCB+8             SAVE AWAY                                  
         AH    R1,READ               BYTES PRINTED   HERE+PREV PRINT            
         STH   R1,READ               READ USD NXT TIME TO GET NEW BYTES         
         GOTO1 VHEXOUT,DMCB,PEBCD,PDATA,,=C'MIX'                                
         TR    PEBCD,TRTABLE         TRANSLATE VIEWABLE CHARS                   
         BAS   RE,PRINT              PRINT OUT DATA                             
*                                                                               
         CLI   IODSP,0               HAVE WE PRINTED   ALL IN/OUT BYTS?         
         BE    PRT50                 YES, RETURN TO CALLER                      
         LA    R2,1(R2)              BUMP LINE COUNTER                          
         STC   R2,CNT                SAVE COUNTER                               
         CLC   =C'DMTRACE',ENTCMD    IF DMTRACE,MORE PRT    LINES ALLWD         
         BNE   PRT40A                NO, USE ONLY 2-REG OR 3-EXTN               
         CLC   CNT,DATALN            HAVE WE PRINTED ALL 4/6 LINES?             
         BE    PRT50                 YES, EXIT                                  
         B     PRT10                 CONTINUE PROCESSING LINES                  
*                                                                               
PRT40A   CLC   CNT,LINES             HAVE WE PRINTED ALL LINES?                 
         XC    PRTLINE,PRTLINE       CLEAR LINE FOR NEW DATA                    
         BNH   PRT10                 NO, KEEP PRINTING                          
*                                                                               
PRT50    XC    PRTLINE,PRTLINE       CLEAR PRINT LINE                           
         L     RE,RTNADR             RESTORE RETURN ADDRESS                     
         BR    RE                    RTN TO CALLR-DO OUPUT OR EXIT              
*                                                                               
PRTX     LTR   R1,R1                 SET CC=EQUAL- IF ERROR CC=NEQU             
         B     XIT                   EXIT                                       
*                                                                               
         EJECT                                                                  
*                                                                               
* *******************************************************************           
* PRINT - DEPENDING ON VALUE OF PQCTL, PRINT WILL OPEN QUEUE, PUT OUT           
*         A LINE ON QUEUE, OR CLOSE QUEUE:                                      
* PQCTL =      X'00' TO INITIALIZE A REPORT                                     
*              X'01' TO PRINT A LINE (R7=A(DATA))                               
*              X'FF' TO TERMINATE A REPORT.                                     
* ON EXIT CC=NEQ ON ERROR WITH ERRNUM SET TO ERROR.                             
* *******************************************************************           
*                                                                               
PRINT    NTR1                                                                   
         MVI   ERRNUM,0                                                         
         XC    PQLINE,PQLINE       CLEAR PRINT LINE                             
         CLI   PQCTL,0                                                          
         BNE   PRINT2                                                           
*                                                                               
PRINT1   LA    R5,PQLINE           SET REPORT ATTRIBUTES                        
         USING PQPLD,R5                                                         
         MVC   QLSRCID,UTLID                                                    
         MVI   QLEXTRA,X'FF'                                                    
         MVC   QLSUBID,SUBID                                                    
         MVI   QLCLASS,C'S'                                                     
         MVI   QLLINET,X'C0'                                                    
         MVI   QLLINEW,132                                                      
         MVC   QLRETNL,=H'8'                                                    
         MVC   QLRETND,=H'1'                                                    
         MVC   QLDESC,SPACES                                                    
         MVC   QLDESC,=C'***TRACE***'                                           
*                                                                               
         GOTO1 VDATAMGR,DMCB,=C'DMPRINT',=C'PRTQUE',0,PQLINE,TWA0               
         CLI   DMCB+8,0                                                         
         BNE   PCHECK                                                           
         MVC   REPNO,QLREPRNO      EXTRACT REPORT NUMBER                        
         MVC   SUBID,QLSUBID       EXTRACT REPORT SUBID                         
         MVC   PRTQID,=C'PRTQ  '                                                
         MVC   PRTQID+4(1),QLREPCHR   PRTQ FILEID NEW LOCATION                  
         CLI   PRTQID+4,C'A'                                                    
         BNL   *+10                                                             
         MVC   PRTQID+4(1),QLREPRCI-1 PRTQ FILEID OLD LOCATION                  
         CLI   PRTQID+4,C'A'                                                    
         BNL   *+8                                                              
         MVI   PRTQID+4,C' '       UNKNOWN PRTQ FILE                            
*                                                                               
         MVC   PQLINE,SPACES                                                    
         MVI   PQLINE,X'8B'                                                     
         B     PRINT4                                                           
*                                                                               
PRINT2   CLI   PQCTL,1                                                          
         BNE   PRINT6                                                           
         MVC   PQLINE,SPACES                                                    
         CLI   LNCNT,X'FF'           IF CNT=FF, DO A PG BREAK                   
         BNE   *+12                  NO                                         
         MVI   PQLINE,X'89'          PAGE BREAK                                 
         B     *+8                                                              
         MVI   PQLINE,X'09'                                                     
*                                                                               
         MVC   PQLINE+1(80),0(R7)                                               
         ZIC   RF,LNCNT              BUMP LINE COUNTER                          
         LA    RF,1(RF)                                                         
         STC   RF,LNCNT                                                         
*                                                                               
PRINT4   GOTO1 VDATAMGR,DMCB,=C'DMPRINT',=C'PRTQUE',0,PQLINE,TWA0               
         B     PCHECK                                                           
*                                                                               
PRINT6   MVI   PQLINE,X'FF'                                                     
         B     PRINT4                                                           
*                                  TEST FOR ERRORS & SET ERRNUM                 
PCHECK   CLI   DMCB+8,0                                                         
         BE    PXIT                                                             
         MVI   ERRNUM,3                                                         
         TM    DMCB+8,X'80'        E-O-F ERROR                                  
         BO    PXIT                                                             
         MVI   ERRNUM,4                                                         
         TM    DMCB+8,X'01'        FORMAT ERROR                                 
         BO    PXIT                                                             
         MVI   ERRNUM,5                                                         
         TM    DMCB+8,X'40'        DISK ERROR                                   
         BO    PXIT                                                             
         DC    H'0'                                                             
*                                                                               
PXIT     CLI   ERRNUM,0            SET CONDITION CODE                           
         XIT1                                                                   
* ********************************************************************          
* GETTIME- GET TIME OF DAY IN A NICE FORMAT. PLACE IN TIME                      
* ********************************************************************          
GETTIME  NTR1                                                                   
         TIME  DEC                 GET TIME OF DAY                              
         STCM  R0,14,DUB                                                        
         LA    R2,TIME             STORE FORMATED TIME                          
         XC    TIME,TIME                                                        
CVTTIME  DS    0H                                                               
         UNPK  WORK(9),DUB(5)                                                   
         MVC   0(6,R2),=C'  .  .'                                               
         MVC   0(2,R2),WORK        HOURS                                        
         MVC   3(2,R2),WORK+2      MINUTES                                      
         MVC   6(2,R2),WORK+4      SECONDS                                      
         B     XIT                                                              
         SPACE 3                                                                
* ********************************************************************          
* ERROR MESSAGES                                                                
* ********************************************************************          
ERR1     MVC   MSG(30),=C'NOT CONNECTED TO A TEST BUFFER'                       
         B     ERRX                                                             
*                                                                               
ERR2     MVC   MSG(32),=C'INVALID INPUT-MUST BE:X/ /ON/OFF'                     
         B     ERRX                                                             
*                                                                               
ERR3     MVC   MSG(29),=C'INPUT MUST BE DECIMAL NUMERIC'                        
         B     ERRX                                                             
*                                                                               
ERR4     MVC   MSG(20),=C'MAX 4 DIGITS ALLOWED'                                 
         B     ERRX                                                             
*                                                                               
ERR5     MVC   MSG(19),=C'ENTRY NOT IN BUFFER'                                  
         B     ERRX                                                             
*                                                                               
ERR6     MVC   MSG(16),=C'NO ENTRIES EXIST'                                     
         B     ERRX                                                             
*                                                                               
ERR7     MVC   MSG(32),=C'$TRACE ONLY VALID ON TEST SYSTEMS'                    
         B     ERRX                                                             
*                                                                               
ERRCNCT  MVC   MSG(32),=C'MUST BE CONNECTED. ENTER USER ID'                     
         B     ERRX                                                             
*                                                                               
ERRINV   MVC   MSG(13),=C'INVALID INPUT'                                        
         B     ERRX                                                             
*                                                                               
ERRDEF   MVC   MSG(15),=C'INVALID TEST ID'                                      
         B     ERRX                                                             
*                                                                               
ERRMIS   MVC   MSG(22),=C'PLEASE ENTER A TEST ID'                               
         B     ERRX                                                             
*                                                                               
ERRPFKY  MVC   MSG(34),=C'HIT ENTER KEY TO SEND TO PRT QUEUE'                   
         B     ERRX                                                             
*                                                                               
ERRDSK   MVC   MSG(28),=C'PRINT QUEUE OPEN: DISK ERROR'                         
         B     ERRX                                                             
*                                                                               
ERRFMT   MVC   MSG(30),=C'PRINT QUEUE OPEN: FORMAT ERROR'                       
         B     ERRX                                                             
*                                                                               
ERREOF   MVC   MSG(27),=C'PRINT QUEUE OPEN: EOF ERROR'                          
         B     ERRX                                                             
*                                                                               
ERRPRT1  MVC   MSG(25),=C'ERROR OPENING PRINT QUEUE'                            
         B     ERRX                                                             
*                                                                               
ERRPRT2  MVC   MSG(25),=C'ERROR WRITING PRINT QUEUE'                            
         B     ERRX                                                             
*                                                                               
ERRPRT3  MVC   MSG(25),=C'ERROR CLOSING PRINT QUEUE'                            
         B     ERRX                                                             
*                                                                               
ERRUSID  MVC   MSG(15),=C'INVALID USER ID'                                      
         B     ERRX                                                             
*                                                                               
ERRTWA   MVC   MSG(25),=C'ERROR READING/WRITING TWA'                            
         B     ERRX                                                             
*                                                                               
CHGD     MVC   SRVMSG,SPACES         CLEAR MESSAGE LINE                         
         MVC   SRVMSG(34),=C'TRACE SUSPEND BIT HAS BEEN CHANGED'                
         B     ERRX1                                                            
*                                                                               
ERRX     MVC   SRVMSG(12),=C'***ERROR****'                                      
         MVC   SRVMSG+12(48),MSG                                                
ERRX1    OI    SRVMSGH+6,X'80'                                                  
         OI    6(R4),X'40'           POSN CURSOR ON INVALID FIELD               
*                                                                               
XIT      XIT1                                                                   
* ********************************************************************          
* VARIABLES AND CONSTANTS                                                       
* ********************************************************************          
         SPACE 2                                                                
         GETEL R6,ELDISP,ELCODE      GET SYSTEM ELEMENT                         
         SPACE 2                                                                
SPACES   DC    CL134' '              SPACES TO CLR SCREEN/PRT LINE              
ELDISP   DC    H'28'                                                            
*                                                                               
* TABLE OF COMMANDS                                                             
CMDTABLE DS    0F                                                               
DMADD    DC    CL7'DMADD  ',X'0105'                                             
DMREAD   DC    CL7'DMREAD ',X'0206'                                             
DMRSEQ   DC    CL7'DMRSEQ ',X'0306'                                             
DMRDHI   DC    CL7'DMRDHI ',X'0406'                                             
DMTRACE  DC    CL7'DMTRACE',X'0000'                                             
DMDEL    DC    CL7'DMDEL  ',X'0505'                                             
DMRDIR   DC    CL7'DMRDIR ',X'0606'                                             
DMWRT    DC    CL7'DMWRT  ',X'0805'                                             
DMPRINT  DC    CL7'DMPRINT',X'0907'                                             
DMFAST   DC    CL7'DMFAST ',X'0A0A'                                             
GETREC   DC    CL7'GETREC ',X'0B06'                                             
PUTREC   DC    CL7'PUTREC ',X'0C06'                                             
ADDREC   DC    CL7'ADDREC ',X'0D06'                                             
DMRSRV   DC    CL7'DMRSRV ',X'0E06'                                             
DMRLSE   DC    CL7'DMRLSE ',X'0F06'                                             
ADFREC   DC    CL7'ADFREC ',X'1006'                                             
*&&US*&& DC    CL7'GETDSK ',X'0B06'               HEAP OF SHIT                  
         DC    X'FF'                                                            
*                                                                               
*                                                                               
         SPACE 2                                                                
         EJECT                                                                  
* TRANLATION TABLE TO DISPLAY ONLY DISPLAYABLE CHARS OR '.'                     
TRTABLE  DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     00-0F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     10-1F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     20-2F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     30-3F                    
         DC    X'404B4B4B4B4B4B4B4B4B4B4B4B4D4E4B'     40-4F                    
         DC    X'504B4B4B4B4B4B4B4B4B4B5B5C5D4B4B'     50-5F                    
         DC    X'60614B4B4B4B4B4B4B4B4B6B6C6D4B6F'     60-6F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B7B4B7D7E4B'     70-7F                    
         DC    X'4B8182838485868788894B4B4B4B4B4B'     80-8F                    
         DC    X'4B9192939495969798994B4B4B4B4B4B'     90-9F                    
         DC    X'4B4BA2A3A4A5A6A7A8A94B4B4B4B4B4B'     A0-AF                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     B0-BF                    
         DC    X'4BC1C2C3C4C5C6C7C8C94B4B4B4B4B4B'     C0-CF                    
         DC    X'4BD1D2D3D4D5D6D7D8D94B4B4B4B4B4B'     D0-DF                    
         DC    X'4B4BE2E3E4E5E6E7E8E94B4B4B4B4B4B'     E0-EF                    
         DC    X'F0F1F2F3F4F5F6F7F8F94B4B4B4B4B4B'     F0-FF                    
*                                                                               
IO       DC    1000X'00'                                                        
*                                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
* ******************************************************************            
*                WORKING STORAGE                                                
* ******************************************************************            
WRKD     DSECT                                                                  
DUB      DS    D                                                                
DUB1     DS    D                                                                
DMCB     DS    6F                                                               
*                                                                               
VHEXOUT  DS    A                                                                
VHEXIN   DS    A                                                                
VSCANNER DS    A                                                                
VHELLO   DS    A                                                                
VKEYDIR  DS    A                                                                
VKEYFIL  DS    A                                                                
VGLOBBER DS    A                                                                
VDATCON  DS    A                                                                
ATSTRC   DS    A                     ADDRESS OF TRACE TABLE                     
ATIA     DS    A                     A(TIA)                                     
APARMS   DS    A                     A(TIA)                                     
AGLOBALS DS    A                     ADDRESS OF GLOBAL VARIABLES                
*                                                                               
TSTBUFNM DS    CL4                   NAME OF TEST BUFFER                        
ID       DS    CL4                   TEST ID                                    
SUBID    DS    CL4                   SUBID-NAME ON PRINTED REPORT               
UTLID    DS    CL2                   USER ID- SYSTEM CONNECTED TO               
USRIDNAM DS    CL10                  USER ID- SYSTEM CONNECTED TO               
KEY      DS    CL28                  KEY FOR CTFILE                             
KEYSAVE  DS    CL28                  SAVED KEY                                  
MSG      DS    CL60                                                             
*                                                                               
SYST     DS    X                     SYSTEM CONNECTD TO (TOVSYS)                
ELCODE   DS    X                     ELEMENT CODE                               
EXTN     DS    X                     EXTENDED :X'10'=EXTN X'00'=NORMAL          
ONOFF    DS    X                     TRACE ON/OFF FLAG= X'80' OR X'00'          
PFKEY    DS    X                     FUNCTION KEY PRESSED                       
LINES    DS    X                     # OUTPUT LINES-DEFAULT=2 EXTN(X)=3         
LNCNT    DS    X                     COUNTS THE # LINES ON THE PAGE             
DATALN   DS    X                     # OUTPUT LINES-FOR DMTRACE- (4/6)          
ENTYS    DS    X                     # ENTRIES TO DISPLAY ON SCREEN             
CNT      DS    X                     # OUTPUT LINES-DEFAULT=2 EXTN(X)=3         
PRNTD    DS    X                     #LINES PRINTED SO FAR                      
IODSP    DS    X                     # BYTES OF EITHER INP OUR OUT DISP         
INDISP   DS    X                     # BYTES OF DMCBW3 TO DISPLAY               
OUTDISP  DS    X                     # BYTES OF DMCBW4 TO DISPLAY               
*                                                                               
START    DS    H                     OFFST OF ENTRY IN BUFFER REQUESTD          
READ     DS    H                     # BYTES ALREAD READ/DISPLAYED              
INPNUM   DS    H                     INPUT NUMBER FROM USER                     
TRCNUM   DS    H                     START COUNT FOR IO TRACE                   
*                                                                               
INTVL    DS    F                     INTERVAL BETWEEN ACTUAL I/O'S              
TEMP     DS    F                     TEMP STORAGE                               
RTNADR   DS    F                     RETURN ADDRESS FROM A BAS                  
SCRADR   DS    F                     ADDR OF NEXT AVAIL LINE ON SCRN            
*                                                                               
         DS    0F                    FULL WORD ALLIGNMENT                       
HEADER   DS    CL(HDRLNQ)            HEADER COPIED FROM BUFFER                  
FLAG     DS    CL1                   EITHER C'I' OR C'O' INPUT/OUTPUT           
ENTRY    DS    CL(ENTLNQ)            ONE TRACE ENTRY COPIED FROM BUFFER         
*                                                                               
PUTD     DC    CL8'PUTD'                                                        
PQCTL    DS    C                                                                
ERRNUM   DS    C                                                                
FILLS    DS    CL2                                                              
DQUID    DS    CL10                                                             
TIME     DS    CL8                   5 BYTES FOR TIME                           
DATE     DS    CL8                   DATE                                       
RECLEN   DS    H                                                                
REPNO    DS    H                                                                
PRTQID   DS    CL6                                                              
DESCR    DS    CL80                  DESCRIPTION                                
PRTLINE  DS    CL133                 PRINT LINE FOR REPORT                      
PQLINE   DS    CL133                                                            
WORK     DS    CL200                                                            
SCANTBL  DS    CL200                                                            
*                                                                               
         DS    0D                                                               
TBLOCK   DS    1920C                                                            
TWA0     DS    20000X                                                           
WRKX     DS    0H                                                               
         EJECT                                                                  
*                                                                               
GLVSPID  EQU   X'01'                 LAST REPORT ADDED TO PRT QUEUE             
*                                                                               
HDRD     DSECT                       DSECT TO COVER TRACE TABLE HEADER          
HDRLBL   DS    CL24      +00         TRACEBUF**TRACEBUF*** LABEL                
HDRLGIO  DS    XL4       +24         NUMBER OF LOGICAL I/O'S PERFORMED          
HDRINVL  DS    XL4       +28         ACTL INTERVAL BETW I/O'S READ              
HDRSYS   DS    CL4       +32         CURRENT SYSTEM INPUT NUMBER                
HDRENTY  DS    CL4       +36         CURRENT # OF ENTRIES IN BUFFER             
HDRENDA  DS    CL4       +40         ADDRESS OF BUFFER END                      
HDRSUSP  DS    CL1       +44         TRACE SUSPENDED=X'80'                      
HDRSUSQ  EQU   X'40'                 SUSPEND TRACE                              
HDREXTN  DS    CL1       +45         EXTENDED DISPLAY                           
HDREXTQ  EQU   X'80'                 EXTEND=X'80'                               
         DS    CL2       +46         SPARE                                      
HDRLNQ   EQU   *-HDRD                LENGTH OF HEADER                           
*                                                                               
HDRENTQ  EQU   HDRENTY-HDRD          DISPLACEMENT TO HDRENTRY                   
         SPACE 2                                                                
*                                                                               
ENTD     DSECT                       DSECT TO COVER TRACE TABLE ENTRIES         
ENTCMD   DS    CL8                   COMMAND                                    
ENTFILE  DS    CL8                   FILENAME                                   
ENTDMC1  DS    CL1                   DMCBW1 ON INPUT                            
ENTINP   DS    CL63                  DATA AT DMCBW3 ON INPUT                    
ENTDMC3  DS    CL1                   DMCBW3 ON OUTPUT                           
ENTOUT   DS    CL63                  DATA AT DMCBW3 ON INPUT                    
*                                                                               
ENTLNQ   EQU   *-ENTD                LENGTH OF ENTRY                            
*                                                                               
OUTQ     EQU   20                    #BYTES TO OUTPUT ON LINE                   
         EJECT                                                                  
*                                                                               
SCRND    DSECT                       DSECT FOR SCREEN LINE                      
SCRHDR   DS    CL8                   LINE HEADER                                
SCRLN    DS    0CL(L'SRVLN1)         TO CLEAR OUTPUT LINE                       
SCRENTY  DS    CL3                   ENTRY NUMBER                               
         DS    CL1                                                              
SCRNAME  DS    CL8                   COMMAND/FILENAME                           
         DS    CL1                   BLANK                                      
SCRDMCB  DS    CL2                   DMCBW1/3 1 BYTE CODE                       
         DS    CL2                   BLANK                                      
SCRDATA  DS    CL40                  INPUT/OUTPUT BYTES                         
         DS    CL1                   BLANK                                      
SCREBCD  DS    CL20                  EBCIDIC DATA                               
*                                                                               
         SPACE 2                                                                
PRTLND   DSECT                       DSECT FOR PRINT LINE                       
PLINE    DS    CL1                   1 SPACE INDENT                             
PENTY    DS    CL3                   ENTRY NUMBER                               
         DS    CL1                                                              
PNAME    DS    CL8                   COMMAND/FILENAME                           
         DS    CL1                   BLANK                                      
PDMCB    DS    CL2                   DMCBW1/3 1 BYTE CODE                       
         DS    CL2                   BLANK                                      
PDATA    DS    CL40                  INPUT/OUTPUT BYTES                         
         DS    CL1                   BLANK                                      
PEBCD    DS    CL20                  EBCIDIC DATA                               
*                                                                               
         SPACE 2                                                                
LINED    DSECT                     DISPLAY LINE FORMAT                          
         DS    CL8                                                              
         DS    CL1                                                              
LPOSN    DS    CL2                                                              
         DS    CL3                                                              
LTRM     DS    CL3                                                              
         DS    CL2                                                              
LID      DS    CL4                                                              
         DS    CL2                                                              
LLOG     DS    CL3                                                              
         DS    CL2                                                              
LLTRK    DS    CL4                                                              
         DS    CL2                                                              
LHTRK    DS    CL4                                                              
         DS    CL2                                                              
LREC     DS    CL7                                                              
         DS    CL3                                                              
LPTCH    DS    CL2                                                              
         DS    CL1                                                              
         EJECT                                                                  
* -------------------------------------------------------------------           
* ++INCLUDES:                                                                   
* -------------------------------------------------------------------           
* DDFLDHDR                                                                      
       ++INCLUDE DDFLDHDR                                                       
         EJECT                                                                  
* DDCOMFACS                                                                     
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
* FADSECTS                                                                      
       ++INCLUDE FADSECTS                                                       
         EJECT                                                                  
* SRTRCFFD                                                                      
SRTRCFFD DSECT                                                                  
         DS    CL64                                                             
       ++INCLUDE SRTRCFFD                                                       
         EJECT                                                                  
* FACHKPT                                                                       
       ++INCLUDE FACHKPT                                                        
         EJECT                                                                  
* DMPRTQL                                                                       
       ++INCLUDE DMPRTQL                                                        
         EJECT                                                                  
* SRDDEQUS                                                                      
       ++INCLUDE SRDDEQUS                                                       
         EJECT                                                                  
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'021SRTRC00   02/27/15'                                      
         END                                                                    
