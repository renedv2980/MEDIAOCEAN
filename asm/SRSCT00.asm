*          DATA SET SRSCT00    AT LEVEL 004 AS OF 05/28/08                      
*PHASE T16700A                                                                  
*INCLUDE TWANG                                                                  
         TITLE '$SCT - DISPLAY SCRIPT TRACE TABLE'                              
         PRINT NOGEN                                                            
SCT      CSECT                                                                  
         NMOD1 WRKX-WRKD,*$SCT**,R8,R9                                          
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
         USING SRSCTFFD,R3         R3=A(TWA)                                    
         L     R4,SRPARM4                                                       
         USING COMFACSD,R4         R4=A(COM FAC LIST)                           
         MVC   VHEXIN,CHEXIN                                                    
         MVC   VHEXOUT,CHEXOUT                                                  
         MVC   VSCANNER,CSCANNER                                                
         MVC   VHELLO,CHELLO                                                    
         MVC   VGLOBBER,CGLOBBER                                                
         MVC   VDATCON,CDATCON                                                  
*                                                                               
         LA    R4,SRVMSGH          POINT R4 TO A FIELD                          
         USING FLDHDRD,R4                                                       
         NI    SRVIDH+6,X'BF'                                                   
         XC    MSG,MSG                                                          
*                                                                               
         L     RE,VSSB                                                          
         USING SSBD,RE                                                          
         TM    SSBSYSFL,X'80'      TRACE VALID ON TEST/MEL SYS ONLY             
         BNO   ERR7                TRACE ONLY FOR TST/MEL SYSTEMS               
         DROP  RE                                                               
*                                                                               
PFKEYS   XC    PFKEY,PFKEY         INIT TO ENTER KEY PRESSED                    
         L     R1,28(R1)           LOAD A(TIOB)                                 
         USING TIOBD,R1            OVERLAY THE A(TIOB)                          
         CLI   TIOBAID,0           IS IT AN ENTER KEY?                          
         BE    PFKEYX              YES, PROCEED AS NORMAL                       
         ZIC   R0,TIOBAID          LOAD THE PFKEY PRESSED                       
         CH    R0,=H'12'           PF13 - PF24?                                 
         BNH   *+8                 NO, PF1 - PF12                               
         SH    R0,=H'12'           PF13 -> PF1 .. PF24 -> PF12                  
         STC   R0,PFKEY            STORE THE PFKEY PRESSED                      
PFKEYX   EQU   *                                                                
         DROP  R1                                                               
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
* VALIDATE PARAMETER IN SERVICE REQUEST FIELD:                                  
*-----------------------                                                        
VALPSRV  LA    R4,SRVIDH                                                        
         CLI   FLDILEN,0                                                        
         BE    VALPONE                                                          
         CLI   FLDILEN,4                                                        
         BNH   VALPONE                                                          
         CLI   FLDILEN,9                                                        
         BH    ERRINV                                                           
         ZIC   R1,FLDILEN            LENGTH OF INPUT                            
         SH    R1,=H'5'                                                         
         LA    RF,SRVID                                                         
         CLI   4(RF),C','                                                       
         BNE   ERRINV                                                           
         EXMVC R1,ID,5(RF)                                                      
         OC    ID,SPACES                                                        
         BAS   RE,IDSRCH                                                        
         OC    ID,ID                                                            
         BZ    ERRDEF                                                           
         B     VALPONE                                                          
*                                                                               
* VALIDATE PARAMETER 1:                                                         
*-----------------------                                                        
VALPONE  LA    R4,SRVP1H             PT TO 1ST PARAMETER FIELD                  
         CLI   FLDILEN,0             IF NO INPUT IN 1ST PARM, SKIP              
         BE    VALPTWO               GO TEST 2ND PARAMETER                      
         TM    4(R4),X'08'           NUMERIC INPUT? (TRACE NUMBER?)             
         BNO   VALP05                NO                                         
         CLI   FLDILEN,6             IF NO INPUT IN 1ST PARM, SKIP              
         BH    ERRINV                INVALID INPUT (TOO LARGE)                  
         ZIC   R1,FLDILEN            LENGTH OF INPUT                            
         BCTR  R1,0                  DECR FOR EX-PACK                           
         EX    R1,*+8                CONVERT INPUT FROM CHAR->HEX               
         B     *+10                  SKIP EX PACK                               
         PACK  DUB,SRVP1(0)          PACK                                       
         CVB   R1,DUB                CONVERT TO BINARY                          
         ST    R1,INPNUM             SAVE INPUT TRACE NUMBER                    
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
         L     RF,SRPARM3            INPUT REQRD IF NOT CNCTD                   
         USING UTLD,RF               USE UTL DSECT                              
         OC    TUSER,TUSER           MUST BE CONNECTED                          
         BZ    ERRCNCT               ERROR, MUST CONNECT                        
         BAS   RE,TESTADR            ARE WE CONNECTED TO A TEST BUFFER?         
         B     VALPTHR               YES,  SEE IF DESCRIPTION ENTERED           
         DROP  RF                                                               
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
         L     R1,INPNUM             GET OFFSET INTO BUFFER                     
         ST    R1,START              SAVE AWAY REQSTD ENTRY                     
*                                                                               
         CLI   SRVP1,C'P'            PRINT REQUEST?                             
         BE    PROCPRT               YES- GO HANDLE IT                          
*                                                                               
*PROCDSP- DISPLAY TRACE ON SCREEN                                               
*--------------------------------                                               
PROCDSP  DS    0H                                                               
*--TEST PF KEYS HIT (PF7=UP PF8=DOWN REST=ENTER)                                
         BAS   RE,GETHDR             GET HEADER-IS REQSTD ENT IN BUFFR          
         CLI   PFKEY,7               PAGE UP?                                   
         BNE   PDSP010               NO, ALL OTHER PFKEYS=ENTER KEY             
         L     R1,START              GET THE INITIAL OFFSET                     
         ZIC   R0,ENTYS              NUMBER ENTRIES ON SCREEN                   
         SR    R1,R0                 DECR BY # ENTRIES ON SCREEN                
         SR    R1,R0                 DECR AGAIN TO DO PG BACK                   
         BNM   *+6                   IF NON-NEG,NEW OFFSET IS OK                
         SR    R1,R1                 ELSE, START DISPL FROM TOP BUFFER          
         ST    R1,START              SAVE NEW OFFSET                            
*                                                                               
PDSP010  EQU   *                                                                
         BAS   RE,GETFIRST                                                      
         BAS   RE,HEADLN             SET HEAD LINE                              
         BAS   RE,INFOLN             SET INFORMATION LINE                       
         SPACE 2                                                                
*--CLEAR SCREEN AND INIT SOME VARS FOR DISPLP ACTIVITY                          
         LA    R7,SRVLN1H            PT TO 1ST OUTPUT LINE ON SCRN              
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
PROCPRT  EQU   *                                                                
         CLI   PFKEY,0               IF A PFKEY WAS HIT, COMPLAIN               
         BE    *+12                  ENTER KEY HIT, OKAY                        
         LA    R4,SRVP4H             PT PAST INPUT FIELDS                       
         B     ERRPFKY               'PLEASE HIT ENTER TO PRT TRACE'            
*                                                                               
         LA    R7,PRTLINE            PT TO THE PRINT LINE                       
         MVI   LINES,8               REPORT ALWAYS PRINTS EXTENDED              
         MVI   ENTYS,2               PRINT EXTENDED                             
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
         EJECT                                                                  
* *******************************************************************           
* READTWA -- THIS WAS TAKEN FROM SRCPY00 AND WAS SLIGHLY ALTERED                
* *******************************************************************           
*                                                                               
READTWA  ST    RE,RTNADR           SAVE RETURN ADDRESS                          
         L     R2,APARMS             RESTORE ADDRESS OF PARM LIST               
         L     R7,SRPARM3          ADDRESS OF UTL                               
         USING UTLD,R7             USE DSECT                                    
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
         DROP  R7                                                               
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
* *******************************************************************           
* DISPLP- MAJOR ACTIVITY OF PROGRAM: LOOPS THRU BUFFER AND DISPLAYS             
*         THE ENTRIES.                                                          
* *******************************************************************           
         SPACE 1                                                                
DISPLP   EQU   *                                                                
         LA    R6,HEADER                                                        
         USING SCTTABD,R6                                                       
         OC    ASCTENT,ASCTENT       HAS LAST SCTTAB ENTRY BEEN FOUND?          
         BZ    XIT                   YES, WE'RE DONE                            
         CLC   PRNTD,ENTYS           IS THE SCREEN FULL?                        
         BNL   XIT                   YES, WE'RE DONE                            
         CLI   LINES,4               SEE IF ENTRY WILL FIT ON SCREEN            
         BNE   *+12                  NOT A 10- LINE MAX ENTRY                   
         LA    RF,SRVLN15            ADDR OF LAST START POSTN FOR ENTRY         
         B     *+8                   SKIP 6-LINE ASSIGNMENT                     
         LA    RF,SRVLN11H           ADDR OF LAST STR POSTN FOR 8LN ETY         
         C     RF,SCRADR             IS NEXT AVAIL LINE BEYOND LIMIT?           
         BL    XIT                   YES, DON'T PRINT ANY MORE ENTYS            
*                                                                               
         BAS   RE,DISPSCR            DISPLAY ENTRY ON SRCEEN                    
*                                                                               
         BAS   RE,GETNEXT            GET NEXT SCTTAB ENTRY                      
*                                                                               
         OC    ASCTENT,ASCTENT       HAS LAST SCTTAB ENTRY BEEN FOUND?          
         BZ    XIT                   YES, WE'RE DONE                            
         DROP  R6                                                               
*                                                                               
         LA    R6,ENTRY                                                         
         USING SCTENTRY,R6                                                      
         XC    SRVP1,SRVP1           NO, JUST A DISPLAY.CLEAR FLD 1             
         EDIT  SCTENUM,(6,SRVP1),0,ALIGN=LEFT,ZERO=NOBLANK                      
         DROP  R6                                                               
         OI    SRVP1H+6,X'80'        TRANSMIT FIELD                             
         OI    SRVP2H+6,X'40'                                                   
*                                                                               
         ZIC   R1,PRNTD              INCREMENT NUMBER ENTRIED PRINTED           
         LA    R1,1(R1)              INCR                                       
         STC   R1,PRNTD              SAVE COUNTER                               
         B     DISPLP                LOOP TO DISPLAY FULL SCREEN                
         EJECT                                                                  
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
PRTLP    EQU   *                                                                
         LA    R6,HEADER                                                        
         USING SCTTABD,R6                                                       
         OC    ASCTENT,ASCTENT       HAS LAST SCTTAB ENTRY BEEN FOUND?          
         BZ    PRTLOOPX              YES, WE'RE DONE                            
*                                                                               
         BAS   RE,PRTENTY            SET UP PRT LINE AND PRINT ENTY             
*                                                                               
         BAS   RE,GETNEXT            GET NEXT SCTTAB ENTRY                      
*                                                                               
         OC    ASCTENT,ASCTENT       HAS LAST SCTTAB ENTRY BEEN FOUND?          
         BZ    PRTLOOPX              YES, WE'RE DONE                            
         DROP  R6                                                               
*                                                                               
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
         L     R1,START              BUMP # ENTRIES PROCESSED SO FAR            
         LA    R1,1(R1)              BUMP ENTRY COUNTER                         
         ST    R1,START              SAVE FOR NEXT READ                         
         B     PRTLP                 KEEP LOOPING TILL ALL ENTYS PRNTD          
*                                                                               
PRTLOOPX B     XIT                   DONE PRT (RETURN TO CLOSE QUEUE)           
         EJECT                                                                  
***********************************************************************         
* TESTADR - DO WE HAVE ADDRESS OF TEST TABLE ENTRY?                             
***********************************************************************         
TESTADR  L     RF,SRPARM3            MAKE SURE/GET A(TSTAB ENTRY)               
         USING UTLD,RF               USE UTL DSECT                              
         TM    TTEST,X'10'           DO WE HAVE A(TSTAB)                        
         BNO   ERR1                  NO, 'I=XXX' NOT SET- ERROR                 
         ICM   R5,15,TACCS           R4=ADDRESS OF TSTTAB ENTRY                 
         OC    UTLID,UTLID           IF UTL EMPTY, SET IT                       
         BNZ   *+10                  ALREADY SET DON'T OVERWRITE                
         MVC   UTLID,TUSER           SAVE USER ID (PRG CNCTD TO)                
         MVC   SUBID,2(R5)           SAVE NAME OF TEST ID                       
         MVC   SYST,TOVSYS           SAVE SYSTEM NUMBER                         
         DROP  RF                                                               
*                                                                               
         USING TSTTABD,R5            NOW USE TEST TABLE DSECT                   
         OC    TSTSCT,TSTSCT         TEST SCRIPT TRACE BUFFER PRESENT           
         BZ    ERRSCT                ERROR IF NOT DEFINED                       
         MVC   ATSSCT,TSTSCT         SAVE A(SCRIPT TRACE TABLE)                 
         MVC   TSTBUFNM,TSTTACCS     SAVE NAME OF TEST BUFFER                   
         DROP  R5                                                               
         BR    RE                    RETURN TO CALLER                           
         EJECT                                                                  
*                                                                               
* *********************************************************************         
*GETHDR: GETS THE BUFFER HEADER AND PUTS IT IN HEADER. ALSO TESTS TO            
*        SEE IF THE ENTRY REQUESTED (IN START) IS IN THE BUFFER.                
* *********************************************************************         
         SPACE 1                                                                
GETHDR   ST    RE,RTNADR             SAVE RETURN ADDRESS                        
         L     R6,ATSSCT             ADDR OF SCRIPT TRACE TABLE                 
         LA    RF,SCTHEADL           SCRIPT TRACE TABLE HEADER LENGTH           
*                                                                               
         LA    RE,GETH010            GET INTO 31-BIT MODE                       
         O     RE,=X'80000000'       TURN ON HIGH ORDER BIT                     
         BSM   0,RE                  SET THE MODE                               
GETH010  DS    0H                                                               
*                                                                               
         EX    RF,*+8                MOVE IN RF # BYTES INTO WORK               
         B     *+10                  BRANCH AROUND THE EX MVC                   
         MVC   HEADER(0),0(R6)       MOVE IN HEADER                             
         MVC   TESTDUMP,0(R6)                                                   
*                                                                               
         LA    RE,GETH020            GET OUT OF 31-BIT MODE                     
         BSM   0,RE                  TURN OFF 31-BIT MODE                       
GETH020  DS    0H                                                               
*                                                                               
         LA    R6,HEADER             PT TO HEADER TO PUT DSECT OVER             
         USING SCTTABD,R6            USE HEADER DSECT                           
         ICM   R1,15,SCTCALL         R1=# OF LAST ENTRY IN BUFFER               
         BZ    ERR6                  NO ENTRIES IN BUFFER                       
*                                                                               
         L     R1,START              GET REQUESTED ENTRY                        
         C     R1,SCTCALL            MAKE SURE IN BUFFER                        
         BH    ERR5                  NO, ERROR, NOT IN BUFFER                   
*                                                                               
         MVI   LINES,4               DEFAULT=4 LINES PER ENTRY                  
         MVI   ENTYS,3               REGULAR DISPLAY=3 ENTRIES                  
         TM    HDREXTN,HDREXTQ       EXTENDED DISPLAY?                          
         BNO   GETH030               NO                                         
         MVI   LINES,8               YES SET TO MORE LINES                      
         MVI   ENTYS,2               EXTEND  DISPLAY=2 ENTRIES                  
*                                                                               
GETH030  XC    ONOFF,ONOFF           TRACE ON IS DEFAULT                        
         TM    HDRSUSP,HDRSUSQ       IS TRACE SUSPENDED?                        
         BNO   GETH040               NO                                         
         MVI   HDRSUSP,HDRSUSQ       YES, INDICATE SO                           
*                                                                               
GETH040  L     RE,RTNADR             RESTORE RETRURN ADDRESS                    
         BR    RE                    RETURN TO CALLER                           
         DROP  R6                    RELEASE HEADER DSECT                       
         EJECT                                                                  
* ********************************************************************          
*GETFIRST: GETS FIRST ENTRY FOR SCREEN DEFINED BY START              *          
* ********************************************************************          
         SPACE 1                                                                
GETFIRST ST    RE,RTNADR             SAVE RETURN ADDRESS                        
         XC    ASCTENT,ASCTENT                                                  
         LA    RE,HEADER                                                        
         ICM   R1,15,SCTEND-SCTTABD(RE)                                         
         ICM   R0,15,SCTNEXT-SCTTABD(RE)                                        
         L     R6,ATSSCT             PT TO BEG OF TABLE                         
         AH    R6,=Y(SCTHEADL)       R6=ADDR OF 1ST ENTRY IN BUFFER             
         SR    RF,RF                                                            
         ICM   RF,15,START                                                      
         LA    RE,GETF010            GET INTO 31-BIT MODE                       
         O     RE,=X'80000000'       TURN ON HIGH ORDER BIT                     
         BSM   0,RE                  SET THE MODE                               
GETF010  DS    0H                                                               
*                                                                               
         USING SCTENTRY,R6                                                      
GETF020  DS    0H                                                               
         CR    R6,R1                                                            
         BNL   GETF040                                                          
         CR    R6,R0                                                            
         BNL   GETF040                                                          
         CLM   RF,15,SCTENUM                                                    
         BNH   GETF030                                                          
         AH    R6,=Y(SCTENTL)                                                   
         B     GETF020                                                          
*                                                                               
GETF030  STCM  R6,15,ASCTENT                                                    
         LA    RE,ENTRY                                                         
         LR    R0,R6                                                            
         LA    RF,SCTENTL                                                       
         LR    R1,RF                                                            
         MVCL  RE,R0                 MOVE IN ENTRY                              
*                                                                               
GETF040  LA    RE,GETF100            GET OUT OF 31-BIT MODE                     
         BSM   0,RE                  TURN OFF 31-BIT MODE                       
GETF100  DS    0H                                                               
         L     RE,RTNADR             RESTORE RETURN ADDRESS                     
         BR    RE                    RETURN TO CALLER                           
         EJECT                                                                  
* ********************************************************************          
*GETNEXT: GETS NEXT ENTRY FROM SCTTAB                                *          
* ********************************************************************          
         SPACE 1                                                                
GETNEXT  ST    RE,RTNADR             SAVE RETURN ADDRESS                        
         LA    RE,HEADER                                                        
         ICM   R1,15,SCTEND-SCTTABD(RE)                                         
         ICM   R0,15,SCTNEXT-SCTTABD(RE)                                        
         L     R6,ASCTENT            PT TO CURRENT A(ENTRY IN SCTTAB)           
         XC    ASCTENT,ASCTENT                                                  
         LA    RE,GETN010            GET INTO 31-BIT MODE                       
         O     RE,=X'80000000'       TURN ON HIGH ORDER BIT                     
         BSM   0,RE                  SET THE MODE                               
GETN010  DS    0H                                                               
*                                                                               
         USING SCTENTRY,R6                                                      
GETN020  AH    R6,=Y(SCTENTL)                                                   
         CR    R6,R1                                                            
         BNL   GETN040                                                          
         CR    R6,R0                                                            
         BNL   GETN040                                                          
         OC    SCTENUM,SCTENUM                                                  
         BZ    GETN040                                                          
*                                                                               
         STCM  R6,15,ASCTENT                                                    
         LA    RE,ENTRY                                                         
         LR    R0,R6                                                            
         LA    RF,SCTENTL                                                       
         LR    R1,RF                                                            
         MVCL  RE,R0                 MOVE IN ENTRY                              
*                                                                               
GETN040  LA    RE,GETN100            GET OUT OF 31-BIT MODE                     
         BSM   0,RE                  TURN OFF 31-BIT MODE                       
GETN100  DS    0H                                                               
         L     RE,RTNADR             RESTORE RETURN ADDRESS                     
         BR    RE                    RETURN TO CALLER                           
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
         L     R6,ATSSCT             GET ADDRESS OF TRACE TABLE                 
         USING SCTTABD,R6            USE DSECT FOR THE HEADER                   
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
         DROP  R6                                                               
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
* ********************************************************************          
* HEADLN: SET INFORMATION INDICATORS AT TOP OF SCREEN                           
* ********************************************************************          
         SPACE 1                                                                
HEADLN   NTR1                                                                   
         LA    R4,HEADER              PT TO TRACE BUFFER INFO                   
         USING SCTTABD,R4             USE IT'S DSECT                            
         MVC   SRVHEAD(6),=C'ERROR='                                            
         EDIT  (2,SCTSERR),(4,WORK+20),ALIGN=RIGHT,FILL=0,ZERO=NOBLANK          
         MVC   SRVHEAD+6(4),WORK+20                                             
         MVC   SRVHEAD+12(7),=C'OFFSET='                                        
         EDIT  (2,SCTSDSP),(4,WORK+20),ALIGN=RIGHT,FILL=0,ZERO=NOBLANK          
         MVC   SRVHEAD+19(4),WORK+20                                            
         MVC   SRVHEAD+25(7),=C'OUTLEN='                                        
         EDIT  (2,SCTSOUTL),(4,WORK+20),ALIGN=RIGHT,FILL=0,ZERO=NOBLANK         
         MVC   SRVHEAD+32(4),WORK+20                                            
         MVC   SRVHEAD+38(3),=C'AI='                                            
         GOTO1 VHEXOUT,DMCB,SCTAINP,SRVHEAD+41,4,=C'MIX'                        
         MVC   SRVHEAD+51(3),=C'AO='                                            
         GOTO1 VHEXOUT,DMCB,SCTAOUT,SRVHEAD+54,4,=C'MIX'                        
         MVC   SRVHEAD+64(3),=C'AW='                                            
         GOTO1 VHEXOUT,DMCB,SCTAWRK,SRVHEAD+67,4,=C'MIX'                        
*                                                                               
         OI    SRVHEADH+6,X'80'       TRANSMIT FIELD                            
         OI    SRVHEADH+6,X'08'       HIGH INTENSITY                            
*                                                                               
         XIT1                         RETURN TO CALLER                          
         DROP  R4                                                               
         EJECT                                                                  
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
         EJECT                                                                  
* ********************************************************************          
* INFOLN: SET INFORMATION INDICATORS AT BOTTOM OF SCREEN                        
* ********************************************************************          
         SPACE 1                                                                
INFOLN   NTR1                                                                   
         LA    R4,HEADER              PT TO TRACE BUFFER INFO                   
         USING SCTTABD,R4             USE IT'S DSECT                            
         MVC   SRVINFO+18(2),=C'I='   SET TEST ID                               
         MVC   SRVINFO+20(4),SUBID    SET TEST ID                               
         MVC   SRVINFO+26(6),=C'COUNT='                                         
         EDIT  (4,SCTCOUNT),(8,WORK+20),0,ALIGN=LEFT,ZERO=NOBLANK               
         MVC   SRVINFO+32(5),WORK+20                                            
         MVC   SRVINFO+39(6),=C'START='                                         
         EDIT  (4,SCTSTART),(8,WORK+20),0,ALIGN=LEFT,ZERO=NOBLANK               
         MVC   SRVINFO+45(5),WORK+20                                            
         MVC   SRVINFO+52(6),=C'INTVL='                                         
         EDIT  (4,SCTINT),(8,WORK+20),0,ALIGN=LEFT,ZERO=NOBLANK                 
         MVC   SRVINFO+58(5),WORK+20                                            
         MVC   SRVINFO+65(5),=C'CALL='                                          
         EDIT  (4,SCTCALL),(8,WORK+20),0,ALIGN=LEFT,ZERO=NOBLANK                
         MVC   SRVINFO+70(5),WORK+20                                            
*                                                                               
         OI    SRVINFOH+6,X'80'       TRANSMIT FIELD                            
         OI    SRVINFOH+6,X'08'       HIGH INTENSITY                            
*                                                                               
         XIT1                         RETURN TO CALLER                          
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
* ********************************************************************          
* DISPSCR: DISPLAY ENTRY ON SCREEN.  SCRADR--PTS TO AVAIL PRINT LINE.           
* ********************************************************************          
         SPACE 1                                                                
DISPSCR  NTR1                        SAVE REGISTERS                             
         LA    R6,ENTRY              POINT TO ENTRY                             
         USING SCTENTRY,R6           USE IT'S DSECT                             
         L     R7,SCRADR             GET ADDRESS OF NEXT LINE ON SCREEN         
         USING SCRND,R7              USE SCREEN-LINE DSECT                      
*                                                                               
         LA    RF,SRVINFO            SEE IF SCRN IS FULL                        
         C     RF,SCRADR             ARE WE STILL WITHIN SCREEN?                
         BNH   DSCR100               NO, EXIT                                   
*                                                                               
DENU     EQU   *                                                                
         EDIT  SCTENUM,(6,SCRENTY),0,ALIGN=LEFT,ZERO=NOBLANK                    
DENUX    EQU   *                                                                
*                                                                               
DOFF     EQU   *                                                                
         EDIT  SCTESOFF,(5,SCROFF),0,ALIGN=RIGHT,FILL=0,ZERO=NOBLANK            
DOFFX    EQU   *                                                                
*                                                                               
DSNA     EQU   *                                                                
         MVC   WORK(2),SCTECODE                                                 
         BAS   RE,GETSCNAM           GET SCRIPT COMMAND NAME                    
         MVC   SCRNAME,WORK                                                     
DSNAX    EQU   *                                                                
*                                                                               
DLOG     EQU   *                                                                
         OC    SCTELOGL,SCTELOGL                                                
         BZ    DLOGX                                                            
         MVC   SCRLOGL,SCTELOGL                                                 
         TR    SCRLOGL,TRTABLE                                                  
DLOGX    EQU   *                                                                
*                                                                               
         ZIC   R1,0(R7)              LENGTH OF FIELD                            
         AR    R7,R1                 BUMP TO NEXT LINE                          
         ST    R7,SCRADR             SAVE SCREEN ADDRESS                        
         MVC   SCRSCRTG,=CL6'SCREEN'                                            
         MVC   SCRTIATG,=CL6'TIA   '                                            
*                                                                               
DPROT    EQU   *                                                                
         OC    SCTEPROT,SCTEPROT                                                
         BZ    DPROX                                                            
         MVC   SCRPROT,SCTEPROT                                                 
         TR    SCRPROT,TRTABLE                                                  
DPROX    EQU   *                                                                
*                                                                               
DTIA     EQU   *                                                                
         OC    SCTETIA,SCTETIA                                                  
         BZ    DTIAX                                                            
         MVC   SCRTIA,SCTETIA                                                   
         TR    SCRTIA,TRTABLE                                                   
DTIAX    EQU   *                                                                
*                                                                               
         CLI   EXTN,HDREXTQ          TEST FOR EXTENDED FORMAT TRACE             
         BE    DEXT                                                             
*                                                                               
         ZIC   R1,0(R7)              LENGTH OF FIELD                            
         AR    R7,R1                 BUMP TO NEXT LINE                          
         ST    R7,SCRADR             SAVE SCREEN ADDRESS                        
         MVC   SCRTAGST,=CL6'STATUS'                                            
*                                                                               
DCON     EQU   *                                                                
         TM    SCTECOND,X'01'                                                   
         BZ    *+12                                                             
         MVI   SCRCOND,C'E'                                                     
         B     DCONX                                                            
         TM    SCTECOND,X'02'                                                   
         BZ    *+12                                                             
         MVI   SCRCOND,C'L'                                                     
         B     DCONX                                                            
         TM    SCTECOND,X'04'                                                   
         BZ    *+12                                                             
         MVI   SCRCOND,C'H'                                                     
         B     DCONX                                                            
         MVI   SCRCOND,C'U'                                                     
DCONX    EQU   *                                                                
*                                                                               
DLAB     EQU   *                                                                
         OC    SCTESLAB,SCTESLAB                                                
         BZ    *+10                                                             
         MVC   SCRSLAB,SCTESLAB                                                 
         OC    SCTEWLAB,SCTEWLAB                                                
         BZ    *+10                                                             
         MVC   SCRWLAB,SCTEWLAB                                                 
         OC    SCTEFLAB,SCTEFLAB                                                
         BZ    *+10                                                             
         MVC   SCRFLAB,SCTEFLAB                                                 
         CLC   SCRSLAB,SPACES                                                   
         BNE   *+10                                                             
         MVC   SCRSLAB,=C'..'                                                   
         CLC   SCRWLAB,SPACES                                                   
         BNE   *+10                                                             
         MVC   SCRWLAB,=C'..'                                                   
         CLC   SCRFLAB,SPACES                                                   
         BNE   *+10                                                             
         MVC   SCRFLAB,=C'..'                                                   
DLABX    EQU   *                                                                
*                                                                               
DFLN     EQU   *                                                                
         EDIT  SCTETFLN,(3,SCRTFLN),0,ALIGN=RIGHT,FILL=0,ZERO=NOBLANK           
         EDIT  SCTETSCN,(3,SCRTSCN),0,ALIGN=RIGHT,FILL=0,ZERO=NOBLANK           
         EDIT  SCTETSTN,(3,SCRTSTN),0,ALIGN=RIGHT,FILL=0,ZERO=NOBLANK           
DFLNX    EQU   *                                                                
*                                                                               
DSYS     EQU   *                                                                
         GOTO1 VHEXOUT,DMCB,SCTESYS,SCRSYS,1,=C'MIX'                            
DSYSX    EQU   *                                                                
*                                                                               
DPRG     EQU   *                                                                
         GOTO1 VHEXOUT,DMCB,SCTEPRG,SCRPRG,1,=C'MIX'                            
DPRGX    EQU   *                                                                
*                                                                               
DUID     EQU   *                                                                
         GOTO1 VHEXOUT,DMCB,SCTEUID,SCRUID,2,=C'MIX'                            
DUIDX    EQU   *                                                                
*                                                                               
DPWD     EQU   *                                                                
         GOTO1 VHEXOUT,DMCB,SCTEPWD,SCRPWD,2,=C'MIX'                            
DPWDX    EQU   *                                                                
*                                                                               
DAGS     EQU   *                                                                
         MVC   SCRAGYS,SCTEAGYS                                                 
DAGSX    EQU   *                                                                
*                                                                               
         ZIC   R1,0(R7)              LENGTH OF FIELD                            
         AR    R7,R1                 BUMP TO NEXT LINE                          
         ST    R7,SCRADR             SAVE SCREEN ADDRESS                        
         MVC   SCRTAGTW,=CL3'TWA'                                               
*                                                                               
DTWA     EQU   *                                                                
         EDIT  SCTETOFF,(5,SCRTOFF),0,ALIGN=RIGHT,FILL=0,ZERO=NOBLANK           
         MVC   SCRTWAFD,SCTEFLD                                                 
         TR    SCRTWAFD,TRTABLE                                                 
         GOTO1 VHEXOUT,DMCB,SCTEFLDH,SCRTWAFH,8,=C'MIX'                         
DTWAX    EQU   *                                                                
*                                                                               
DRES     EQU   *                                                                
         MVC   SCRRES,SCTERES                                                   
         TR    SCRRES,TRTABLE                                                   
DRESX    EQU   *                                                                
*                                                                               
DERR     EQU   *                                                                
         MVC   SCRERRM,SCTEERRM                                                 
         TR    SCRERRM,TRTABLE                                                  
DERRX    EQU   *                                                                
         B     DSCR100                                                          
*                                                                               
DEXT     EQU   *                                                                
         ZIC   R1,0(R7)              LENGTH OF FIELD                            
         AR    R7,R1                 BUMP TO NEXT LINE                          
         ST    R7,SCRADR             SAVE SCREEN ADDRESS                        
*                                                                               
DINP     EQU   *                                                                
         MVC   SCRTAGD,=CL6'INPUT '                                             
         EDIT  SCTEOINP,(5,SCROFFD),0,ALIGN=RIGHT,FILL=0,ZERO=NOBLANK           
*                                                                               
         LA    R1,19                                                            
         EXMVC R1,SCREBCD,SCTEINP                                               
         TR    SCREBCD,TRTABLE                                                  
*                                                                               
         GOTO1 VHEXOUT,DMCB,SCTEINP,SCRHEXD,20,=C'MIX'                          
*                                                                               
         ZIC   R1,0(R7)              LENGTH OF FIELD                            
         AR    R7,R1                 BUMP TO NEXT LINE                          
         ST    R7,SCRADR             SAVE SCREEN ADDRESS                        
*                                                                               
         LA    R1,19                                                            
         EXMVC R1,SCREBCD,SCTEINP+20                                            
         TR    SCREBCD,TRTABLE                                                  
*                                                                               
         GOTO1 VHEXOUT,DMCB,SCTEINP+20,SCRHEXD,20,=C'MIX'                       
*                                                                               
         ZIC   R1,0(R7)              LENGTH OF FIELD                            
         AR    R7,R1                 BUMP TO NEXT LINE                          
         ST    R7,SCRADR             SAVE SCREEN ADDRESS                        
*                                                                               
DINPX    EQU   *                                                                
*                                                                               
DOUT     EQU   *                                                                
         MVC   SCRTAGD,=CL6'OUTPUT'                                             
         EDIT  SCTEOOUT,(5,SCROFFD),0,ALIGN=RIGHT,FILL=0,ZERO=NOBLANK           
*                                                                               
         LA    R1,19                                                            
         EXMVC R1,SCREBCD,SCTEOUT                                               
         TR    SCREBCD,TRTABLE                                                  
*                                                                               
         GOTO1 VHEXOUT,DMCB,SCTEOUT,SCRHEXD,20,=C'MIX'                          
*                                                                               
         ZIC   R1,0(R7)              LENGTH OF FIELD                            
         AR    R7,R1                 BUMP TO NEXT LINE                          
         ST    R7,SCRADR             SAVE SCREEN ADDRESS                        
*                                                                               
         LA    R1,19                                                            
         EXMVC R1,SCREBCD,SCTEOUT+20                                            
         TR    SCREBCD,TRTABLE                                                  
*                                                                               
         GOTO1 VHEXOUT,DMCB,SCTEOUT+20,SCRHEXD,20,=C'MIX'                       
*                                                                               
         ZIC   R1,0(R7)              LENGTH OF FIELD                            
         AR    R7,R1                 BUMP TO NEXT LINE                          
         ST    R7,SCRADR             SAVE SCREEN ADDRESS                        
*                                                                               
DOUTX    EQU   *                                                                
*                                                                               
DWRK     EQU   *                                                                
         MVC   SCRTAGD,=CL6'WORK  '                                             
         EDIT  SCTEOWRK,(5,SCROFFD),0,ALIGN=RIGHT,FILL=0,ZERO=NOBLANK           
*                                                                               
         LA    R1,19                                                            
         EXMVC R1,SCREBCD,SCTEWRK                                               
         TR    SCREBCD,TRTABLE                                                  
*                                                                               
         GOTO1 VHEXOUT,DMCB,SCTEWRK,SCRHEXD,20,=C'MIX'                          
*                                                                               
         ZIC   R1,0(R7)              LENGTH OF FIELD                            
         AR    R7,R1                 BUMP TO NEXT LINE                          
         ST    R7,SCRADR             SAVE SCREEN ADDRESS                        
*                                                                               
         LA    R1,19                                                            
         EXMVC R1,SCREBCD,SCTEWRK+20                                            
         TR    SCREBCD,TRTABLE                                                  
*                                                                               
         GOTO1 VHEXOUT,DMCB,SCTEWRK+20,SCRHEXD,20,=C'MIX'                       
*                                                                               
DWRKX    EQU   *                                                                
*                                                                               
DSCR100  ZIC   R1,0(R7)              LENGTH OF FIELD                            
         AR    R7,R1                 BUMP TO NEXT LINE--LEAVE BLNK LINE         
         ST    R7,SCRADR             SAVE SCREEN ADDRESS                        
*                                                                               
DSCRX    AR    R7,R1                 SKIP A LINE                                
         ST    R7,SCRADR                                                        
         B     XIT                   EXIT                                       
*                                                                               
         EJECT                                                                  
         DROP  R6,R7                                                            
         EJECT                                                                  
* ********************************************************************          
*GETSCNAM:GET SCRIPT COMMNAD NAME FROM DDSCRAMTAB                    *          
* ********************************************************************          
         SPACE 1                                                                
GETSCNAM NTR1                                                                   
         LA    RF,ASMTAB                                                        
         USING ASMTABD,RF                                                       
GSCN010  CLI   0(RF),0                                                          
         BE    GSCN030                                                          
         CLC   OPCODE,WORK                                                      
         BE    GSCN020                                                          
         LA    RF,ASMTABLQ(RF)                                                  
         B     GSCN010                                                          
GSCN020  MVC   WORK(6),MNEMONIC                                                 
GSCN030  DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
* ********************************************************************          
* PRTENTY: PRINT ENTRY ON REPORT LINE                                           
* ********************************************************************          
         SPACE 1                                                                
PRTENTY  NTR1                        SAVE REGISTERS                             
         LA    R7,PRTLINE            PR TO PRINT LINE                           
         USING PRTLND,R7             USE PRINT LINE DSECT                       
         XC    PRTLINE,PRTLINE       CLEAR PRT LINE                             
*                                                                               
PRT40A   CLC   CNT,LINES             HAVE WE PRINTED ALL LINES?                 
         XC    PRTLINE,PRTLINE       CLEAR LINE FOR NEW DATA                    
*        BNH   PRT10                 NO, KEEP PRINTING                          
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
         CLI   LNCNT,X'FF'         IF CNT=FF, DO A PG BREAK                     
         BNE   *+12                NO                                           
         MVI   PQLINE,X'89'        PAGE BREAK                                   
         B     *+8                                                              
         MVI   PQLINE,X'09'                                                     
*                                                                               
         MVC   PQLINE+1(80),0(R7)                                               
         ZIC   RF,LNCNT            BUMP LINE COUNTER                            
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
         EJECT                                                                  
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
ERR7     MVC   MSG(32),=C'$SCT ONLY VALID ON TEST SYSTEMS'                      
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
ERRSCT   MVC   MSG(30),=C'NO SCRIPT TRACE BUFFER DEFINED'                       
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
* TRANSLATION TABLE TO DISPLAY ONLY DISPLAYABLE CHARS OR '.'                    
TRTABLE  DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     00-0F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     10-1F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     20-2F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     30-3F                    
         DC    X'404B4B4B4B4B4B4B4B4B4B4B4B4D4E4B'     40-4F                    
         DC    X'504B4B4B4B4B4B4B4B4B4B5B5C5D5E4B'     50-5F                    
         DC    X'60614B4B4B4B4B4B4B4B4B6B6C6D6E6F'     60-6F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B7A7B4B7D7E4B'     70-7F                    
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
*                                                                               
* DDSCRAMTAB                                                                    
       ++INCLUDE DDSCRAMTAB                                                     
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
ATSSCT   DS    A                     ADDRESS OF SCRIPT TRACE TABLE              
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
LINES    DS    X                     # OUTPUT LINES-DEFLT=5 EXTN(X)=8           
LNCNT    DS    X                     COUNTS THE # LINES ON THE PAGE             
DATALN   DS    X                     # OUTPUT LINES-FOR DMTRACE                 
ENTYS    DS    X                     # ENTRIES TO DISPLAY ON SCREEN             
CNT      DS    X                     # OUTPUT LINES-DEFLT=5 EXTN(X)=8           
PRNTD    DS    X                     #LINES PRINTED SO FAR                      
IODSP    DS    X                     # BYTES OF EITHER INP OUR OUT DISP         
INDISP   DS    X                     # BYTES OF DMCBW3 TO DISPLAY               
OUTDISP  DS    X                     # BYTES OF DMCBW4 TO DISPLAY               
*                                                                               
START    DS    F                     OFFST OF ENTRY IN BUFFER REQUESTD          
INPNUM   DS    F                     INPUT NUMBER FROM USER                     
*                                                                               
INTVL    DS    F                     INTERVAL BETWEEN ACTUAL I/O'S              
TEMP     DS    F                     TEMP STORAGE                               
RTNADR   DS    F                     RETURN ADDRESS FROM A BAS                  
SCRADR   DS    F                     ADDR OF NEXT AVAIL LINE ON SCRN            
*                                                                               
         DS    0F                    FULL WORD ALLIGNMENT                       
ASCTENT  DS    AL4                   A(SCTTAB ENTRY)                            
HEADER   DS    CL(SCTHEADL)          HEADER COPIED FROM BUFFER                  
HDRSUSP  DS    XL1                                                              
HDRSUSQ  EQU   X'40'                 SUSPEND TRACE                              
HDREXTN  DS    XL1                                                              
HDREXTQ  EQU   X'80'                 EXTEND=X'80'                               
FLAG     DS    CL1                   EITHER C'I' OR C'O' INPUT/OUTPUT           
ENTRY    DS    CL(SCTENTL)           ONE TRACE ENTRY COPIED FROM BUFFER         
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
TESTDUMP DS    CL255                                                            
*                                                                               
         DS    0D                                                               
TBLOCK   DS    1920C                                                            
TWA0     DS    14336X                                                           
WRKX     DS    0H                                                               
         EJECT                                                                  
*                                                                               
GLVSPID  EQU   X'01'                 LAST REPORT ADDED TO PRT QUEUE             
*                                                                               
OUTQ     EQU   20                    #BYTES TO OUTPUT ON LINE                   
         EJECT                                                                  
*                                                                               
SCRND    DSECT                       DSECT FOR SCREEN LINE                      
SCRHDR   DS    CL8                   LINE HEADER                                
SCRLN    DS    0CL(L'SRVLN1)         TO CLEAR OUTPUT LINE                       
         ORG   SCRLN                                                            
SCRENTY  DS    CL6                   ENTRY NUMBER                               
         DS    CL1                                                              
SCROFF   DS    CL5                   OFFSET INTO SCRIPT                         
         DS    CL1                                                              
SCRNAME  DS    CL6                   COMMAND NAME                               
         DS    CL1                                                              
SCRLOGL  DS    CL40                  EVENT LOG LINE                             
*                                                                               
         ORG   SCRLN                                                            
SCRSCRTG DS    CL6                   SCREEN INFO TAG                            
         DS    CL1                                                              
SCRPROT  DS    CL20                  LAST PROTECTED FIELD                       
         DS    CL1                                                              
SCRTIATG DS    CL6                   TIA INFO TAG                               
         DS    CL1                                                              
SCRTIA   DS    CL40                  CURRENT TIA FIELD                          
*                                                                               
         ORG   SCRLN                                                            
SCRTAGST DS    CL6                   STATUS INFO TAG                            
         DS    CL1                                                              
SCRCOND  DS    CL1                   CONDITION CODE                             
         DS    CL1                                                              
SCRSLAB  DS    CL2                   LAST SCRIPT LABEL                          
         DS    CL1                                                              
SCRWLAB  DS    CL2                   LAST WORK AREA LABEL                       
         DS    CL1                                                              
SCRFLAB  DS    CL2                   LAST FIELD LABEL                           
         DS    CL1                                                              
SCRTFLN  DS    CL3                   CURRENT TWA FIELD#                         
         DS    CL1                                                              
SCRTSCN  DS    CL3                   CURRENT TWA SCREEN#                        
         DS    CL1                                                              
SCRTSTN  DS    CL3                   CURRENT TWA STEREO FIELD#                  
         DS    CL1                                                              
SCRSYS   DS    CL2                   CONNECTED SYSTEM HEX                       
         DS    CL1                                                              
SCRPRG   DS    CL2                   CONNECTED SYSTEM HEX                       
         DS    CL1                                                              
SCRUID   DS    CL4                   CONNECTED USERID#                          
         DS    CL1                                                              
SCRPWD   DS    CL4                   CONNECTED PASSWORD# HEX                    
         DS    CL1                                                              
SCRAGYS  DS    CL2                   CONNECTED SECURITY AGENCY                  
*                                                                               
         ORG   SCRLN                                                            
SCRTAGTW DS    CL3                   TWA INFO TAG                               
         DS    CL4                                                              
SCRTOFF  DS    CL5                   OFFSET TO CURRENT TWA FIELD                
         DS    CL1                                                              
SCRTWAFH DS    CL16                  TWA FIELD HEADER HEX                       
         DS    CL1                                                              
SCRTWAFD DS    CL20                  TWA FIELD DATA SNAPSHOT                    
         DS    CL1                                                              
SCRRES   DS    CL8                   RESULTS FIELD                              
         DS    CL1                                                              
SCRERRM  DS    CL10                  LAST ERROR MESSAGE SNAPSHOT                
*                                                                               
         ORG   SCRLN                                                            
SCRTAGD  DS    CL6                   DATA INFO TAG                              
         DS    CL1                                                              
SCROFFD  DS    CL5                   OFFSET IN DATA AREA                        
         DS    CL1                                                              
SCRHEXD  DS    CL40                  SNAP SHOT HEX DATA AREA                    
         DS    CL1                                                              
SCREBCD  DS    CL20                  SNAP SHOT EBCDIC DATA AREA                 
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
*                                                                               
ASMTABD  DSECT                                                                  
MNEMONIC DS    CL6                 MNEMONIC CORRESPONDING TO OP-CODE            
MTCHLEN  DS    AL1                 LENGTH-1 FOR EXECUTED COMPARE                
OPCODE   DS    CL2                 2 CHARACTER OP-CODE                          
MXOPERS  DS    AL1                 MAX # OF OPERANDS FOR THIS OP-CODE           
MXLBL    EQU   X'80'               INSTRUCTION IS A LABEL                       
MXBRNCH  EQU   X'40'               INSTRUCTION IS A BRANCH                      
DATATYPE DS    CL1                 WHICH CLASS OF DATA TO VALIDATE              
DATALEN  DS    AL1                 MAX LENGTH OF DATA TO FOLLOW                 
ASMTABLQ EQU   *-ASMTABD                                                        
         EJECT                                                                  
*                                                                               
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
* SRSCTFFD                                                                      
SRSCTFFD DSECT                                                                  
         DS    CL64                                                             
       ++INCLUDE SRSCTFFD                                                       
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
**PAN#1  DC    CL21'004SRSCT00   05/28/08'                                      
         END                                                                    
