*          DATA SET SEACS10    AT LEVEL 006 AS OF 05/01/02                      
*PHASE TA0D10A                                                                  
ACS10    TITLE '- SECURITY ACCESS - LIMIT ACCESS GROUP RECORDS'                 
         CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACS10*,RA,RR=RE                                              
         USING WORKD,R7            R7=A(GLOBAL W/S)                             
         USING TWAD,R5             R5=A(TWA)                                    
         USING SAVAREA,R6          R6=A(GLOBAL SAVE AREA)                       
         LA    R2,IOKEY            R2=A(RECORD KEY)                             
         USING SALAREC,R2                                                       
         L     RC,ASYSWORK                                                      
         USING SYSWORK,RC          RC=A(SYSTEM + LOCAL W/S)                     
         L     R1,ACOM                                                          
         L     R1,CXSORT-COMFACSD(R1)                                           
         ST    R1,VXSORT                                                        
         ST    RE,APRELO                                                        
         ST    RB,APBASE1                                                       
         ST    RA,APBASE2                                                       
         ST    RD,APWORKA                                                       
*                                                                               
         OC    OPTAGY,OPTAGY                                                    
         BZ    *+14                                                             
         MVC   AGENCYID,OPTAGY                                                  
         B     *+10                                                             
         MVC   AGENCYID,CUAALF                                                  
*                                                                               
         XR    RF,RF                                                            
         IC    RF,APMODE                                                        
         SLL   RF,2                                                             
         B     *+0(RF)                                                          
         SPACE 1                                                                
         B     VALKEY              01 - APMVALK                                 
         B     VALREC              02 - APMVALR                                 
         B     DISKEY              03 - APMDISK                                 
         B     DISREC              04 - APMDISR                                 
         B     DELREC              05 - APMDELR                                 
         B     RESREC              06 - APMRESR                                 
         B     VALSEL              07 - APMVALP                                 
         B     GETSEL              08 - APMGETS                                 
         B     DISSEL              09 - APMDISS                                 
         B     EXIT                10 - APMVALS                                 
         B     FSTLST              11 - APMFLST                                 
         B     VAL2DEL             12 - APMPROC                                 
         B     EXIT                13 - APMFSCR                                 
         B     LSTSCR              14 - APMLSCR                                 
         B     VALREQ              15 - APMVALQ                                 
         B     PRTREP              16 - APMREPP                                 
         B     EXIT                17 - APMSETT                                 
         B     PUTKEY              18 - APMPUTK                                 
         B     EXIT                19 - APMNEWK                                 
         B     EXIT                20 - APMFRP                                  
         B     DISCHGS             21 - APMDISS2                                
         SPACE 1                                                                
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE KEY OF A LIMIT ACCESS GROUP RECORD              *         
***********************************************************************         
         SPACE 1                                                                
VALKEY   LA    R2,IOKEY                                                         
         XC    SALAKEY,SALAKEY       SET UP KEY:                                
         MVC   SALATYP(RTYPL),RTYP     RECORD TYPE (CONSTANT)                   
         MVC   SALAAGY,AGENCYID        AGENCY FROM LOGON                        
         CLC   OPTAGY,=AL2(0)          AGENCY OPTION OVERRIDE                   
         BE    *+10                                                             
         MVC   SALAAGY,OPTAGY          AGENCY FROM OPTION                       
         SPACE 1                                                                
         LA    R1,LAGLACH              GROUP CODE FROM SCREEN                   
         MVI   FVMAXL,L'LAGLAC        (IF IT'S OK)                              
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL                                                            
         BNE   VALKEYX                                                          
         MVC   SALAAGR,FVIFLD                                                   
         SPACE 1                                                                
         MVC   APRECKEY(L'SALAKEY),SALAKEY  SAVE KEY FOR LATER                  
         SPACE 1                                                                
         LA    R1,IORDD+IOCONFIL+IO1 SETUP FOR I/O                              
         CLI   APACTN,ACTDIS       DON'T LOCK FOR ACTION 'DISPLAY'              
         BE    *+8                                                              
         LA    R1,IOLOCK(R1)                                                    
         GOTO1 AIO                 DO THE I/O                                   
         BE    VKEY2                                                            
         BL    VALKEYX             I/O ERROR EXIT                               
*                                                                               
         MVI   APINDS,APIOKADD                                                  
         TM    IOERR,IOEDEL        TEST RECORD IS DELETED                       
         BZ    *+8                                                              
         MVI   APINDS,APIOKDIS+APIOKRES                                         
         B     VALKEYY                                                          
         SPACE 1                                                                
VKEY2    EQU   *                   CHECK WHETHER ITS OK TO DELETE               
         L     R2,AIOAREA1         R2=A(RECORD)                                 
         USING SAPCTD,R4           R4=A(PERSON COUNT ELEMENT)                   
         LA    R4,SALADATA         FIND THE PERSON COUNT ELEMENT                
         XR    RF,RF                                                            
         CLI   SAPCTEL,SAPCTELQ                                                 
         BE    *+12                                                             
         IC    RF,SAPCTLN                                                       
         BXH   R4,RF,*-12                                                       
         SPACE 1                                                                
         CLC   SAPCTVAL,=H'0'      CAN ONLY DEL IF PERSON COUNT IS 0            
         BE    *+12                                                             
         MVI   APINDS,APIOKDIS+APIOKCHA                                         
         B     *+8                                                              
         MVI   APINDS,APIOKDIS+APIOKCHA+APIOKDEL                                
         SPACE 1                                                                
*                                                                               
VALKEYY  MVI   FVMINL,1            READ SYSTEM FIELD                            
         GOTO1 AFVAL,LAGSYSH                                                    
         BNE   VK100                                                            
         GOTO1 AVALSYS,LAGSYSH     VALIDATE SYSTEM                              
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFESYS)                                            
         B     VALKEYX             INVALID SYSTEM                               
         MVC   SYSTEM,APWORK       HOLD FOR LATER USE                           
*                                                                               
VK100    CLI   SYSTEM,0            IF NO SYSTEM INPUT                           
         BNE   VK110                                                            
         CLI   APACTN,ACTADD       AND NOT ADD ACTION                           
         BE    VK110                                                            
*                                  DEFAULT TO FIRST IN RECORD                   
         XC    APELEM,APELEM                                                    
         MVI   APELEM,SALASELQ                                                  
         L     R2,AIOAREA1                                                      
         GOTO1 AGETELS,(R2)                                                     
         ICM   R1,15,APPARM                                                     
         BZ    VK110                                                            
         MVC   SYSTEM,SALASNUM-SALASD(R1)                                       
*                                                                               
VK110    MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
         SPACE 1                                                                
VALKEYX  B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD OR CHANGE AN ACCESS GROUP RECORD                     *         
***********************************************************************         
         SPACE 1                                                                
VALREC   EQU   *                                                                
         L     R2,AIOAREA1         R2=A(RECORD)                                 
         XC    PCOUNT,PCOUNT                                                    
         XC    SVLASALL,SVLASALL                                                
         CLI   APACTN,ACTADD                                                    
         BE    VALRECA                                                          
         CLI   SYSTEM,0                                                         
         BE    DISREC                                                           
         CLC   SYSTEM,SAVSYS                                                    
         BNE   DISREC                                                           
*                                                                               
VALRECC  EQU   *                   PROCESS CHANGE ACTION                        
         LA    R3,SALADATA                                                      
VR010    CLI   0(R3),0             END OF RECORD                                
         BE    VR100                                                            
         CLI   0(R3),SALASELQ                                                   
         BE    VR020                                                            
         CLI   0(R3),SAPCTELQ                                                   
         BE    VR030                                                            
         B     VR014                                                            
VR012    XR    RF,RF                                                            
         IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     VR010                                                            
*                                                                               
VR014    SR    R0,R0               DELETE ELEMENT                               
         ICM   R0,1,0(R3)                                                       
         MVC   APWORK(1),2(R3)                                                  
         GOTO1 VHELLO,APPARM,(C'D',CTFILE),((R0),SALAREC),(1,APWORK)            
         CLI   APPARM+12,0                                                      
         BE    *+6                                                              
         DC    H'00'                                                            
         B     VR010                                                            
*                                                                               
         USING SALASD,R3                                                        
VR020    CLI   SYSTEM,0            DELETE SYSTEM ELEMENT IF SPECIFIED           
         BE    VR012                                                            
         CLC   SYSTEM,SALASNUM                                                  
         BNE   VR012                                                            
         MVC   SVLASALL,SALASALL                                                
         B     VR014                                                            
*                                                                               
         USING SAPCTD,R3                                                        
VR030    MVC   PCOUNT,SAPCTVAL     SAVE PERSON COUNT                            
         B     VR014                                                            
*                                                                               
VALRECA  EQU   *                   FORMAT RECORD FOR ADD.                       
         XC    PCOUNT,PCOUNT                                                    
         LA    R2,IOKEY                                                         
         L     R2,AIOAREA1         INITIALISE RECORD                            
         XC    SALAKEY(SALAFRST+1),SALAKEY                                      
         MVC   SALAKEY,APRECKEY    REMOVE ALL ELEMENTS                          
         LA    R0,SALAFRST+1                                                    
         STCM  R0,3,SALALEN                                                     
         EJECT                                                                  
         USING SALAND,R3                                                        
VR100    LA    R3,APELEM           BUILD THE ELEMENT DATA                       
         XC    APELEM,APELEM                                                    
         MVI   SALANEL,SALANELQ                                                 
         MVI   SALANLN,SALANLNQ                                                 
         LA    R1,LAGLANH          GROUP NAME FROM SCREEN                       
         MVI   FVMAXL,L'LAGLAN        (IF IT'S OK)                              
         MVI   FVMINL,0            0 => IT'S OPTIONAL INPUT                     
         GOTO1 AFVAL                                                            
         BH    VALRECX             (SHOULDNT HAPPEN)                            
         BL    VR110               LOW => FIELD NOT INPUT                       
*                                                                               
         MVC   LAGLAN,FVIFLD       RETRANSMIT NAME                              
         OI    LAGLANH+FHOID,FHOITR                                             
         XR    RF,RF               MOVE IT INTO ELEMENT                         
         IC    RF,FVXLEN           RF = LENGTH OF INPUT - 1                     
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SALANNAM(0),FVIFLD                                               
*                                                                               
         LA    RF,1(RF)            BUMP RF TO ACTUAL LENGTH                     
         LA    RE,SALANLNQ         RECALCULATE ELEMENT LENGTH                   
         AR    RE,RF                                                            
         STC   RE,SALANLN                                                       
*                                  NAME NOT I/P SO ELEMENT SHORT                
VR110    MVC   LAGLAC,SALAAGR      RETRANSMIT CODE                              
         OI    LAGLACH+FHOID,FHOITR                                             
         GOTO1 AADDELS,SALAREC     ADD GROUP NAME ELEMENT                       
*                                  PROCESS STAFF COUNT ELEMENT                  
         USING SAPCTD,R3                                                        
         XC    APELEM,APELEM                                                    
         MVI   SAPCTEL,SAPCTELQ                                                 
         MVI   SAPCTLN,SAPCTLNQ                                                 
         MVC   SAPCTVAL,PCOUNT                                                  
         GOTO1 AADDELS,SALAREC     ADD PERSON COUNT ELEMENT                     
*                                  PROCESS SYSTEM ELEMENT                       
         USING SALASD,R3                                                        
         CLI   SYSTEM,0            UNLESS NONE ENTERED                          
         BE    VR200                                                            
         XC    APELEM,APELEM                                                    
         MVI   SALASEL,SALASELQ                                                 
         MVI   SALASLN,SALASLNQ                                                 
         MVC   SALASNUM,SYSTEM                                                  
         GOTO1 AFVAL,LAGALLH       READ DEFAULT ACCESS CODE                     
         BE    VR120                                                            
         B     VR130                                                            
*                                  TEST FOR DELETE CURRENT SYSTEM               
VR120    CLI   FVILEN,1                                                         
         BNE   VR130                                                            
         CLI   FVIFLD,C'D'                                                      
         BNE   VR130                                                            
         CLI   APACTN,ACTADD       INVALID FOR ADD ACTION                       
         BE    SAEIIF                                                           
         B     VR200               ELSE DO NOT RESTORE ELEMENT                  
*                                  VALIDATE DEFAULT ACCESS CODE                 
*                                    FOR ALL PROGRAMS                           
VR130    XC    SALASALL,SALASALL                                                
         CLI   FVILEN,0                                                         
         BE    VR140                                                            
         GOTO1 AVALLACC,APPARM,(SALASNUM,SALASALL)                              
         BNE   EXIT                                                             
*                                  VALIDATE ACCESS CODES                        
VR140    BAS   RE,VALPLAC            FOR EACH PROGRAM IN TABLE                  
         BNE   EXIT                                                             
*                                                                               
VR150    GOTO1 AADDELS,SALAREC     REPLACE UPDATED SYSTEM EL                    
         B     VR200                                                            
         EJECT                                                                  
VR200    GOTO1 ASETACT,SALAREC     UPDATE ACTIVITY ELEMENT                      
*                                  UPDATE RECORD ON FILE                        
         MVC   IOKEY(L'SALAKEY),APRECKEY  RESTORE LACCESS RECORD KEY            
         CLI   APACTN,ACTADD                                                    
         BE    *+12                                                             
         LA    R1,IOWRITE+IOCONFIL+IO1                                          
         B     *+8                                                              
         LA    R1,IOADD+IOCONFIL+IO1                                            
         GOTO1 AIO                 PERFORM THE I/O                              
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     DISREC                                                           
*                                                                               
VALRECX  B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY KEY OF AN ACCESS GROUP RECORD                    *         
***********************************************************************         
         SPACE 1                                                                
DISKEY   LA    R2,APRECKEY                                                      
         TWAXC LAGLACH,LAGLACH                                                  
         OI    LAGLACH+FHOID,FHOITR                                             
         MVI   LAGLAC,C' '                                                      
         MVC   LAGLAC+1(L'LAGLAC-1),LAGLAC                                      
         MVC   LAGLAC,SALAAGR                                                   
         CLI   SYSTEM,0                                                         
         BE    DISKEYX                                                          
         GOTO1 ADISSYS,SYSTEM                                                   
         MVC   LAGSYS,APWORK                                                    
         SPACE 1                                                                
DISKEYX  B     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO DISPLAY ACCESS GROUP RECORD                              *         
***********************************************************************         
         SPACE 1                                                                
DISREC   L     R2,AIOAREA1                                                      
         OI    LAGLACH+FHOID,FHOITR                                             
         MVI   LAGLAC,C' '                                                      
         MVC   LAGLAC+1(L'LAGLAC-1),LAGLAC                                      
         MVC   LAGLAC,SALAAGR                                                   
         TWAXC LAGLANH                                                          
         TWAXC LAGPN01H,PROT=Y                                                  
         BAS   RE,REENTER          HANDLE DELETE/RESTORE PROMPT                 
         BAS   RE,DISSHD           DISPLAY SYSTEM SUMMARY HEADER                
         CLI   SYSTEM,0                                                         
         BE    DR002                                                            
         GOTO1 ADISSYS,SYSTEM      REDISPLAY SYSTEM NAME                        
         MVC   LAGSYS,APWORK                                                    
         BAS   RE,INITPGML                                                      
DR002    LA    R3,SALADATA         GET ELEMENT DATA                             
*                                                                               
DR010    CLI   0(R3),0             TEST END OF RECORD                           
         BE    DISRECX                                                          
         CLI   0(R3),SALANELQ                                                   
         BE    DR020                                                            
         CLI   0(R3),SALASELQ                                                   
         BE    DR030                                                            
DR012    SR    R0,R0               BUMP TO NEXT ELEMENT                         
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     DR010                                                            
*                                                                               
         USING SALAND,R3                                                        
DR020    OI    LAGLANH+FHOID,FHOITR                                             
         MVI   LAGLAN,C' '         CLEAR SCREEN FIELD                           
         MVC   LAGLAN+1(L'LAGLAN-1),LAGLAN                                      
         LA    RE,SALANLNQ         RE = LENGTH OF FIXED PART                    
         XR    RF,RF                                                            
         IC    RF,SALANLN          RF = LENGTH OF WHOLE ELEMENT                 
         SR    RF,RE               RF = LENGTH OF VARIABLE PART                 
         BCTR  RF,0                -1 FOR EXECUTE                               
         LTR   RF,RF               CHECK NULL LENGTH                            
         BM    DR012                                                            
         EX    RF,*+8                                                           
         B     DR012                                                            
         MVC   LAGLAN(0),SALANNAM                                               
*                                                                               
         USING SALASD,R3                                                        
DR030    CLI   SYSTEM,0                                                         
         BE    DR012                                                            
         CLC   SALASNUM,SYSTEM                                                  
         BNE   DR012                                                            
         GOTO1 ADISLACC,APPARM,(SALASNUM,SALASALL)                              
         MVC   LAGALL,APWORK                                                    
         BAS   RE,DISPLAC                                                       
         B     DR012                                                            
*                                                                               
DISRECX  MVC   SAVSYS,SYSTEM       SAVE LAST DISPLAYED SYSTEM                   
         B     EXIT                                                             
         DROP  R3                                                               
         SPACE 1                                                                
REENTER  CLI   APACTN,ACTDEL       HANDLE DELETE REENTER PROMPT                 
         BE    *+12                                                             
         CLI   APACTN,ACTRES       HANDLE RESTORE REENTER PROMPT                
         BNE   *+8                                                              
*                                  AVOID NO DATA ENTERED SYSTEM MESSAGE         
         OI    ACSSRVH+FHOID,FHOITR+FHOIMO                                      
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY SYSTEMS SUMMARY HEADER                           *         
***********************************************************************         
         SPACE 1                                                                
DISSHD   NTR1                                                                   
         MVI   SYSNAMS,C' '                                                     
         MVC   SYSNAMS+1(L'SYSNAMS-1),SYSNAMS                                   
         MVC   SYSNAMS(7),=C'SYSTEMS'                                           
         NC    SYSNAMS+1(6),=8X'BF'                                             
         MVC   SYSNAMS+8(8),=C'ASSIGNED'                                        
         NC    SYSNAMS+9(7),=8X'BF'                                             
         LA    RE,SYSNAMS+18       SET A(NEXT ENTRY)                            
         ST    RE,ASYSNAMS                                                      
         LA    R1,SYSNAMS                                                       
         SR    RE,R1                                                            
         STC   RE,SYSNAMSL         SET LIST LENGTH                              
         LA    R3,SALADATA         GET PASSWORD RECORD ELEMENT DATA             
*                                                                               
DSHD10   CLI   0(R3),0             TEST END OF RECORD                           
         BE    DSHD100                                                          
         CLI   0(R3),SALASELQ      TEST SYSTEM ELEMENT                          
         BE    DSHD30                                                           
DSHD20   ZIC   RF,1(R3)            GET NEXT ELEMENT                             
         AR    R3,RF                                                            
         B     DSHD10                                                           
*                                                                               
         USING SASYSD,R3                                                        
DSHD30   L     R4,ASYS             SEARCH SE LIST FOR SE NUM SESYSNUM           
         L     R4,VSELIST-SYSFACD(R4)                                           
         LH    RE,0(R4)                                                         
         L     RF,2(R4)                                                         
         LA    R4,6(R4)            R3=A(SELIST ENTRY)                           
         USING SELISTD,R4                                                       
         CLC   SASYSNUM,SEOVSYS                                                 
         BE    DSHD40                                                           
         BXLE  R4,RE,*-10                                                       
         LA    R4,=CL7'XXX    '    SET UNKNOWN SYSTEM NAME                      
*                                                                               
DSHD40   L     RE,ASYSNAMS         MOVE NAME TO LIST                            
         SR    R1,R1                                                            
         ICM   R1,1,SYCNT          TEST COUNT OF ITEMS IN LIST                  
         BZ    *+12                                                             
         MVI   0(RE),C'/'                                                       
         LA    RE,1(RE)                                                         
         LA    R1,1(R1)                                                         
         STC   R1,SYCNT            BUMP ITEM COUNT                              
         LA    RF,SYSNAMS+L'SYSNAMS-3                                           
         CR    RE,RF                                                            
         BH    DSHD20                                                           
         MVC   0(3,RE),SENAME      EXTRACT SE NAME                              
         NC    1(2,RE),=8X'BF'     SET TO LOWER CASE                            
         LA    RE,3(RE)                                                         
         ST    RE,ASYSNAMS                                                      
         LA    R1,SYSNAMS                                                       
         SR    RE,R1                                                            
         STC   RE,SYSNAMSL         SET LIST LENGTH                              
         B     DSHD20                                                           
         DROP  R4                                                               
*                                                                               
DSHD100  MVI   LAGSHD,C'-'         DISPLAY LIST OF SYSTEMS                      
         MVC   LAGSHD+1(L'LAGSHD-1),LAGSHD                                      
         SR    RF,RF                                                            
         ICM   RF,1,SYSNAMSL       RF=L'SYSTEM NAMES LIST                       
         BZ    DSHD110                                                          
         LA    R1,L'LAGSHD                                                      
         SR    R1,RF                                                            
         BNP   DSHD110                                                          
         SRL   R1,1                                                             
         LA    RE,LAGSHD(R1)                                                    
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),SYSNAMS                                                  
DSHD110  OI    LAGSHDH+6,X'80'                                                  
*                                                                               
DSHDX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DELETE AN LIMIT ACCESS GROUP RECORD                      *         
***********************************************************************         
         SPACE 1                                                                
DELREC   L     R2,AIOAREA1                                                      
         GOTO1 ASETACT,SALAREC                                                  
         OI    SALASTAT,X'80'      SET DELETE FLAG IN RECORD                    
         GOTO1 AIO,IOPUT+IOCONFIL+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
DELRECX  B     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO RESTORE A DELETED LIMIT ACCESS GROUP RECORD              *         
***********************************************************************         
         SPACE 1                                                                
RESREC   L     R2,AIOAREA1                                                      
         GOTO1 ASETACT,SALAREC                                                  
         NI    SALASTAT,FF-X'80'   UNSET DELETE                                 
         GOTO1 AIO,IOWRITE+IOCONFIL+IO1                                         
         BE    *+6                                                              
         DC    H'0'                                                             
RESRECX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO HANDLE FIRST FOR SCREEN LIST (SET SCREEN TO MODIFIED)    *         
***********************************************************************         
         SPACE 1                                                                
FSTLST   OI    ACSSRVH+FHOID,FHOIMO                                             
         B     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO VALIDATE SELECT PARAMETERS & INIT VALUES FOR LIST PROCESS*         
***********************************************************************         
VALSEL   LA    R2,APRECKEY         SET UP RECORD KEY                            
         OI    ACSSRVH+FHOID,FHOIMO                                             
         XC    SALAKEY,SALAKEY                                                  
         MVC   SALATYP(RTYPL),RTYP TYPE                                         
         MVC   SALAAGY,AGENCYID    AGENCY FROM LOGON                            
         CLC   OPTAGY,=AL2(0)      AGENCY OPTION OVERRIDES                      
         BE    *+10                                                             
         MVC   SALAAGY,OPTAGY                                                   
         XC    SELOPT(SELOPTL),SELOPT                                           
*                                                                               
VSLAG    GOTO1 AFVAL,LSTLACH       GET GROUP FILTER                             
         BNE   VSLAGX                                                           
         ZIC   R1,FVILEN                                                        
         L     RE,=F'-1'                                                        
         LA    RF,FVIFLD                                                        
VSLAG1   CLI   0(RF),C'A'          FIND LENGTH TO 1ST SP CHAR                   
         BL    VSLAG2              FOR KEY COMPARE IN GETSEL                    
         LA    RE,1(,RE)                                                        
         LA    RF,1(,RF)                                                        
         BCT   R1,VSLAG1                                                        
VSLAG2   STC   RE,SELKEYCL                                                      
         MVC   SELLAG,FVIFLD                                                    
         MVC   SELLAGL,FVILEN                                                   
         MVC   SELLAGSP,0(RF)                                                   
VSLAGX   EQU   *                                                                
         MVC   SALAAGR,SELLAG                                                   
*        SPACE 1                                                                
         LA    R0,LSTACTH          SET INIT VALUES                              
         ST    R0,APPARM           A(FIRST LIST LINE)                           
         MVC   APPARM+6(L'LSTLN),LSTLN   LENGTH OF LIST LINE                    
         MVI   APPARM+4,(LSTFOOTH-LSTACTH)/(LSTACT2H-LSTACTH)                   
         MVC   FVMSGNO,=AL2(FVFOK) MESSAGE NO                                   
VALSELX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GET NEXT LIST/SEL RECORD                                            *         
***********************************************************************         
GETSEL   L     R2,AIOAREA1                                                      
         MVC   IOKEY(L'SALAKEY),APRECKEY                                        
         SPACE 1                                                                
         TM    APINDS,APILNSEQ     IS IT THE START OF A NEW SCREEN?             
         BO    GETSELS             NO -> READ SEQ                               
         GOTO1 AIO,IOCONFIL+IOHI+IO1  READ HIGH TO START OFF                    
         BNE   GETSELN                                                          
         B     GETSELF                                                          
         SPACE 1                                                                
         TM    APINDS,APILRERD     HAS SEQUENTIAL READ BEEN BROKEN?             
         BZ    GETSELS             NO -> READ SEQ                               
         GOTO1 AIO,IOCONFIL+IORD+IO1  READ EQUAL TO RESTART                     
         BNE   GETSELN             & DROP THRU INTO READ SEQ                    
         SPACE 1                                                                
GETSELS  EQU   *                   READ SEQUENTIAL                              
         GOTO1 AIO,IOCONFIL+IOSQ+IO1                                            
         BNE   GETSELN                                                          
         SPACE 1                                                                
GETSELF  EQU   *                   CHECK THE RECORD IS OK                       
         CLI   SALATYP,SALATYPQ    CORRECT TYPE?                                
         BNE   GETSELN                                                          
         CLI   SALASUB,SALASUBQ    CORRECT SUB TYPE?                            
         BNE   GETSELN                                                          
         CLC   OPTAGY,=AL2(0)      AGENCY OPTION OVERRIDE                       
         BE    *+18                                                             
         CLC   SALAAGY,OPTAGY      OPTION AGENCY                                
         BNE   GETSELN                                                          
         B     *+14                                                             
         CLC   SALAAGY,AGENCYID    LOGON AGENCY                                 
         BNE   GETSELN                                                          
*                                                                               
GSLAG    CLI   SELLAGSP,C' '       GROUP CODE - FILTER ONLY IF IT               
         BNH   GSLAGX                CONTAINS SPECIAL (WILD) CHARS.             
         XR    R1,R1                                                            
         ICM   R1,1,SELKEYCL                                                    
         BM    GSLAG1                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SALAAGR(0),SELLAG                                                
         BH    GETSELN             (NO MORE RELEVENT RECORDS)                   
GSLAG1   GOTO1 ATXTFLT,APPARM,(SELLAGL,SELLAG),(L'SALAAGR,SALAAGR)              
         BNE   GETSELS             READ NEXT RECORD                             
GSLAGX   EQU   *                                                                
*                                                                               
         MVC   APRECKEY(L'SALAKEY),SALAKEY                                      
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     GETSELX                                                          
GETSELN  MVI   APMODE,APMEOFS      NO MORE RECORDS                              
         SPACE 1                                                                
GETSELX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY LIST/SELECT LINE                                 *         
***********************************************************************         
DISSEL   L     R2,AIOAREA1                                                      
         L     R4,APPARM           APPARM=A(LINE TO BE FILLED)                  
         USING LSTACTH,R4                                                       
         SPACE 1                                                                
         MVC   LSTGCDE,SALAAGR     FILL IN ACCESS GROUP CODE                    
         SPACE 1                                                                
         LA    R3,SALADATA         FIND THE GROUP NAME ELEMENT                  
         USING SALAND,R3           R3=A(GRP NAME ELEMENT)                       
         XR    RF,RF                                                            
         CLI   SALANEL,SALANELQ                                                 
         BE    *+12                                                             
         IC    RF,SALANLN                                                       
         BXH   R3,RF,*-12                                                       
         SPACE 1                                                                
         MVI   LSTGNME,C' '        CLEAR NAME AND FILL IT IN                    
         MVC   LSTGNME+1(L'LSTGNME-1),LSTGNME                                   
         CLI   SALANLN,SALANLNQ    IF IT EXISTS                                 
         BE    DISSELP                                                          
         SPACE 1                                                                
         XR    RF,RF                                                            
         IC    RF,SALANLN          RF=TOTAL LENGTH                              
         LA    RE,SALANLNQ+1       RE=L'FIXED PART (+1 SO THAT...               
         SR    RF,RE               RF=L'NAME-1 FOR THE EXECUTE)                 
         LTR   RF,RF               CHECK NULL LENGTH                            
         BM    DISSELP                                                          
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   LSTGNME(0),SALANNAM                                              
         SPACE 1                                                                
         DROP  R3                                                               
         SPACE 1                                                                
DISSELP  LA    R3,SALADATA         FIND THE PERSON COUNT                        
         USING SAPCTD,R3                                                        
         XR    RF,RF                                                            
         CLI   SAPCTEL,SAPCTELQ                                                 
         BE    *+12                                                             
         IC    RF,SAPCTLN                                                       
         BXH   R3,RF,*-12                                                       
*                                                                               
         EDIT  (B2,SAPCTVAL),(4,LSTGPCT),ZERO=NOBLANK                           
         SPACE 1                                                                
         DROP  R3                                                               
         SPACE 1                                                                
DISSELX  B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* TEST TEST TEST ROUTINE TO TRANSFER CHANGES TO LIST SELECT SCREEN    *         
* IF IT'S JUST CHANGED THE NEW RECORD IMAGE IS IN IOAREA1 SO WE CAN   *         
* LEAVE IT ALONE.                                                     *         
* IF IT'S BEEN DELETED WE SET UP IOAREA1 WITH SPOOF VALUES AND LEAVE  *         
* DISSEL TO DO ITS JOB                                                *         
***********************************************************************         
DISCHGS  L     R2,AIOAREA1         DO NOTHING IF IT'S NOT BEEN DELETED          
         TM    SALASTAT,X'80'                                                   
         BZ    DISCHGSX                                                         
         TM    APINDS,APIOKDEL     OR IF IT'S NOT ALLOWED TO BE                 
         BZ    DISCHGSX                                                         
DISCHGSD EQU   *                   REC IS DELETED SO SET UP SPOOF               
         XC    SALAKEY(SALAFRST+1),SALAKEY  INIT KEY                            
         MVC   SALAKEY,IOKEY                                                    
         LA    R0,SALAFRST+1                                                    
         STCM  R0,3,SALALEN                                                     
         SPACE 1                                                                
         XC    APELEM,APELEM                                                    
         LA    R3,APELEM           INIT NAME ELEMENT TO                         
         USING SALAND,R3           SPOOF VALUE OF '** DELETED **'               
         MVI   SALANEL,SALANELQ                                                 
         LA    RF,SALANLNQ                                                      
         LA    RF,L'SALANNAM(RF)                                                
         STC   RF,SALANLN                                                       
         MVC   SALANNAM(2),=C'**'                                               
         MVCDD SALANNAM+3(8),CT#DELD                                            
         MVC   SALANNAM+12(2),=C'**'                                            
         GOTO1 AADDELS,SALAREC                                                  
         SPACE 1                                                                
         DROP  R3                                                               
         SPACE 1                                                                
         XC    APELEM,APELEM                                                    
         LA    R3,APELEM           INIT PERSON COUNT ELEMENT TO                 
         USING SAPCTD,R3           SPOOF VALUE OF ZERO                          
         MVI   SAPCTEL,SAPCTELQ                                                 
         MVI   SAPCTLN,SAPCTLNQ                                                 
         GOTO1 AADDELS,SALAREC                                                  
         SPACE 1                                                                
         DROP  R3                                                               
         SPACE 1                                                                
DISCHGSX B     DISSEL              N.B. NOT TO EXIT                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE DELETE ACTION FROM LIST/SELECT                  *         
* ONLY ALL RIGHT IF PERSON COUNT IS 0                                 *         
***********************************************************************         
VAL2DEL  MVC   IOKEY(L'SALAKEY),APRECKEY                                        
         LA    R1,IORDD+IOCONFIL+IO1    READ DEL IN CASE SOMEONE ELSE           
         GOTO1 AIO                      DELETED IT                              
         BNL   *+6                                                              
         DC    H'0'                                                             
         L     R2,AIOAREA1                                                      
*                                                                               
         TM    IOERR,IOEDEL        CAN'T DELETE IF ALREADY DELETED              
         BO    VAL2DELN                                                         
*                                                                               
         LA    R3,SALADATA         FIND THE PERSON COUNT                        
         USING SAPCTD,R3                                                        
         XR    RF,RF                                                            
         CLI   SAPCTEL,SAPCTELQ                                                 
         BE    *+12                                                             
         IC    RF,SAPCTLN                                                       
         BXH   R3,RF,*-12                                                       
*                                                                               
         CLC   SAPCTVAL,=AL2(0)    ONLY OK IF NO PEOPLE ATTACHED                
         BNE   VAL2DELN                                                         
*                                                                               
         MVC   FVMSGNO,=AL2(FVFOK) GOT THIS FAR SO IT MUST BE OK                
         B     VAL2DELX                                                         
*                                                                               
VAL2DELN EQU   *                   IT'S NOT OK                                  
         MVC   FVMSGNO,=AL2(FVFXDEL)  CAN'T DELETE MESSAGE                      
VAL2DELX B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO HANDLE LAST FOR SCREEN (ENABLE PFKEYS)                   *         
***********************************************************************         
LSTSCR   MVI   APMODE,APMPFKS                                                   
LSTSCRX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE VALIDATE REPORT REQUEST SCREEN                              *         
***********************************************************************         
VALREQ   L     R9,AREP                                                          
         USING REPD,R9                                                          
         XC    APRECKEY,APRECKEY                                                
         SPACE 1                                                                
         MVI   FVMINL,1                                                         
         MVI   FVMAXL,L'REPREQ                                                  
         GOTO1 AFVAL,REPREQH       VALIDATE REQUESTOR                           
         BNE   VALREQX                                                          
         MVC   INUSER,FVIFLD       PUT VALUE IN THIS TIME INPUTS                
         SPACE 1                                                                
         GOTO1 AVALWHEN,REPWHENH   VALIDATE WHEN (NOW SOON O/NIGHT)             
         BNE   VALREQX                                                          
         SPACE 1                                                                
         GOTO1 AVALDEST,REPDESTH   VALIDATE DEST                                
         BNE   VALREQX                                                          
         SPACE 1                                                                
         GOTO1 AVALOTYP,REPOTYPH   VALIDATE OUTPUT TYPE                         
         BNE   VALREQX                                                          
         SPACE 1                                                                
         LA    R2,APRECKEY         SET INITIAL KEY VALUE                        
         MVC   SALATYP(RTYPL),RTYP                                              
         MVC   SALAAGY,AGENCYID                                                 
         CLC   OPTAGY,=AL2(0)      AGENCY OPTION OVERRIDE                       
         BE    *+10                                                             
         MVC   SALAAGY,OPTAGY                                                   
         MVI   SALAAGR,C' '        SPACES IN GROUP CODE SO WE...                
         MVC   SALAAGR+1(L'SALAAGR-1),SALAAGR   ...DONT READ COUNT REC          
         SPACE 1                                                                
         MVCDD REPDESC,CT#AGRL     INITIAL REPORT STUFF                         
         GOTO1 VDICTAT,APPARM,C'SL  ',REPDESC                                   
         MVI   REPHEADI,REPHSPAC+REPHCLRA                                       
         MVI   REPMIDSI,REPMSPAC+REPMCLRA                                       
         MVI   REPFOOTN,0                                                       
         LA    R0,REPSPEC                                                       
         ST    R0,REPAPHS                                                       
         OI    REPIND2,REPILOW                                                  
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         SPACE 1                                                                
VALREQX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PRINT ACCESS GROUP RECORDS                               *         
***********************************************************************         
PRTREP   L     R9,AREP                                                          
         LA    R2,IOKEY            SET UP INITIAL KEY                           
         XC    SALAKEY,SALAKEY                                                  
         MVC   SALATYP(RTYPL),RTYP                                              
         MVC   SALAAGY,AGENCYID                                                 
         CLC   OPTAGY,=AL2(0)      AGENCY OPTION OVERRIDE                       
         BE    *+10                                                             
         MVC   SALAAGY,OPTAGY                                                   
         MVI   SALAAGR,C' '        SPACES IN AGR SO DONT READ COUNT REC         
         MVC   SALAAGR+1(L'SALAAGR-1),SALAAGR                                   
         LA    R1,IOHI+IOCONFIL+IO1                                             
         GOTO1 AIO                 READ FIRST                                   
         LA    R2,IOAREA1          HANG DSECT ON RECORD                         
*                                                                               
PREP100  BNE   PRTREPX                                                          
         CLC   SALATYP(RTYPL),RTYP CORRECT TYPE?                                
         BNE   PRTREPX                                                          
         CLC   OPTAGY,=AL2(0)      AGENCY OPTION OVERRIDE                       
         BE    PREP110                                                          
         CLC   SALAAGY,OPTAGY      AGENCY FROM OPTION                           
         BNE   PRTREPX                                                          
         B     PREP120                                                          
PREP110  CLC   SALAAGY,AGENCYID    AGENCY FROM LOGON                            
         BNE   PRTREPX                                                          
*                                                                               
PREP120  MVC   LINEGCDE,SALAAGR    FILL IN ACCESS GROUP CODE                    
         LA    R3,SALADATA         FIND THE GROUP NAME ELEMENT                  
         USING SALAND,R3           R3=A(GRP NAME ELEMENT)                       
         XR    RF,RF                                                            
         CLI   SALANEL,SALANELQ                                                 
         BE    *+12                                                             
         IC    RF,SALANLN                                                       
         BXH   R3,RF,*-12                                                       
*                                                                               
         MVI   LINEGNME,C' '       CLEAR NAME AND FILL IT IN                    
         MVC   LINEGNME+1(L'LINEGNME-1),LINEGNME                                
         CLI   SALANLN,SALANLNQ    IF IT EXISTS                                 
         BE    PREP130                                                          
         XR    RF,RF                                                            
         IC    RF,SALANLN          RF=TOTAL LENGTH                              
         LA    RE,SALANLNQ+1       RE=L'FIXED PART (+1 SO THAT...               
         SR    RF,RE               RF=L'NAME-1 FOR THE EXECUTE)                 
         LTR   RF,RF               CHECK NULL LENGTH                            
         BM    PREP130                                                          
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   LINEGNME(0),SALANNAM                                             
         DROP  R3                                                               
*                                                                               
PREP130  LA    R3,SALADATA         FIND THE PERSON COUNT                        
         USING SAPCTD,R3                                                        
         XR    RF,RF                                                            
         CLI   SAPCTEL,SAPCTELQ                                                 
         BE    PREP140                                                          
         IC    RF,SAPCTLN                                                       
         BXH   R3,RF,*-12                                                       
*                                          EDIT INTO LINE                       
PREP140  EDIT  (B2,SAPCTVAL),(4,LINEPCT),ZERO=NOBLANK                           
         DROP  R3                                                               
*                                                                               
         LA    R3,SALADATA         GET SYSTEM ELEMENTS                          
PREP150  CLI   0(R3),0             TEST END OF RECORD                           
         BE    PREP200                                                          
         CLI   0(R3),SALASELQ                                                   
         BE    PREP170                                                          
PREP160  SR    R0,R0               BUMP TO NEXT ELEMENT                         
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     PREP150                                                          
*                                                                               
         USING SALASD,R3                                                        
PREP170  EQU   *                                                                
         MVC   SYSTEM,SALASNUM                                                  
         GOTO1 ADISSYS,SYSTEM                                                   
         MVC   LINESYS,APWORK                                                   
         BAS   RE,PRTLAC                                                        
         GOTO1 VREPORT,REPD                                                     
         B     PREP160                                                          
         DROP  R3                                                               
*                                                                               
PREP200  EQU   *                                                                
         GOTO1 VREPORT,REPD        PRINT A LINE                                 
         LA    R1,IOSQ+IOCONFIL+IO1                                             
         GOTO1 AIO                 READ NEXT REC                                
         B     PREP100             BRANCH TO TOP OF LOOP                        
*                                                                               
PRTREPX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD PROGRAM NAME LIST AND CODE SAVE TABLE              *         
***********************************************************************         
         SPACE 1                                                                
INITPGML NTR1                                                                   
         BAS   RE,GETSE            GET SELIST ENTRY FOR SYSTEM                  
         L     R1,ASE                ADDRESS IN ASE                             
         L     R1,SEPGMS-SELISTD(R1)                                            
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         ST    R1,APFULL                                                        
         USING PGMLSTD,R1          R1=A(PROGRAMS LIST)                          
         LA    R4,XSORTBLK         SORT PROGRAM NAMES ALPHA                     
         SR    R8,R8               COUNT NUMBER OF PROGRAMS                     
         XC    PROGRAM,PROGRAM                                                  
*                                  SORT PROGRAM NAMES ALPHABETICALLY            
IPL010   TM    PGMIND2,PGMISECA    ONLY NEW SECURITY PROGRAMS                   
         BNO   IPL011                                                           
         MVC   L'PGMNAME(1,R4),PGMNUM                                           
         MVC   0(L'PGMNAME,R4),PGMNAME                                          
         LA    R4,L'PGMNAME+L'PGMNUM(R4)                                        
         LA    R8,1(R8)                                                         
IPL011   BXLE  R1,RE,IPL012                                                     
         B     IPL016                                                           
IPL012   MVC   PROGRAM,PGMNUM                                                   
         LR    R0,RF                                                            
         LR    RF,R1                                                            
         SR    RF,RE                                                            
         L     R3,APFULL                                                        
IPL013   CLC   PROGRAM,PGMNUM-PGMLSTD(R3)                                       
         BNE   IPL014              AVOID SYNONOMOUS PROGRAMS                    
         LR    RF,R0                                                            
         B     IPL011                                                           
IPL014   BXLE  R3,RE,IPL013                                                     
         LR    RF,R0                                                            
         B     IPL010                                                           
*                                                                               
IPL016   LA    R4,XSORTBLK                                                      
         LA    R0,L'PGMNAME                                                     
         LA    R3,L'PGMNAME+L'PGMNUM                                            
         GOTO1 VXSORT,APPARM,(X'00',(R4)),(R8),(R3),(R0),0                      
*                                                                               
         TWAXC LAGPN01H,PROT=Y                                                  
         LA    R3,PSAVTAB          POINT TO SAVE TABLE                          
         LTR   R8,R8                                                            
         BZ    IPLX                EXIT IF NO PROGRAMS IN LIST                  
         LA    R1,LAGPN01H         BUILD PROGRAM CODE SAVE TABLE                
         LR    RF,R1                 AND DISPLAY PROGRAM NAMES                  
         LA    RE,LAGTENDH         SAVE A(DISPLAY TABLE END)                    
         LA    R0,3                                                             
         USING PGMLD,R1                                                         
IPL020   CR    R1,RE               DIE IF OVERFLOW                              
         BL    *+6                                                              
         DC    H'00'                                                            
         MVC   0(1,R3),L'PGMNAME(R4)  SAVE PROGRAM CODE                         
         S     R1,APRELO           SAVE DISPLAY ADDRESS                         
         ST    R1,1(R3)              IN RELOCATABLE FORM                        
         A     R1,APRELO                                                        
         LA    R3,L'PSAVTAB(R3)    BUMP SAVE TABLE POINTER                      
*                                  DISPLAY PROGRAM NAME                         
         MVC   PGMLNAM(L'PGMLNAM),0(R4)                                         
         OI    PGMLNAMH+(FVOIND-FVIHDR),FVOXMT                                  
         NI    PGMLNAMH+(FVATRB-FVIHDR),X'FF'-FVAHIGH                           
         XC    PGMLVAL,PGMLVAL                                                  
         OI    PGMLVALH+(FVOIND-FVIHDR),FVOXMT                                  
         OI    PGMLVALH+(FVATRB-FVIHDR),FVAHIGH                                 
         BCT   R0,IPL022                                                        
         LA    R1,PGMLINE(RF)                                                   
         LA    R0,3                                                             
         LR    RF,R1                                                            
         B     IPL024                                                           
IPL022   LA    R1,PGMLLEN(R1)                                                   
IPL024   LA    R4,L'PGMNAME+L'PGMNUM(R4)                                        
         BCT   R8,IPL020                                                        
*                                                                               
IPLX     XC    0(L'PSAVTAB,R3),0(R3)  MARK END OF SAVE TABLE                    
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* LOCATE SELIST ENTRY FOR SYSTEM AND SAVE AT ASE                      *         
***********************************************************************         
GETSE    NTR1                                                                   
         L     R3,ASYS                                                          
         L     R3,VSELIST-SYSFACD(R3)                                           
         LH    RE,0(R3)                                                         
         L     RF,2(R3)                                                         
         LA    R3,6(R3)                                                         
         USING SELISTD,R3                                                       
*                                                                               
         CLC   SYSTEM,SEOVSYS                                                   
         BE    *+10                                                             
         BXLE  R3,RE,*-10                                                       
         DC    H'0'                DIE IF N/F                                   
*                                                                               
         ST    R3,ASE              SAVE A(SELIST ENTRY)                         
         MVC   APGM,SEPGMS         AND A(SEPGMS)                                
         B     EXIT                                                             
         DROP  R3                                                               
*                                                                               
         EJECT                                                                  
********************************************************************            
*   DISPLAY PROGRAM ACCESS CODES                                   *            
*   R3 = SYS ELEMENT                                               *            
********************************************************************            
         USING SALASD,R3                                                        
DISPLAC  NTR1                                                                   
         LA    R4,PSAVTAB          POINT TO PROGRAM CODE SAVE TABLE             
*                                                                               
         USING PGMLD,R8                                                         
DPLA010  OC    0(L'PSAVTAB,R4),0(R4)                                            
         BZ    DPLAX               EXIT AT END OF TABLE                         
         L     R8,1(R4)                                                         
         A     R8,APRELO                                                        
         MVC   PROGRAM,0(R4)       SAVE PROGRAM CODE                            
         BAS   RE,GETPAVAL         GET ACCESS CODE FROM SYSTEM ELEMENT          
         BNE   DPLA020             NOT FOUND USE DEFAULT                        
         OI    PGMLNAMH+(FVATRB-FVIHDR),FVAHIGH                                 
         NI    PGMLVALH+(FVATRB-FVIHDR),X'FF'-FVAHIGH                           
         B     DPLA030                                                          
DPLA020  MVC   PLACVAL,SALASALL    DEFAULT TO ALL VALUE                         
         NI    PGMLNAMH+(FVATRB-FVIHDR),X'FF'-FVAHIGH                           
         OI    PGMLVALH+(FVATRB-FVIHDR),FVAHIGH                                 
*                                                                               
*                                DISPLAY ACCESS CODE VALUE IN TABLE             
DPLA030  GOTO1 ADISLACC,APPARM,(SYSTEM,PLACVAL)                                 
         MVC   PGMLVAL,APWORK                                                   
         LA    R4,L'PSAVTAB(R4)                                                 
         B     DPLA010             DO NEXT TABLE ENTRY                          
*                                                                               
DPLAX    B     EXIT                                                             
         DROP  R3                                                               
         SPACE 2                                                                
********************************************************************            
*   GET PROGRAM ACCESS CODE VALUE FROM SYSTEM ELEMENT              *            
*   ON INPUT PROGRAM 1 BYTE PROGRAM CODE, R3 POINTS TO SYSTEM ELEM.*            
*   ON OUTPUT PLACVAL LIMIT ACCESS CODE IF FOUND ELSE CC .NE.      *            
********************************************************************            
         SPACE 1                                                                
         USING SALASD,R3                                                        
GETPAVAL NTR1                                                                   
         XC    PLACVAL,PLACVAL                                                  
         LA    R1,SALASPGM         POINT TO SYSTEM ELEMENT                      
         ZIC   RE,SALASLN                                                       
*                                  FIND PROGRAM CODE IN ELEMENT                 
GPAV010  CH    RE,=Y(SALASLNQ)                                                  
         BNH   GPAVNO              END OF ELEMENT                               
         CLC   PROGRAM,0(R1)                                                    
         BE    GPAVYES             PROGRAM FOUND                                
         LA    R1,L'SALASPGM(R1)   GET NEXT PROGRAM CODE                        
         SH    RE,=Y(L'SALASPGM)                                                
         B     GPAV010                                                          
*                                                                               
GPAVNO   B     NO                  PROGRAM NOT FOUND                            
*                                                                               
GPAVYES  MVC   PLACVAL,1(R1)       SAVE ACCESS CODE VALUE                       
         B     YES                                                              
         DROP  R3                                                               
         EJECT                                                                  
********************************************************************            
*   VALIDATE PROGRAM LIMIT ACCESS CODES                            *            
*   R3 = SYS ELEMENT                                               *            
********************************************************************            
         USING SALASD,R3                                                        
VALPLAC  NTR1                                                                   
         LA    R2,PSAVTAB          POINT TO PROGRAM CODE SAVE TABLE             
         LA    R4,XSORTBLK                                                      
         SR    R8,R8                                                            
*                                                                               
VPLA010  OC    0(L'PSAVTAB,R2),0(R2)                                            
         BZ    VPLA100             END OF TABLE                                 
         L     R1,1(R2)            GET DISPLAY ADDRESS                          
         A     R1,APRELO                                                        
         LA    R1,PGMLVALH-PGMLD(R1)                                            
         XC    PLACVAL,PLACVAL                                                  
         GOTO1 AFVAL               VALIDATE ACCESS CODE FIELD                   
         BNE   VPLA012                                                          
*                                  VALIDATE CODE RETURN IN APWORK               
         GOTO1 AVALLACC,APPARM,(SYSTEM,PLACVAL)                                 
         BNE   VPLANO                                                           
*                                                                               
VPLA012  CLC   SVLASALL,PLACVAL    CHECK IF SAME AS OLD DEFAULT                 
         BE    VPLA020               IF SO IGNORE                               
         CLC   SALASALL,PLACVAL    CHECK IF SAME AS NEW DEFAULT                 
         BE    VPLA020               IF SO IGNORE                               
         MVC   0(1,R4),0(R2)       ELSE ADD VALUE TO SORT BLOCK                 
         MVC   1(L'PLACVAL,R4),PLACVAL                                          
         LA    R4,L'PGMNUM+L'PLACVAL(R4)                                        
         LA    R8,1(R8)            BUMP COUNT                                   
VPLA020  LA    R2,L'PSAVTAB(R2)                                                 
         B     VPLA010             GET NEXT PROGRAM                             
*                                                                               
VPLA100  LA    R1,SALASLNQ                                                      
         LTR   R8,R8                                                            
         BZ    VPLA120             EXIT IF NO PROGRAM CODES TO SORT             
         LA    R4,XSORTBLK           ELSE SORT PROGRAM CODES                    
         LA    R0,L'PGMNUM+L'PLACVAL                                            
         LA    RF,L'PGMNUM                                                      
         GOTO1 VXSORT,APPARM,(X'00',(R4)),(R8),(R0),(RF),0                      
         LA    R1,SALASLNQ                                                      
         LA    RE,SALASPGM                                                      
*                                                                               
VPLA110  MVC   0(1,RE),0(R4)       BUILD PROGRAM ACCESS CODES                   
         MVC   1(L'PLACVAL,RE),1(R4)     INTO SYSTEM ELEMENT                    
         LA    R1,L'PGMNUM+L'PLACVAL(R1)                                        
         LA    RE,L'PGMNUM+L'PLACVAL(RE)                                        
         LA    R4,L'PGMNUM+L'PLACVAL(R4)                                        
         BCT   R8,VPLA110                                                       
*                                                                               
VPLA120  STC   R1,SALASLN          SAVE NEW LENGTH OF SYSTEM ELEMENT            
         B     VPLAYES                                                          
*                                                                               
VPLANO   B     NO                                                               
*                                                                               
VPLAYES  B     YES                                                              
         DROP  R3                                                               
         EJECT                                                                  
********************************************************************            
*   PRINT PROGRAM LIMIT ACCESS CODES                               *            
*   R3 = A(SYS ELEMENT)                                            *            
*   R9 = A(REPD)                                                   *            
********************************************************************            
         USING SALASD,R3                                                        
         USING REPD,R9                                                          
PRTLAC   NTR1                                                                   
         BAS   RE,GETSE            GET SELIST ENTRY FOR SYSTEM                  
         L     R1,ASE                ADDRESS IN ASE                             
         L     R1,SEPGMS-SELISTD(R1)                                            
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         ST    R1,APFULL                                                        
         USING PGMLSTD,R1          R1=A(PROGRAMS LIST)                          
         LA    R4,XSORTBLK         SORT PROGRAM NAMES ALPHA                     
         SR    R8,R8               COUNT NUMBER OF PROGRAMS                     
         XC    PROGRAM,PROGRAM                                                  
*                                  SORT PROGRAM NAMES ALPHABETICALLY            
PLAC010  TM    PGMIND2,PGMISECA    ONLY NEW SECURITY PROGRAMS                   
* !!     BNO   PLAC011                                                          
         MVC   L'PGMNAME(1,R4),PGMNUM                                           
         MVC   0(L'PGMNAME,R4),PGMNAME                                          
         LA    R4,L'PGMNAME+L'PGMNUM(R4)                                        
         LA    R8,1(R8)                                                         
PLAC011  BXLE  R1,RE,PLAC012                                                    
         B     PLAC016                                                          
PLAC012  MVC   PROGRAM,PGMNUM                                                   
         LR    R0,RF                                                            
         LR    RF,R1                                                            
         SR    RF,RE                                                            
         L     R2,APFULL                                                        
PLAC013  CLC   PROGRAM,PGMNUM-PGMLSTD(R2)                                       
         BNE   PLAC014             AVOID SYNONOMOUS PROGRAMS                    
         LR    RF,R0                                                            
         B     PLAC011                                                          
PLAC014  BXLE  R2,RE,PLAC013                                                    
         LR    RF,R0                                                            
         B     PLAC010                                                          
*                                                                               
PLAC016  LA    R4,XSORTBLK                                                      
         LA    R0,L'PGMNAME                                                     
         LA    RF,L'PGMNAME+L'PGMNUM                                            
         GOTO1 VXSORT,APPARM,(X'00',(R4)),(R8),(RF),(R0),0                      
         LTR   R8,R8                                                            
         BZ    PLACX               EXIT IF NO PROGRAMS IN LIST                  
         LA    R2,LINELACS                                                      
*                                  PRINT LIST OF PROGRAM                        
*                                    LIMIT ACCESS CODES                         
PLAC100  MVC   PROGRAM,L'PGMNAME(R4)                                            
         GOTO1 ADISPGM,APPARM,(SYSTEM,PROGRAM)                                  
         ICM   R1,15,APPARM                                                     
         BNZ   PLAC110                                                          
         GOTO1 VHEXOUT,APPARM,PROGRAM,APWORK,1,=C'TOG'                          
         B     PLAC120                                                          
*                                                                               
PLAC110  MVC   0(L'PGMNAME,R2),APWORK                                           
*                                                                               
PLAC120  LA    R2,L'LINEPGMN-1(R2)                                              
         CLI   0(R2),C' '                                                       
         BNE   *+8                                                              
         BCT   R2,*-8                                                           
         MVI   1(R2),C'='                                                       
         LA    R2,2(R2)                                                         
         BAS   RE,GETPAVAL                                                      
         BE    PLAC130                                                          
         MVC   PLACVAL,SALASALL                                                 
*                                                                               
PLAC130  GOTO1 ADISLACC,APPARM,(SYSTEM,PLACVAL)                                 
         MVC   0(L'LINEPGML,R2),APWORK                                          
         LA    R2,L'LINEPGML-1(R2)                                              
         CLI   0(R2),0                                                          
         BNE   *+8                                                              
         BCT   R2,*-8                                                           
         CLI   0(R2),C' '                                                       
         BNE   *+8                                                              
         BCT   R2,*-8                                                           
         LA    RF,L'LINELAC(R2)                                                 
         LA    RE,LINEEND                                                       
         CR    RF,RE                                                            
         BNL   PLAC200                                                          
         MVI   1(R2),C' '                                                       
         LA    R2,2(R2)                                                         
         B     PLAC210                                                          
*                                                                               
PLAC200  GOTO1 VREPORT,REPD                                                     
         LA    R2,LINELACS                                                      
*                                                                               
PLAC210  LA    R4,L'PGMNAME+L'PGMNUM(R4)                                        
         BCT   R8,PLAC100                                                       
*                                                                               
PLACX    B     EXIT                                                             
         DROP  R3,R9                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PUT KEY OF RECORD TO SAVED KEY TABLE                     *         
***********************************************************************         
PUTKEY   LA    R2,APRECKEY                                                      
         LA    R3,APELEM                                                        
         SPACE 1                                                                
         MVI   0(R3),KEYLAG        FORMATTED LIKE AN ELEMENT                    
         MVI   1(R3),L'SALAAGR+2   LENGTH                                       
         MVC   2(L'SALAAGR,R3),SALAAGR                                          
         SPACE 1                                                                
         XR    R0,R0               X'00' TO MARK THE END                        
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         MVI   0(R3),0                                                          
         SPACE 1                                                                
         GOTO1 APUTKEY             CONTROLLER ROUTINE                           
         SPACE 1                                                                
PUTKEYX  B     EXIT                                                             
*                                                                               
*                                  ERROR EXITS                                  
SAEIIF   MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     NO                  INPUT FIELD ERROR                            
SAEFTL   MVC   FVMSGNO,=AL2(FVFLONG)                                            
         B     NO                  INPUT FIELD TOO LONG                         
SAEFNN   MVC   FVMSGNO,=AL2(FVFNOTN)                                            
         B     NO                  INPUT FIELD NOT NUMERIC                      
SAEFTS   MVC   FVMSGNO,=AL2(FVFSHRT)                                            
         B     NO                  INPUT FIELD TOO SHORT                        
SAEFNH   MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     NO                  INPUT FIELD ERROR                            
SAEMIF   MVC   FVMSGNO,=AL2(FVFNONE)                                            
         B     NO                  MISSING FIELD                                
SAEIIO   MVC   FVMSGNO,=AL2(FVFIOER)                                            
         B     NO                  I/O ERROR                                    
SAERNF   MVC   FVMSGNO,=AL2(FVFERNF)                                            
         B     NO                  RECORD NOT FOUND                             
SAEDIF   MVC   FVMSGNO,=AL2(FVFDUPE)                                            
         B     NO                  DUPLICATE                                    
SAERAE   MVC   FVMSGNO,=AL2(FVFERAE)                                            
         B     NO                  ALREADY EXISTS                               
SAESYS   MVC   FVMSGNO,=AL2(FVFESYS)                                            
         B     NO                  SYSTEM NAME ERROR                            
SAEFTB   MVC   FVMSGNO,=AL2(CE#FVMAX)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     NO                  FIELD VALUE EXCEEDS MAXIMUM                  
*                                                                               
YES      SR    RC,RC               RETURN CC EQUAL                              
NO       LTR   RC,RC               RETURN CC NOT EQUAL                          
         B     EXIT                                                             
         SPACE 1                                                                
SALAFRST EQU   SALADATA-SALAREC                                                 
FF       EQU   X'FF'                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
CTFILE   DC    C'CTFILE '                                                       
YAUTH    DC    X'000F'                                                          
NAUTH    DC    X'0000'                                                          
XAUTH    DC    X'FFFF'                                                          
         SPACE 1                                                                
LSTLN    DC    AL2(LSTACT2H-LSTACTH)  LIST LINE LENGTH                          
         SPACE 1                                                                
RTYP     DS    0XL4                RECORD TYPE/SUB TYPE                         
         DC    AL1(SALATYPQ,SALASUBQ)                                           
RTYPL    EQU   *-RTYP                                                           
         SPACE 1                                                                
REPSPEC  DS    0X                                                               
         SPEC  H1,1,RUN                                                         
         SPEC  H1,30,CT#LAGL,30,L                                               
         SPEC  H2,30,CT#LAGL,30,LU                                              
         SPEC  H1,100,AGYNAME                                                   
         SPEC  H2,100,AGYADD                                                    
         SPEC  H3,100,REQUESTOR                                                 
         SPEC  H3,120,PAGE                                                      
         SPEC  H4,100,REPORT                                                    
         SPEC  M1,2,CT#CODE,8,L                                                 
         SPEC  M2,2,CT#CODE,8,LU                                                
         SPEC  M1,11,CT#NAME,30,L                                               
         SPEC  M2,11,CT#NAME,30,LU                                              
         SPEC  M1,42,CT#STAFF,5,L                                               
         SPEC  M2,42,CT#STAFF,5,LU                                              
         SPEC  M1,48,CT#SYS,7,L                                                 
         SPEC  M2,48,CT#SYS,7,LU                                                
         SPEC  M1,56,CT#LIMAC,74,L                                              
         SPEC  M2,56,CT#LIMAC,74,LU                                             
         SPEC  END                                                              
         EJECT                                                                  
* SEACSWRK                                                                      
       ++INCLUDE SEACSWRK                                                       
         EJECT                                                                  
TWAD     DSECT                                                                  
         ORG   ACSTABH                                                          
       ++INCLUDE SEACSEFD                                                       
         ORG   ACSTABH                                                          
       ++INCLUDE SEACSCFD                                                       
         ORG   LSTLIN              * LIST LINE LAYOUT *                         
LSTGCDE  DS    CL8                 ACCESS GROUP CODE                            
         DS    CL2                                                              
LSTGNME  DS    CL30                ACCESS GROUP NAME                            
         DS    CL4                                                              
LSTGPCT  DS    CL4                 PERSON COUNT                                 
         SPACE 2                                                                
         ORG   ACSTABH                                                          
       ++INCLUDE SEACSAFD                                                       
*                                  WORKING STORAGE SAVED IN TWA                 
         ORG   SAVOVER                                                          
SAVSYS   DS    XL1                 SAVE CURRENT SYSTEM                          
*                                                                               
* TABLE OF SYSTEM PROGRAM CODES AND CORRESPONDING DISPLAY ADDRESSES             
* EACH ENTRY IS 1 BYTE PROGRAM CODE, 4 BYTES DISPLAY ADDRESS (-RELO)            
PSAVTAB  DS    64XL5               TABLE ENTRIES                                
*                                                                               
PGMLD    DSECT                     ** PROGRAM LIST DISPLAY DSECT **             
PGMLNAMH DS    CL8                                                              
PGMLNAM  DS    CL(L'LAGPN01)                                                    
PGMLVALH DS    CL8                                                              
PGMLVAL  DS    CL(L'LAGPV01)                                                    
PGMLLEN  EQU   *-PGMLD                                                          
PGMLINE  EQU   LAGPN04H-LAGPN01H                                                
         ORG                                                                    
         SPACE 2                                                                
REPD     DSECT                     ** PRINT LINE LAYOUT **                      
         ORG   REPP1                                                            
         DS    CL1                                                              
LINEGCDE DS    CL8                 ACCESS GROUP CODE                            
         DS    CL1                                                              
LINEGNME DS    CL30                ACCESS GROUP NAME                            
         DS    CL1                                                              
LINEPCT  DS    CL5                 PERSON COUNT                                 
         DS    CL1                                                              
LINESYS  DS    CL7                 SYSTEM                                       
         DS    CL1                                                              
LINELACS DS    0CL74               PROGRAM LIMIT ACCESS CODES                   
LINELAC  DS    0CL25                                                            
LINEPGMN DS    CL7                 PROGRAM NAME                                 
         DS    CL1                  =                                           
LINEPGML DS    CL16                LIMIT ACCESS CODE                            
         DS    CL1                  ,                                           
         ORG   LINELACS+L'LINELACS                                              
LINEEND  DS    0C                                                               
         EJECT                                                                  
WORKD    DSECT                                                                  
         ORG   APLOCAL             ** DSECT TO COVER LOCAL W/S **               
DUB      DS    D                                                                
WORK     DS    XL64                                                             
ASE      DS    A                                                                
APGM     DS    A                                                                
VXSORT   DS    A                                                                
PCOUNT   DS    XL(L'SAPCTVAL)      PERSON STAFF COUNT SAVE                      
SVLASALL DS    XL(L'SALASALL)      LIMIT ACCESS DEFAULT SAVE                    
PROGRAM  DS    C                   PROGRAM CODE                                 
PLACVAL  DS    XL(L'SALASALL)      PROGRAM LIMIT ACCESS CODE                    
MGNAME   DS    CL8                 PROGRAM NAME SAVE                            
SYSTEM   DS    C                   SYSTEM SE NUMBER SAVE                        
SYCNT    DS    CL1                                                              
ASYSNAMS DS    A                                                                
SYSNAMS  DS    CL78                                                             
SYSNAMSL DS    XL1                                                              
SYSNUMS  DS    XL1                                                              
*                                                                               
AGENCYID DS    XL2                 AGENCY ALPHA ID                              
*                                                                               
XSORTBLK DS    64XL(10)                                                         
*                                                                               
SELOPT   DS    0X                  LIST SELECT FILTERS                          
SELLAG   DS    XL(L'SALAAGR)       LIMIT ACCESS GROUP CODE FILTER               
SELLAGL  DS    XL1                 LAG FILTER LENGTH                            
SELLAGSP DS    XL1                 FIRST NON SPACE CHAR                         
SELKEYCL DS    XL1                 KEY COMPARE LENGTH                           
SELOPTL  EQU   *-SELOPT                                                         
*                                                                               
LOCALX   EQU   *                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006SEACS10   05/01/02'                                      
         END                                                                    
LOCALX   EQU   *                                                                
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
         END                                                                    
