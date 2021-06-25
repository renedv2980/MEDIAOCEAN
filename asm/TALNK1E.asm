*          DATA SET TALNK1E    AT LEVEL 001 AS OF 02/01/12                      
*PHASE T7041EA                                                                  
TALNK1E  TITLE 'NEW MEDIA/INTERNET MAINTENANCE UPLOAD SERVER'                   
         PRINT NOGEN                                                            
SVRDEF   LKSVR TYPE=U,CODE=ENTRY,RLEN=6000,REQUEST=*,WORKERKEY=TANI,   +        
               LINKIO=Y,BLOCKS=(B#SAVED,SAVED),SYSTEM=TALSYSQ,IDF=Y             
ERRTAB   EQU   7500                                                             
WORKLNQ  EQU   ERRTAB                                                           
                                                                                
ENTRY    NMOD1 WORKLNQ,**TA1E**,RR=RE                                           
         LR    RF,RC                                                            
         LR    RC,R1                                                            
         USING LP_D,RC                                                          
         L     R7,LP_ARUNP                                                      
         USING RUNPARMD,R7         R7=A(RUNPARMS)                               
         XR    R6,R6                                                            
         ICM   R6,7,RUNPARUN                                                    
         USING RUNFACSD,R6         R6=A(RUNFACS)                                
         L     R9,LP_ABLK1                                                      
         USING WORKD,R9            R9=A(GLOBAL W/S)                             
         L     R8,LP_ABLK2                                                      
         USING SAVED,R8            R8=A(SAVE W/S)                               
         L     RA,ATWA                                                          
         USING TWAD,RA             RA=A(TWA)                                    
                                                                                
         ST    RE,SRVRRELO         SAVE PROGRAM RELOCATION FACTOR               
         STM   R2,RB,LP_R2RB       SAVE REGISTERS FOR SUB-ROUTINES              
                                                                                
         ST    RF,AERRTAB          SAVE A(ERROR TABLE)                          
                                                                                
         CLI   RUNPMODE,RINIREQQ   TEST 'INITIALIZE' MODE                       
         JE    INIT                                                             
         CLI   RUNPMODE,RRUNREQQ   TEST 'RUN REQUEST' MODE                      
         JE    INPUT                                                            
         J     YES                                                              
         EJECT                                                                  
                                                                                
***********************************************************************         
*        INITIALIZE UPLOAD                                            *         
***********************************************************************         
                                                                                
INIT     LA    R0,SAVED            CLEAR SAVED STORAGE                          
         LHI   R1,SAVEL                                                         
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         L     R1,ALP              RESTORE A(LP_D)                              
         MVC   ALIOB,LP_ALIOB      EXTRACT A(LIOB) FROM LP_D                    
         J     YES                                                              
         EJECT                                                                  
                                                                                
***********************************************************************         
*        PROCESS AND UPLOAD RECORDS                                   *         
***********************************************************************         
                                                                                
INPUT    BRAS  RE,NIUPLOAD         PROCESS THE INPUT RECORD                     
         J     YES                 EXIT BACK TO DDLINK                          
                                                                                
***********************************************************************         
*        EXITS                                                        *         
***********************************************************************         
                                                                                
YES      LHI   RE,1                                                             
         J     *+8                                                              
NO       LHI   RE,0                                                             
         CHI   RE,1                                                             
XIT      XIT1                                                                   
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        PROCESS NEW MEDIA/INTERNET MAINTENANCE UPLOAD REQUEST        *         
***********************************************************************         
                                                                                
NIUPLOAD NTR1  BASE=*,LABEL=*                                                   
         USING LIOB,R5                                                          
         L     R5,ALIOB                                                         
         GOTO1 VLINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTMAP',I#NIULD)               
                                                                                
         BRAS  RE,ASRTREQ          ASSERT ALL REQUIRED FIELDS PROVIDED          
                                                                                
         LA    R0,SAVED            CLEAR LOCAL STORAGE                          
         LHI   R1,LOCALL                                                        
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         GOTOR (#INITERR,AINITERR),DMCB,I$EROV,0,I$CLMC                         
                                                                                
         GOTOR (#VALSTF,AVALSTF),DMCB,RQNISTF                                   
         JNE   NIUP20                                                           
                                                                                
         LA    R3,IOKEY            R3=A(KEY)                                    
         L     R4,AIO3             R4=A(I/O AREA)                               
                                                                                
         USING TLMDD,R3                                                         
         XC    TLMDKEY,TLMDKEY                                                  
         MVI   TLMDCD,TLMDCDQ                                                   
         MVI   TLMDTYPE,INTERNET                                                
         CLI   RQNIMED,C'I'                                                     
         JE    *+8                                                              
         MVI   TLMDTYPE,NEWMEDIA                                                
         MVC   TLMDCODE,RQNICOD                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JE    NIUP10                                                           
         DROP  R3                                                               
                                                                                
         BRAS  RE,INITADD          IF NOT FOUND, PREPARE TO ADD                 
         J     NIUP20                                                           
                                                                                
NIUP10   BRAS  RE,INITCHA          AND PREPARE TO CHANGE                        
                                                                                
NIUP20   MVI   OUTPUT,NISTER       IF AN ERROR HAS BEEN ENCOUNTERED             
         TM    ERRSTAT,ESECTRD     RETURN "NOT OK" STATUS                       
         JO    NIUP30                                                           
         MVI   OUTPUT,NISTOK1      ELSE RETURN "OK" STATUS                      
         CLI   ACTION,ACTADD                                                    
         JE    NIUP30                                                           
         MVI   OUTPUT,NISTOK2                                                   
                                                                                
NIUP30   GOTO1 VLINKIO,DMCB,('LIOAPUT',LIOB),('LIOTRAW',1),            +        
               ('LD_UBINQ',OUTPUT),(1,0)                                        
                                                                                
         CLI   RQNIMOD,RQNIEXE     IF MODE IS EXECUTE                           
         JNE   NIUP40                                                           
         TM    ERRSTAT,ESECTRD     AND NO ERRORS HAVE BEEN ENCOUNTERED          
         JO    NIUP40                                                           
         BRAS  RE,EXECADD          ADD                                          
         BRAS  RE,EXECCHA          OR CHANGE NEW MEDIA/INTERNET RECORD          
         BRAS  RE,ADDWTR           AND ADD WEB TRANSACTION RECORD               
                                                                                
         GOTO1 VLINKIO,DMCB,('LIOAPUT',LIOB),('LIOTRAW',2),            +        
               ('LD_CHARQ',RQNISTF),(L'RQNISTF,0)                               
         GOTO1 VDATCON,DMCB,(5,0),(8,OUTPUT)                                    
         GOTO1 VLINKIO,DMCB,('LIOAPUT',LIOB),('LIOTRAW',3),            +        
               ('LD_CHARQ',OUTPUT),(8,0)                                        
NIUP40   GOTO1 (RF),(R1),('LIOAPUT',LIOB),('LIOTRAW',4),               +        
               ('LD_CHARQ',RQNIMED),(L'RQNIMED,0)                               
         GOTO1 (RF),(R1),('LIOAPUT',LIOB),('LIOTRAW',5),               +        
               ('LD_CHARQ',RQNICOD),(L'RQNICOD,0)                               
                                                                                
         GOTOR (#OUTERR,AOUTERR),DMCB,O#NIERR,OUTPUT                            
                                                                                
         TM    ERRSTAT,ESREVIW     IF CHANGE REQUIRES REVIEW                    
         JZ    YES                 SEND DOWN NEW MEDIA/INTERNET RECORD          
         GOTOR VLINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTMAP',I#NIULD)               
         GOTOR (RF),(R1),('LIOAPUT',LIOB),('LIOTRUN',I#NISDLD)                  
         GOTOR (RF),(R1),('LIOAPUT',LIOB),('LIOTRAW',D#NISSTF),        +        
               ('LD_CHARQ',RQNISTF),(L'RQNISTF,0)                               
         GOTOR (RF),(R1),('LIOAPUT',LIOB),('LIOTRAW',D#NISMED),        +        
               ('LD_CHARQ',RQNIMED),(L'RQNIMED,0)                               
         GOTOR (RF),(R1),('LIOAPUT',LIOB),('LIOTRAW',D#NISCOD),        +        
               ('LD_CHARQ',RQNICOD),(L'RQNICOD,0)                               
         GOTOR (RF),(R1),('LIOAPUT',LIOB),('LIOTERU',0),0,0                     
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
NISTOK1  EQU   1                   NO ERRORS - NEW MEDIA/INTERNET ADDED         
NISTOK2  EQU   2                   NO ERRORS - NEW MEDIA/INTERENT CHGED         
NISTER   EQU   3                   ERRORS                                       
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE ASSERTS ALL REQUIRED FIELDS HAVE BEEN PROVIDED       *         
***********************************************************************         
                                                                                
ASRTREQ  NTR1  BASE=*,LABEL=*                                                   
         OC    RQNISTF,RQNISTF     ASSERT THAT STAFF ID IS PROVIDED             
         JNZ   *+6                                                              
         DC    H'00'                                                            
         OC    RQNIMED,RQNIMED     ASSERT THAT MEDIA IS PROVIDED                
         JNZ   *+6                                                              
         DC    H'00'                                                            
         OC    RQNICOD,RQNICOD     ASSERT THAT CODE IS PROVIDED                 
         JNZ   *+6                                                              
         DC    H'00'                                                            
         OC    RQNINAM,RQNINAM     ASSERT THAT NAME IS PROVIDED                 
         JNZ   *+6                                                              
         DC    H'00'                                                            
         OC    RQNIWID,RQNIWID     ASSERT THAT WEB APPLICATION ID               
         JNZ   XIT                 IS PROVIDED                                  
         DC    H'00'                                                            
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        INITIAL PREPARATION FOR ADDITION OF NEW MEDIA/INTERNET       *         
*        RECORD                                                       *         
***********************************************************************         
                                                                                
INITADD  NTR1  BASE=*,LABEL=*                                                   
         MVI   ACTION,ACTADD                                                    
         GOTOR (#ADDERR,AADDERR),DMCB,ERNINTFD                                  
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR INITADD                                    *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERNINTFD DC    AL1(ENINTFDX-*),AL2(1),AL1(ERRCATY2),AL1(D#NICOD)                
         DC    C'Code is not on file'                                           
ENINTFDX EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        INITIAL PREPARATION FOR CHANGE OF NEW MEDIA/INTERNET RECORD  *         
*        ON ENTRY ... R4=A(NEW MEDIA/INTERNET RECORD)                 *         
***********************************************************************         
                                                                                
INITCHA  NTR1  BASE=*,LABEL=*                                                   
         MVI   ACTION,ACTCHA       IF FOUND, SET ACTION TO CHANGE               
         GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOFIL+IO3'                           
                                                                                
         LA    R2,ELEM             R2=A(ELEM)                                   
                                                                                
         MVI   ELCODE,0                                                         
         BRAS  RE,GETEL            READ THROUGH ALL ELEMENTS                    
         J     *+8                                                              
INITC10  BRAS  RE,NEXTEL                                                        
         JNE   XIT                                                              
                                                                                
         CLI   0(R4),TAACELQ       DELETE ACTIVITY ELEMENT                      
         JNE   *+12                                                             
         MVI   0(R4),X'FF'                                                      
         J     INITC10                                                          
                                                                                
         CLI   0(R4),TAFNELQ       PROCESS EXISTING FREE FORM NAMES             
         JNE   *+12                                                             
         BRAS  RE,CPYTAFN                                                       
         J     INITC10                                                          
                                                                                
         CLI   0(R4),TANAELQ       PROCESS NAME ELEMENT                         
         JNE   INITC10                                                          
         BRAS  RE,CPYTANA                                                       
         J     INITC10                                                          
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE COPIES FREE FORM NAME ELEMENT INTO REQUEST MAP       *         
*        ON ENTRY ... R2 = A(ELEM)                                              
*                     R4 = A(FREE FORM NAME ELEMENT)                  *         
***********************************************************************         
                                                                                
         USING TAFND,R4                                                         
CPYTAFN  NTR1  BASE=*,LABEL=*                                                   
         CLI   TAFNTYPE,TAFNTWEB   IF TYPE IS WEB APPLICATION ID                
         JNE   XIT                                                              
         MVI   0(R4),X'FF'         SET TO DELETE ELEMENT                        
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE COPIES NAME ELEMENT INTO REQUEST MAP                 *         
*        ON ENTRY ... R2 = A(ELEM)                                              
*                     R4 = A(FREE FORM NAME ELEMENT)                  *         
***********************************************************************         
                                                                                
         USING TANAD,R4                                                         
CPYTANA  NTR1  BASE=*,LABEL=*                                                   
         XC    ELEM,ELEM                                                        
         ZIC   RF,TANALEN          COPY EXISTING NAME ELEMENT                   
         BCTR  RF,0                INTO ELEM                                    
         EX    RF,*+8                                                           
         J     *+10                                                             
         MVC   ELEM(0),0(R4)                                                    
                                                                                
         MVI   0(R4),X'FF'         SET TO DELETE ELEMENT                        
         DROP  R4                                                               
                                                                                
         USING TANAD,R2                                                         
         OC    TANANAME(L'RQNINAM),SPACES                                       
         GOTOR (#CHKFLD,ACHKFLD),DMCB,(L'RQNINAM,RQNINAM),TANANAME,    +        
               ERNINAM,0                                                        
         J     XIT                                                              
         DROP  R2                                                               
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR CPYTANA                                    *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERNINAM  DC    AL1(ENINAMX-*),AL2(2),AL1(ERRCATY2),AL1(D#NINAM)                 
         DC    C'Review update to Name'                                         
ENINAMX  EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE ADDS NEW MEDIA/INTERNET RECORD TO FILE               *         
*        ON ENTRY ... R3=A(IOKEY)                                     *         
*                     R4=A(NEW MEDIA/INTERNET RECORD)                 *         
***********************************************************************         
                                                                                
EXECADD  NTR1  BASE=*,LABEL=*                                                   
         CLI   ACTION,ACTADD       IF ACTION IS ADD ...                         
         JNE   XIT                                                              
                                                                                
         XC    0(255,R4),0(R4)     INITIALIZE I/O AREA                          
                                                                                
         USING TLMDD,R4                                                         
         MVC   TLMDKEY,IOKEYSAV    BUILD KEY WITH SAVED KEY                     
         MVI   TLMDLEN+1,41        AND RECORD LENGTH                            
         DROP  R4                                                               
                                                                                
         BRAS  RE,BLDREC           BUILD NEW MEDIA/INTERNET RECORD              
         GOTOR (#ADDREC,AADDREC),'IO3'                                          
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE CHANGES EXISTING NEW MEDIA/INTERNET RECORD           *         
*        ON ENTRY ... R4=A(NEW MEDIA/INTERNET RECORD)                 *         
***********************************************************************         
                                                                                
EXECCHA  NTR1  BASE=*,LABEL=*                                                   
         CLI   ACTION,ACTCHA       IF ACTION IS CHANGE                          
         JNE   XIT                 DELETE ALL MARKED ELEMENTS                   
         GOTO1 VHELLO,DMCB,(C'D',=C'TALFIL'),(X'FF',(R4)),0                     
                                                                                
         BRAS  RE,BLDREC           BUILD NEW MEDIA/INTERNET RECORD              
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOFIL+IO3'                           
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        BUILD NEW MEDIA/INTERNET RECORD                              *         
*        ON ENTRY ... R4=A(NEW MEDIA/INTERNET RECORD)                 *         
***********************************************************************         
                                                                                
BLDREC   NTR1  BASE=*,LABEL=*                                                   
         LA    R2,ELEM             R2=A(ELEMENT)                                
         BAS   RE,ADDTANA          ADD NAME ELEMENT                             
         GOTOR (#ADDWID,AADDWID),DMCB,(R4),RQNIWID                              
         GOTOR (#ADDTAAC,AADDTAAC),DMCB,(X'80',(R4)),RQNISTF,SVTIME             
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ADD NAME ELEMENT                                             *         
*        ON ENTRY ... R2=A(ELEM)                                      *         
*                     R4=A(COMMERCIAL RECORD)                         *         
***********************************************************************         
                                                                                
         USING TANAD,R2                                                         
ADDTANA  NTR1                                                                   
         XC    ELEM,ELEM                                                        
         MVI   TANAEL,TANAELQ                                                   
         MVC   TANANAME(L'RQNINAM),RQNINAM                                      
         GOTOR (#SETELEN,ASETELEN)                                              
         GOTO1 VHELLO,DMCB,(C'P',=C'TALFIL'),(R4),(R2),0                        
         J     XIT                                                              
         DROP  R2                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE ADDS WEB TRANSACTION RECORD                          *         
*        ON ENTRY ... R4 = A(I/O AREA 3)                              *         
***********************************************************************         
                                                                                
         USING TLWTD,R4                                                         
ADDWTR   NTR1  BASE=*,LABEL=*                                                   
         CLI   ACTION,ACTADD       IF ACTION IS ADD                             
         JNE   XIT                                                              
                                                                                
         XC    0(255,R4),0(R4)     INITIALIZE WEB TRANSACTION RECORD            
         MVI   TLWTCD,TLWTCDQ                                                   
                                                                                
         MVI   TLWTWBAP,TLWTWAVI   SET WEB APPLICATION                          
         CLC   =C'VC',RQNIWID                                                   
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         GOTO1 VDATCON,DMCB,(5,0),(1,TLWTDATE)                                  
         MVC   TLWTTIME,SVTIME                                                  
                                                                                
         MVI   TLWTACTN,TLWTAANM   INDICATE IF ADDING NEW MEDIA                 
         CLI   RQNIMED,C'N'                                                     
         JE    AWTR10                                                           
         MVI   TLWTACTN,TLWTAAIN   OR ADDING INTERNET                           
                                                                                
AWTR10   MVC   TLWTWBID,RQNIWID    SET WEB APPLICATION ID                       
         MVC   TLWTUNIC,RQNICOD    AND UNIQUE IDENTIFIER                        
                                                                                
         MVI   TLWTLEN+1,41        SET RECORD LENGTH                            
         DROP  R4                                                               
                                                                                
         USING TAWTD,R2                                                         
         LA    R2,ELEM                                                          
         XC    ELEM,ELEM           BUILD WEB TRANSACTION ELEMENT                
         MVI   TAWTEL,TAWTELQ                                                   
         MVI   TAWTLEN,TAWT9LNQ                                                 
         MVC   TAWTSTAF,RQNISTF                                                 
         MVC   TAWTNINM,RQNINAM                                                 
         GOTO1 VHELLO,DMCB,(C'P',=C'TALFIL'),(R4),(R2),0                        
         GOTOR (#ADDREC,AADDREC),'IO3'                                          
         J     XIT                                                              
         DROP  R2                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         GETEL R4,DATADISP,ELCODE                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        SVRDEF                                                       *         
***********************************************************************         
                                                                                
SVRDEF   CSECT                                                                  
                                                                                
***********************************************************************         
* REQUEST MAP - NEW MEDIA/INTERNET MAINTENANCE UPLOAD                 *         
***********************************************************************         
                                                                                
NIHDR    LKMAP H,I#NIULD,NEWREC=Y                                               
F$MOD    LKMAP F,D#NIMOD,UBIN,TA#PMODE,OLEN=L'RQNIMOD,MAXLEN=1,        +        
               OUTPUT=(D,B#SAVED,RQNIMOD)                                       
F$STF    LKMAP F,D#NISTF,CHAR,TA#STAFF,MAXLEN=L'RQNISTF,               +        
               OUTPUT=(D,B#SAVED,RQNISTF)                                       
F$MED    LKMAP F,D#NIMED,CHAR,TA#MEDCD,OLEN=L'RQNIMED,MAXLEN=1,        +        
               OUTPUT=(D,B#SAVED,RQNIMED)                                       
F$COD    LKMAP F,D#NICOD,CHAR,TA#CODE,OLEN=L'RQNICOD,                  +        
               OUTPUT=(D,B#SAVED,RQNICOD)                                       
F$NAM    LKMAP F,D#NINAM,CHAR,TA#NAME,MAXLEN=L'RQNINAM,                +        
               OUTPUT=(D,B#SAVED,RQNINAM)                                       
F$WID    LKMAP F,D#NIWID,CHAR,TA#WAPID,MAXLEN=L'RQNIWID,               +        
               OUTPUT=(D,B#SAVED,RQNIWID)                                       
F$EOV    LKREQ F,D#NIEOV,(I,B#SAVED,I$EROV),UBIN,LIST=F,               *        
               OLEN=2,MAXLEN=3,TEXT=TA#EOVER,COL=*                              
F$CMC    LKREQ F,D#NICMC,(I,B#SAVED,I$CLMC),UBIN,LIST=F,               *        
               OLEN=1,MAXLEN=3,TEXT=TA#CLRMC,COL=*                              
         LKMAP E                                                                
                                                                                
         LKMAP X                                                                
         EJECT                                                                  
***********************************************************************         
*        SAVED                                                        *         
***********************************************************************         
                                                                                
SAVED    DSECT                                                                  
SVVALS   DS    0X                  ** SAVED VALUES **                           
                                                                                
OUTPUT   DS    CL250               OUTPUT BLOCK FOR LINKIO                      
                                                                                
SVTIME   DS    XL3                 SAVED TIME                                   
LOCALL   EQU   *-SAVED                                                          
                                                                                
***********************************************************************         
*        COMMENT MAINTENANCE REQUEST MAP FIELDS                       *         
***********************************************************************         
*                                                                               
RQUPVAL  DS    6000X               SEE SVRDEF for RLEN                          
RQUPLNQ  EQU   *-RQUPVAL                                                        
         ORG   RQUPVAL                                                          
                                                                                
RQNIMOD  DS    CL1                 MODE                                         
RQNIRTV  EQU   1                   RETRIEVE                                     
RQNIVFY  EQU   2                   VERIFY                                       
RQNIEXE  EQU   3                   EXECUTE                                      
RQNISTF  DS    CL8                 STAFF CODE                                   
RQNIMED  DS    CL1                 MEDIA                                        
RQNICOD  DS    CL4                 CODE                                         
RQNINAM  DS    CL30                NAME                                         
RQNIWID  DS    CL18                WEB APPLICATION ID                           
RQNILNQ  EQU   *-RQNIMOD                                                        
                                                                                
I$EROV   DS    A                   A(ERROR OVERRIDES)                           
I$CLMC   DS    A                   A(MAP CODES TO CLEAR)                        
SAVEL    EQU   *-SAVED                                                          
                                                                                
* INCLUDED DSECTS                                                               
         PRINT OFF                                                              
       ++INCLUDE TALNKWRK                                                       
       ++INCLUDE DDLINKIOD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001TALNK1E   02/01/12'                                      
         END                                                                    
