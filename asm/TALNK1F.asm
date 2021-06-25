*          DATA SET TALNK1F    AT LEVEL 002 AS OF 05/29/15                      
*PHASE T7041FE                                                                  
TALNK1F  TITLE 'COMMERCIAL VERIFY UPLOAD SERVER'                                
         PRINT NOGEN                                                            
SVRDEF   CSECT                                                                  
         LKSVR TYPE=U,CODE=ENTRY,RLEN=6000,REQUEST=*,WORKERKEY=TACV,   +        
               LINKIO=Y,BLOCKS=(B#SAVED,SAVED),SYSTEM=TALSYSQ,IDF=Y             
ERRTAB   EQU   7500                                                             
CSTTAB   EQU   (L'RQCVSEQ*1250+1)                                               
WORKLNQ  EQU   ERRTAB+CSTTAB                                                    
                                                                                
ENTRY    NMOD1 WORKLNQ,**TA1F**,RR=RE                                           
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
         AHI   RF,ERRTAB                                                        
         ST    RF,ACSTTAB          SAVE A(CAST TABLE)                           
                                                                                
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
                                                                                
INPUT    BRAS  RE,CVUPLOAD         PROCESS THE INPUT RECORD                     
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
*        PROCESS COMMERCIAL VERIFY UPLOAD REQUEST                     *         
***********************************************************************         
                                                                                
CVUPLOAD NTR1  BASE=*,LABEL=*                                                   
         USING LIOB,R5                                                          
         L     R5,ALIOB                                                         
                                                                                
         BAS   RE,UPLCAST          IF ONLY UPLOADING CAST                       
         JE    YES                 DO SO AND GO GET NEXT REQUEST                
                                                                                
         LA    R3,IOKEY                                                         
         L     R4,AIO3                                                          
                                                                                
         MVC   FULL1,ACSTTAB                                                    
         LA    R0,SAVED            CLEAR LOCAL STORAGE                          
         LHI   R1,LOCALL                                                        
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         MVC   ACSTTAB,FULL1                                                    
                                                                                
         GOTOR (#INITERR,AINITERR),DMCB,I$EROV,0,I$CLMC                         
                                                                                
         BAS   RE,ASRTREQ          ASSERT ALL REQUIRED FIELDS PROVIDED          
         JNE   CVU140                                                           
         BAS   RE,ASRTVAL          ASSERT ALL FIELDS HAVE VALID VALUES          
         JNE   CVU140                                                           
                                                                                
         GOTOR (#VALSTF,AVALSTF),DMCB,RQCVSTF                                   
         JNE   CVU140                                                           
                                                                                
***********************************************************************         
*        CHECK COMMERCIAL                                             *         
***********************************************************************         
                                                                                
         USING TLCOPD,R3                                                        
         XC    TLCOPKEY,TLCOPKEY   ENSURE THAT COMMERCIAL EXISTS                
         MVI   TLCOPCD,TLCOCCDQ                                                 
         MVC   TLCOCCOM,RQCVCOM                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         CLC   IOKEY(L'TLCOPKEY),IOKEYSAV                                       
         JE    CVU10                                                            
         GOTOR (#ADDERR,AADDERR),DMCB,ERCVNF                                    
         J     CVU140                                                           
         DROP  R3                                                               
                                                                                
CVU10    GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOFIL+IO3'                           
                                                                                
         GOTO1 CHKWAPP,DMCB,AIO3   ENSURE WEB APPLICATION ID MATCHES            
                                                                                
         USING TACOD,R4                                                         
         MVI   ELCODE,TACOELQ                                                   
         BRAS  RE,GETEL            ENSURE THAT COMMERCIAL IS NOT                
         JE    *+6                 ALREADY VERIFIED                             
         DC    H'0'                                                             
         OC    TACOVDTE,TACOVDTE                                                
         JZ    CVU20                                                            
         GOTOR (#ADDERR,AADDERR),DMCB,ERCVAV                                    
         OI    ERRSTAT,ESREVIW                                                  
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        CHECK ALL ATTACHED VERSIONS                                  *         
***********************************************************************         
                                                                                
         USING TLVRD,R3                                                         
CVU20    XC    TLVRKEY,TLVRKEY                                                  
         MVI   TLVRCD,TLVRCDQ                                                   
         MVC   TLVRCOM,RQCVCOM                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO4'                               
         J     CVU40                                                            
CVU30    GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO4'                               
CVU40    CLC   IOKEY(TLVRVER-TLVRCD),IOKEYSAV                                   
         JNE   CVU50                                                            
         DROP  R3                                                               
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO4'                              
                                                                                
         GOTO1 CHKWAPP,DMCB,AIO4   ENSURE WEB APPLICATION ID MATCHES            
         JE    CVU30                                                            
                                                                                
***********************************************************************         
*        CHECK ALL ATTACHED CAST                                      *         
***********************************************************************         
                                                                                
         USING TLCAD,R3                                                         
CVU50    XC    TLCAKEY,TLCAKEY                                                  
         MVI   TLCACD,TLCACDQ                                                   
         MVC   TLCACOM,RQCVCOM                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO4'                               
         J     CVU70                                                            
CVU60    GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO4'                               
CVU70    CLC   IOKEY(TLCASORT-TLCACD),IOKEYSAV                                  
         JNE   CVU80                                                            
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO4'                              
                                                                                
         L     R4,AIO4                                                          
         MVI   ELCODE,TATRELQ      TEST IF MUSICIAN                             
         BRAS  RE,GETEL                                                         
         JE    CVU60                                                            
                                                                                
         GOTO1 CHKWAPP,DMCB,AIO4   ENSURE WEB APPLICATION ID MATCHES            
         JE    CVU60                                                            
                                                                                
***********************************************************************         
*        CHECK ALL ATTACHED CAST ACCORDING TO WEB APPLICATION         *         
***********************************************************************         
                                                                                
         USING FAWSSVRD,R1                                                      
CVU80    LA    R1,WSSVRBLK                                                      
         MVC   FAWSTOKN,=C'CAST'                                                
         MVI   FAWSACTN,FAWSARST   RECALL CAST TABLE VIA WSSVR                  
         XC    FAWSLEN,FAWSLEN                                                  
         MVC   FAWSADR,ACSTTAB                                                  
         GOTO1 VWSSVR,(R1)                                                      
         CLI   FAWSRTN,0                                                        
         JNE   CVU130                                                           
         DROP  R1                                                               
                                                                                
         L     R2,ACSTTAB                                                       
                                                                                
         USING TLCAD,R3                                                         
CVU90    XC    TLCAKEY,TLCAKEY                                                  
         MVI   TLCACD,TLCACDQ                                                   
         MVC   TLCACOM,RQCVCOM                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO4'                               
         J     CVU110                                                           
CVU100   GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO4'                               
CVU110   CLC   IOKEY(TLCASORT-TLCACD),IOKEYSAV                                  
         JE    CVU120                                                           
         GOTOR (#ADDERR,AADDERR),DMCB,ERCVDCST                                  
         OI    ERRSTAT,ESREVIW                                                  
         J     CVU130                                                           
                                                                                
CVU120   CLC   TLCASEQ,0(R2)                                                    
         JNE   CVU100                                                           
         DROP  R3                                                               
                                                                                
         CLI   L'RQCVSEQ(R2),X'FF'                                              
         JE    CVU130                                                           
         LA    R2,L'RQCVSEQ(R2)                                                 
         J     CVU90                                                            
                                                                                
***********************************************************************         
*        IF MODE IS EXECUTE AND NO ERRORS HAVE BEEN ENCOUNTERED       *         
*        VERIFY THE COMMERCIAL                                        *         
***********************************************************************         
                                                                                
CVU130   CLI   RQCVMOD,RQCVEXE                                                  
         JNE   CVU140                                                           
         TM    ERRSTAT,ESECTRD                                                  
         JO    CVU140                                                           
                                                                                
         USING TACOD,R4                                                         
         L     R4,AIO3                                                          
         MVI   ELCODE,TACOELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         GOTO1 VDATCON,DMCB,(5,0),(1,TACOVDTE)                                  
         MVC   TACOVSTU,LP_USRID                                                
         MVC   TACOVST,RQCVSTF                                                  
                                                                                
         TIME  DEC                                                              
         STCM  R0,14,TACOVTIM      CURRENT TIME                                 
         DROP  R4                                                               
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOFIL+IO3'                           
                                                                                
***********************************************************************         
*        SEND RESPONSE BACK TO WEB APPLICATION                        *         
***********************************************************************         
                                                                                
CVU140   MVI   OUTPUT,CVSTER       IF AN ERROR HAS BEEN ENCOUNTERED             
         TM    ERRSTAT,ESECTRD     RETURN "NOT OK" STATUS                       
         JO    CVU150                                                           
         MVI   OUTPUT,CVSTOK       ELSE RETURN "OK" STATUS                      
                                                                                
CVU150   GOTO1 VLINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTMAP',O#CVSTA)               
         GOTO1 (RF),(R1),('LIOAPUT',LIOB),('LIOTRAW',1),               +        
               ('LD_UBINQ',OUTPUT),(1,0)                                        
                                                                                
         CLI   RQCVMOD,RQCVEXE                                                  
         JNE   CVU160                                                           
         TM    ERRSTAT,ESECTRD                                                  
         JO    CVU160                                                           
         GOTO1 VLINKIO,DMCB,('LIOAPUT',LIOB),('LIOTRAW',2),            +        
               ('LD_CHARQ',RQCVSTF),(L'RQCVSTF,0)                               
         GOTO1 VDATCON,DMCB,(5,0),(8,OUTPUT)                                    
         GOTO1 VLINKIO,DMCB,('LIOAPUT',LIOB),('LIOTRAW',3),            +        
               ('LD_CHARQ',OUTPUT),(8,0)                                        
                                                                                
CVU160   GOTO1 VHEXOUT,DMCB,RQCVCOM,OUTPUT,L'RQCVCOM,0                          
         GOTO1 VLINKIO,DMCB,('LIOAPUT',LIOB),('LIOTRAW',4),            +        
               ('LD_CHARQ',OUTPUT),(8,0)                                        
                                                                                
         GOTOR (#OUTERR,AOUTERR),DMCB,O#CVERR,OUTPUT                            
                                                                                
         TM    ERRSTAT,ESREVIW                                                  
         JZ    XIT                                                              
         GOTOR VLINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTMAP',I#CVULD)               
         GOTO1 (RF),(R1),('LIOAPUT',LIOB),('LIOTRUN',I#COSDLD)                  
         GOTO1 (RF),(R1),('LIOAPUT',LIOB),('LIOTRAW',D#COSSTF),        +        
               ('LD_CHARQ',RQCVSTF),(L'RQCVSTF,0)                               
         GOTO1 (RF),(R1),('LIOAPUT',LIOB),('LIOTRAW',D#COSCOM),        +        
               ('LD_HEXDQ',RQCVCOM),(L'RQCVCOM,0)                               
         GOTO1 (RF),(R1),('LIOAPUT',LIOB),('LIOTRAW',D#COSAVR),        +        
               ('LD_CHARQ',=C'Y'),(1,0)                                         
         GOTO1 (RF),(R1),('LIOAPUT',LIOB),('LIOTERU',0),0,0                     
                                                                                
         GOTO1 (RF),(R1),('LIOAPUT',ALIOB),('LIOTRUN',I#CASDLD)                 
         GOTO1 (RF),(R1),('LIOAPUT',LIOB),('LIOTRAW',D#CASSTF),        +        
               ('LD_CHARQ',RQCVSTF),(L'RQCVSTF,0)                               
         GOTO1 (RF),(R1),('LIOAPUT',LIOB),('LIOTRAW',D#CASCOM),        +        
               ('LD_HEXDQ',RQCVCOM),(L'RQCVCOM,0)                               
         GOTO1 (RF),(R1),('LIOAPUT',LIOB),('LIOTRAW',D#CASEMU),        +        
               ('LD_CHARQ',=C'Y'),(1,0)                                         
         GOTO1 (RF),(R1),('LIOAPUT',LIOB),('LIOTERU',0),0,0                     
         J     YES                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO SAVE PAYABLE CAST INTO WEB RESPONSE DETAILS BLOCK *         
*        ON ENTRY ... R8=A(WEB RESPONSE DETAILS AREA)                 *         
***********************************************************************         
                                                                                
UPLCAST  NTR1                                                                   
         OC    RQCVSEQ,RQCVSEQ     IF UPLOADING CAST                            
         JZ    NO                                                               
                                                                                
         L     R2,ACSTTAB                                                       
                                                                                
         USING FAWSSVRD,R1                                                      
         LA    R1,WSSVRBLK                                                      
         MVC   FAWSTOKN,=C'CAST'                                                
         MVI   FAWSACTN,FAWSARST   RECALL CAST TABLE VIA WSSVR                  
         XC    FAWSLEN,FAWSLEN                                                  
         ST    R2,FAWSADR                                                       
         GOTO1 VWSSVR,(R1)                                                      
         CLI   FAWSRTN,0                                                        
         JE    UC10                                                             
         DROP  R1                                                               
                                                                                
         MVI   0(R2),X'FF'         IF NOT FOUND, INITIALIZE                     
                                                                                
UC10     CLI   0(R2),X'FF'         FIND NEXT EMPTY SLOT IN                      
         JE    UC20                CAST TABLE                                   
         LA    R2,L'RQCVSEQ(R2)    AND SAVE THIS PERFOMER'S                     
         J     UC10                INTO IT                                      
UC20     MVC   0(L'RQCVSEQ,R2),RQCVSEQ                                          
         MVI   L'RQCVSEQ(R2),X'FF'                                              
                                                                                
         USING FAWSSVRD,R1                                                      
         LA    R1,WSSVRBLK                                                      
         MVC   FAWSTOKN,=C'CAST'                                                
         MVI   FAWSACTN,FAWSASVE                                                
         LHI   RE,CSTTAB                                                        
         STCM  RE,3,FAWSLEN        SAVE CAST TABLE INTO WSSVR AREA              
         MVC   FAWSADR,ACSTTAB                                                  
         GOTO1 VWSSVR,(R1)                                                      
         CLI   FAWSRTN,0                                                        
         JE    YES                                                              
         DC    H'00'                                                            
         DROP  R1                                                               
                                                                                
***********************************************************************         
*        ROUTINE ASSERTS ALL REQUIRED FIELDS HAVE BEEN PROVIDED       *         
***********************************************************************         
                                                                                
ASRTREQ  NTR1                                                                   
         MVI   BYTE1,D#CVMOD                                                    
         CLI   RQCVMOD,0           ASSERT THAT MODE IS PROVIDED                 
         JE    ARMIS                                                            
                                                                                
         MVI   BYTE1,D#CVSTF                                                    
         OC    RQCVSTF,RQCVSTF     ASSERT THAT STAFF ID IS PROVIDED             
         JZ    ARMIS                                                            
                                                                                
         MVI   BYTE1,D#CVCOM                                                    
         OC    RQCVCOM,RQCVCOM     ASSERT THAT INTERNAL COMMERCIAL              
         JZ    ARMIS               NUMBER IS PROVIDED                           
                                                                                
         MVI   BYTE1,D#CVWID                                                    
         OC    RQCVWID,RQCVWID     ASSERT THAT WEB APPLICATION ID               
         JZ    ARMIS               IS PROVIDED                                  
         J     YES                                                              
                                                                                
ARMIS    GOTOR (#ADDGERR,AADDGERR),DMCB,('EENMIS',0),(BYTE1,0)                  
         J     NO                                                               
                                                                                
***********************************************************************         
*        ROUTINE ASSERTS ALL PROVIDED VALUES ARE VALID                *         
***********************************************************************         
                                                                                
ASRTVAL  NTR1                                                                   
         MVI   BYTE1,D#CVMOD       VALIDATE MODE                                
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFMODE',RQCVMOD)                        
         JNE   AVINV                                                            
                                                                                
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFWID',RQCVWID)                         
         JE    YES                                                              
         MVI   BYTE1,D#CVWID                                                    
         J     AVINV                                                            
                                                                                
AVINV    GOTOR (#ADDGERR,AADDGERR),DMCB,('EENINV',0),(BYTE1,0)                  
         J     NO                                                               
                                                                                
***********************************************************************         
*        ROUTINE CHECKS THAT REQUEST'S WEB APPLICATION ID MATCHES     *         
*        THE PROVIDED RECORD'S WEB APPLICATION ID                     *         
*        ON ENTRY ... P1 = A(RECORD TO CHECK)                         *         
***********************************************************************         
                                                                                
CHKWAPP  NTR1                                                                   
         TM    PROSTAT,PSLUENC                                                  
         JO    NO                                                               
                                                                                
         L     R4,0(R1)                                                         
         GOTOR (#GETELEM,AGETELEM),DMCB,('TAFNELQ',(R4)),('TAFNTWEB',0)         
         JNE   CWA10                                                            
                                                                                
         USING TAFND,R4                                                         
         L     R4,AELEM                                                         
         ZIC   R1,TAFNLEN          ENSURE THAT WEB APPLICATION ID               
         SHI   R1,4                MATCHES                                      
         EX    R1,*+8                                                           
         J     *+10                                                             
         CLC   RQCVWID(0),TAFNNAME                                              
         JE    YES                                                              
         DROP  R4                                                               
                                                                                
CWA10    GOTOR (#ADDERR,AADDERR),DMCB,ERCVLUAS                                  
         OI    ERRSTAT,ESREVIW                                                  
         OI    PROSTAT,PSLUENC                                                  
         J     NO                                                               
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR CVUPLOAD                                   *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERCVNF   DC    AL1(ECVNFX-*),AL2(1),AL1(ERRCATY3),AL1(D#CVCOM)                  
         DC    C'Commercial record is not on file'                              
ECVNFX   EQU    *                                                               
                                                                                
ERCVAV   DC    AL1(ECVAVX-*),AL2(2),AL1(ERRCATY1),AL1(D#CVCOM)                  
         DC    C'Commercial is already verified'                                
ECVAVX   EQU   *                                                                
                                                                                
ERCVLUAS DC    AL1(ECVLUASX-*),AL2(3),AL1(ERRCATY1),AL1(D#CVWID)                
         DC    C'Last update to commercial from another source'                 
ECVLUASX EQU   *                                                                
                                                                                
ERCVDCST DC    AL1(ECVDCSTX-*),AL2(4),AL1(ERRCATY1),AL1(D#CVSEQ)                
         DC    C'Cast has been deleted'                                         
ECVDCSTX EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
CVSTOK   EQU   2                   NO ERRORS - COMMERCIAL VERIFIED              
CVSTER   EQU   3                   ERRORS                                       
                                                                                
         GETEL R4,DATADISP,ELCODE                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        SVRDEF                                                       *         
***********************************************************************         
                                                                                
SVRDEF   CSECT                                                                  
                                                                                
***********************************************************************         
* REQUEST MAP - COMMERCIAL VERIFY UPLOAD                              *         
***********************************************************************         
                                                                                
CVHDR    LKMAP H,I#CVULD,NEWREC=Y                                               
F$MOD    LKMAP F,D#CVMOD,UBIN,TA#PMODE,OLEN=L'RQCVMOD,MAXLEN=1,        +        
               OUTPUT=(D,B#SAVED,RQCVMOD)                                       
F$STF    LKMAP F,D#CVSTF,CHAR,TA#STAFF,MAXLEN=L'RQCVSTF,               +        
               OUTPUT=(D,B#SAVED,RQCVSTF)                                       
F$COM    LKMAP F,D#CVCOM,HEXD,TA#COMCD,OLEN=L'RQCVCOM,MAXLEN=8,        +        
               OUTPUT=(D,B#SAVED,RQCVCOM)                                       
F$SEQ    LKMAP F,D#CVSEQ,HEXD,TA#CSTSQ,OLEN=L'RQCVSEQ,MAXLEN=4,        +        
               OUTPUT=(D,B#SAVED,RQCVSEQ)                                       
F$WID    LKMAP F,D#CVWID,CHAR,TA#WAPID,MAXLEN=L'RQCVWID,               +        
               OUTPUT=(D,B#SAVED,RQCVWID)                                       
         LKMAP E                                                                
                                                                                
         LKMAP X                                                                
         EJECT                                                                  
***********************************************************************         
*        SAVED                                                        *         
***********************************************************************         
                                                                                
SAVED    DSECT                                                                  
SVVALS   DS    0X                  ** SAVED VALUES **                           
                                                                                
OUTPUT   DS    CL250               OUTPUT BLOCK FOR LINKIO                      
                                                                                
ACSTTAB  DS    A                   A(CAST SEQUENCE NUMBER TABLE)                
                                                                                
SVTIME   DS    XL3                 SAVED TIME                                   
                                                                                
PROSTAT  DS    X                                                                
PSLUENC  EQU   X'80'               LAST UPDATE ERROR ENCOUNTERED                
LOCALL   EQU   *-SAVED                                                          
                                                                                
***********************************************************************         
*        COMMERCIAL VERIFY REQUEST MAP FIELDS                         *         
***********************************************************************         
*                                                                               
RQUPVAL  DS    6000X               SEE SVRDEF for RLEN                          
RQUPLNQ  EQU   *-RQUPVAL                                                        
         ORG   RQUPVAL                                                          
                                                                                
RQCVMOD  DS    CL1                 MODE                                         
RQCVRTV  EQU   1                   RETRIEVE                                     
RQCVVFY  EQU   2                   VERIFY                                       
RQCVEXE  EQU   3                   EXECUTE                                      
RQCVSTF  DS    CL8                 STAFF CODE                                   
RQCVCOM  DS    XL4                 INTERNAL COMMERCIAL NUMBER                   
RQCVSEQ  DS    XL2                 CAST SEQUENCE NUMBER                         
RQCVWID  DS    CL18                WEB APPLICATION ID                           
RQCVLNQ  EQU   *-RQCVMOD                                                        
                                                                                
I$EROV   DS    A                   A(ERROR OVERRIDES)                           
I$CLMC   DS    A                   A(MAP CODES TO CLEAR)                        
SAVEL    EQU   *-SAVED                                                          
                                                                                
* INCLUDED DSECTS                                                               
         PRINT OFF                                                              
       ++INCLUDE TALNKWRK                                                       
       ++INCLUDE DDLINKIOD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002TALNK1F   05/29/15'                                      
         END                                                                    
