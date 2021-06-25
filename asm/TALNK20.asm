*          DATA SET TALNK20    AT LEVEL 001 AS OF 08/25/13                      
*PHASE T70420A                                                                  
TALNK20  TITLE 'COMMERCIAL COMPLETIONS LOCK'                                    
         PRINT NOGEN                                                            
SVRDEF   CSECT                                                                  
         LKSVR TYPE=U,CODE=ENTRY,RLEN=6000,REQUEST=*,WORKERKEY=TACV,   +        
               LINKIO=Y,BLOCKS=(B#SAVED,SAVED),SYSTEM=TALSYSQ,IDF=Y             
ERRTAB   EQU   7500                                                             
WORKLNQ  EQU   ERRTAB                                                           
                                                                                
ENTRY    NMOD1 WORKLNQ,**TA20**,RR=RE                                           
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
                                                                                
INPUT    BRAS  RE,CCLUPLD          PROCESS THE INPUT RECORD                     
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
*        PROCESS COMMERCIAL COMPLETIONS LOCK REQUEST                  *         
***********************************************************************         
                                                                                
CCLUPLD  NTR1  BASE=*,LABEL=*                                                   
         USING LIOB,R5                                                          
         L     R5,ALIOB                                                         
                                                                                
         LA    R0,SAVED            CLEAR LOCAL STORAGE                          
         LHI   R1,LOCALL                                                        
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         GOTOR (#INITERR,AINITERR),DMCB,I$EROV,0,I$CLMC                         
                                                                                
         BRAS  RE,ASRTREQ          ASSERT ALL REQUIRED FIELDS PROVIDED          
         JNE   CCLU00                                                           
         BRAS  RE,ASRTVAL          ASSERT ALL FIELDS HAVE VALID VALUES          
         JNE   CCLU00                                                           
                                                                                
         GOTO1 VHEXOUT,DMCB,RQCCLCOM,SVINTCOM,L'RQCCLCOM,0                      
                                                                                
         MVI   OUTPUT,SUCCQ                                                     
         GOTOR (#VALSTF,AVALSTF),DMCB,RQCCLSTF                                  
         JE    *+12                                                             
CCLU00   MVI   OUTPUT,UNSUCCQ                                                   
         JNE   CCLU80                                                           
*                                                                               
* GET COMMERCIAL RECORD AND UPDATE TAFN ELEMENTS                                
*                                                                               
         USING TLCOPD,R3                                                        
         LA    R3,IOKEY                                                         
         XC    TLCOPKEY,TLCOPKEY                                                
         MVI   TLCOPCD,TLCOCCDQ                                                 
         MVC   TLCOCCOM,RQCCLCOM                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         CLC   IOKEY(L'TLCOPKEY),IOKEYSAV                                       
         JE    CCLU10                                                           
                                                                                
         MVI   OUTPUT,UNSUCCQ                                                   
         GOTOR (#ADDERR,AADDERR),DMCB,ERCVNF                                    
         J     CCLU80                                                           
                                                                                
CCLU10   CLI   RQCCLMOD,RQCCLEXE   IF MODE IS EXECUTE                           
         JNE   CCLU80                                                           
                                                                                
         MVC   SVCOMKEY,IOKEY      SAVE COMMERCIAL KEY                          
         GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOFIL+IO3'                           
                                                                                
         OC    RQCCLSEQ,RQCCLSEQ                                                
         JNZ   *+8                                                              
         BAS   RE,PROCREC          PROCESS RECORD & UPDATE TAFN ELEMS           
*                                                                               
* GET ALL VERSIONS FOR COMMERCIAL AND UPDATE TAFN ELEMENTS                      
*                                                                               
         USING TLVRD,R3                                                         
         LA    R3,IOKEY                                                         
         XC    TLVRKEY,TLVRKEY                                                  
         MVI   TLVRCD,TLVRCDQ                                                   
         MVC   TLVRCOM,RQCCLCOM                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         J     CCLU30                                                           
CCLU20   GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
CCLU30   CLC   IOKEY(TLVRVER-TLVRCD),IOKEYSAV                                   
         JNE   CCLU40                                                           
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOFIL+IO3'                           
                                                                                
         OC    RQCCLSEQ,RQCCLSEQ                                                
         JNZ   *+8                                                              
         BAS   RE,PROCREC          PROCESS RECORD & UPDATE TAFN ELEMS           
         J     CCLU20                                                           
*                                                                               
* GET ALL CAST FOR COMML AND UPDATE TAFN ELEMENTS                               
*                                                                               
         USING TLCAD,R3                                                         
CCLU40   LA    R3,IOKEY                                                         
         XC    TLCAKEY,TLCAKEY                                                  
         MVI   TLCACD,TLCACDQ                                                   
         MVC   TLCACOM,RQCCLCOM                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         J     CCLU60                                                           
CCLU50   GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
CCLU60   CLC   IOKEY(TLCASORT-TLCACD),IOKEYSAV                                  
         JNE   CCLU70                                                           
                                                                                
         OC    RQCCLSEQ,RQCCLSEQ   CAST SEQUENCE NUMBER PROVIDED?               
         JZ    *+14                                                             
         CLC   TLCASEQ,RQCCLSEQ    FOUND CAST?                                  
         JNE   CCLU50                                                           
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOFIL+IO3'                           
                                                                                
         BAS   RE,PROCREC          PROCESS RECORD & UPDATE TAFN ELEMS           
                                                                                
         OC    RQCCLSEQ,RQCCLSEQ   CAST SEQUENCE NUMBER PROVIDED?               
         JNZ   CCLU80              YES - EXIT                                   
         J     CCLU50              NO - PROCESS NEXT CAST                       
                                                                                
CCLU70   OC    RQCCLSEQ,RQCCLSEQ   CAST SEQUENCE NUMBER PROVIDED?               
         JZ    CCLU80                                                           
         MVI   OUTPUT,UNSUCCQ      YES - DIDN'T FIND IT SO ERROR                
         GOTOR (#ADDERR,AADDERR),DMCB,ERCVNF                                    
                                                                                
CCLU80   BAS   RE,SENDMAPS                                                      
         J     YES                                                              
*                                                                               
* PROCESS RECORD AND UPDATE TAFN ELEMENTS                                       
*                                                                               
PROCREC  NTR1                                                                   
         USING TAFND,R4                                                         
         L     R4,AIO3                                                          
         MVI   ELCODE,TAFNELQ                                                   
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
PREC10   BRAS  RE,NEXTEL                                                        
         JNE   PREC20                                                           
                                                                                
         CLI   TAFNTYPE,TAFNTWEB   WEB APPLICATION ID?                          
         JE    *+12                                                             
         CLI   TAFNTYPE,TAFNTOWB   OLD WEB ID?                                  
         JNE   PREC10                                                           
                                                                                
         MVI   0(R4),X'FF'         MARK FOR DELETION                            
         J     PREC10                                                           
                                                                                
PREC20   GOTO1 VHELLO,DMCB,(C'D',=C'TALFIL'),(X'FF',AIO3),0                     
         GOTO1 ADDTAFN,DMCB,TAFNTWEB,RQCCLWID  ADD TAFN ELEMENT                 
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOFIL+IO3'                           
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
PRECX    J     XIT                                                              
         DROP  R3,R4                                                            
*                                                                               
* ADD TAFN ELEMENT TO RECORD                                                    
*                                                                               
ADDTAFN  NTR1                                                                   
         ZIC   R2,3(R1)            TAFNTYPE                                     
         L     R3,4(R1)            A(WEB ID)                                    
                                                                                
         USING TAFND,R4                                                         
         XC    ELEM,ELEM                                                        
         LA    R4,ELEM                                                          
         MVI   TAFNEL,TAFNELQ                                                   
         MVI   TAFNLEN,TAFNLNQ+L'RQCCLWID                                       
         STC   R2,TAFNTYPE                                                      
         MVC   TAFNNAME(L'RQCCLWID),0(R3)   WEB ID                              
                                                                                
         GOTO1 VHELLO,DMCB,(C'P',=C'TALFIL'),AIO3,ELEM,0                        
                                                                                
ADDTAFNX J     XIT                                                              
         DROP  R4                                                               
*                                                                               
* SEND RESPONSE MAP CODES                                                       
*                                                                               
SENDMAPS NTR1                                                                   
         GOTO1 VLINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTMAP',O#CCLSTA)              
                                                                                
         GOTO1 VLINKIO,DMCB,('LIOAPUT',LIOB),('LIOTRAW',1),            +        
               ('LD_UBINQ',OUTPUT),(1,0)                                        
                                                                                
         CLI   RQCCLMOD,RQCCLEXE   IF MODE IS EXECUTE                           
         JNE   SENDM10                                                          
         TM    ERRSTAT,ESECTRD     AND ERRORS HAVE BEEN ENCOUNTERED             
         JO    SENDM10                                                          
                                                                                
         GOTO1 VLINKIO,DMCB,('LIOAPUT',LIOB),('LIOTRAW',2),            +        
               ('LD_CHARQ',RQCCLSTF),(L'RQCCLSTF,0)                             
         GOTO1 VDATCON,DMCB,(5,0),(8,OUTPUT)                                    
         GOTO1 VLINKIO,DMCB,('LIOAPUT',LIOB),('LIOTRAW',3),            +        
               ('LD_CHARQ',OUTPUT),(8,0)                                        
                                                                                
SENDM10  GOTO1 (RF),(R1),('LIOAPUT',LIOB),('LIOTRAW',4),               +        
               ('LD_CHARQ',SVINTCOM),(L'SVINTCOM,0)                             
                                                                                
         OC    RQCCLSEQ,RQCCLSEQ                                                
         JZ    SENDM20                                                          
         GOTO1 VHEXOUT,DMCB,RQCCLSEQ,OUTPUT,L'RQCCLSEQ,0                        
         GOTO1 VLINKIO,DMCB,('LIOAPUT',LIOB),('LIOTRAW',9),            +        
               ('LD_CHARQ',OUTPUT),(4,0)                                        
                                                                                
SENDM20  GOTOR (#OUTERR,AOUTERR),DMCB,O#CCLERR,OUTPUT                           
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ERROR ENTRIES                                                *         
*        (LOOK AT ERRENTD FOR DSECT)                                  *         
***********************************************************************         
                                                                                
ERCVNF   DC    AL1(ECVNFX-*),AL2(1),AL1(ERRCATY3),AL1(D#CVCOM)                  
         DC    C'Commercial record is not on file'                              
ECVNFX   EQU    *                                                               
         LTORG                                                                  
                                                                                
ERCVCNF  DC    AL1(ECVCNFX-*),AL2(1),AL1(ERRCATY3),AL1(D#CVSEQ)                 
         DC    C'Cast record is not on file'                                    
ECVCNFX  EQU    *                                                               
         LTORG                                                                  
                                                                                
***********************************************************************         
*        ROUTINE ASSERTS ALL REQUIRED FIELDS HAVE BEEN PROVIDED       *         
***********************************************************************         
                                                                                
ASRTREQ  NTR1  BASE=*,LABEL=*                                                   
         MVI   BYTE1,D#CCLMOD                                                   
         CLI   RQCCLMOD,0          ASSERT THAT MODE IS PROVIDED                 
         JE    ARMIS                                                            
                                                                                
         MVI   BYTE1,D#CCLSTF                                                   
         OC    RQCCLSTF,RQCCLSTF   ASSERT THAT STAFF ID IS PROVIDED             
         JZ    ARMIS                                                            
                                                                                
         MVI   BYTE1,D#CCLCOM                                                   
         OC    RQCCLCOM,RQCCLCOM   ASSERT THAT INTERNAL COMMERCIAL              
         JZ    ARMIS               NUMBER IS PROVIDED                           
                                                                                
         MVI   BYTE1,D#CCLWID                                                   
         OC    RQCCLWID,RQCCLWID   ASSERT THAT WEB APPLICATION ID               
         JZ    ARMIS               IS PROVIDED                                  
         J     YES                                                              
                                                                                
ARMIS    GOTOR (#ADDGERR,AADDGERR),DMCB,('EENMIS',0),(BYTE1,0)                  
         J     NO                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE ASSERTS ALL PROVIDED VALUES ARE VALID                *         
***********************************************************************         
                                                                                
ASRTVAL  NTR1  BASE=*,LABEL=*                                                   
         MVI   BYTE1,D#CCLMOD      VALIDATE MODE                                
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFMODE',RQCCLMOD)                       
         JNE   AVINV                                                            
                                                                                
         CLC   =C'VC',RQCCLWID     VALIDATE WEB APPLICATION ID                  
         JE    YES                                                              
         CLC   =C'TC',RQCCLWID                                                  
         JE    YES                                                              
         CLC   =C'RC',RQCCLWID                                                  
         JNE   AVINV                                                            
         J     YES                                                              
                                                                                
AVINV    GOTOR (#ADDGERR,AADDGERR),DMCB,('EENINV',0),(BYTE1,0)                  
         J     NO                                                               
                                                                                
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
* REQUEST MAP - COMMERCIAL VERIFY UPLOAD                              *         
***********************************************************************         
                                                                                
CVHDR    LKMAP H,I#CCLULD,NEWREC=Y                                              
F$MOD    LKMAP F,D#CVMOD,UBIN,TA#PMODE,OLEN=L'RQCCLMOD,MAXLEN=1,       +        
               OUTPUT=(D,B#SAVED,RQCCLMOD)                                      
F$STF    LKMAP F,D#CVSTF,CHAR,TA#STAFF,MAXLEN=L'RQCCLSTF,              +        
               OUTPUT=(D,B#SAVED,RQCCLSTF)                                      
F$COM    LKMAP F,D#CVCOM,HEXD,TA#COMCD,OLEN=L'RQCCLCOM,MAXLEN=8,       +        
               OUTPUT=(D,B#SAVED,RQCCLCOM)                                      
F$SEQ    LKMAP F,D#CVSEQ,HEXD,TA#CSTSQ,OLEN=L'RQCCLSEQ,MAXLEN=4,       +        
               OUTPUT=(D,B#SAVED,RQCCLSEQ)                                      
F$WID    LKMAP F,D#CVWID,CHAR,TA#WAPID,MAXLEN=L'RQCCLWID,              +        
               OUTPUT=(D,B#SAVED,RQCCLWID)                                      
         LKMAP E                                                                
                                                                                
         LKMAP X                                                                
         EJECT                                                                  
***********************************************************************         
*        SAVED                                                        *         
***********************************************************************         
                                                                                
SAVED    DSECT                                                                  
SVVALS   DS    0X                  ** SAVED VALUES **                           
SVCOMKEY DS    XL(L'TLCOPKEY)                                                   
SVINTCOM DS    XL8                                                              
                                                                                
OUTPUT   DS    CL250               OUTPUT BLOCK FOR LINKIO                      
                                                                                
SUCCQ    EQU   2                   SUCCESSFUL (COMMERCIAL CHANGED)              
UNSUCCQ  EQU   3                   UNSUCCESSFUL                                 
                                                                                
LOCALL   EQU   *-SAVED                                                          
                                                                                
***********************************************************************         
*        COMMERCIAL VERIFY REQUEST MAP FIELDS                         *         
***********************************************************************         
*                                                                               
RQUPVAL  DS    6000X               SEE SVRDEF for RLEN                          
RQUPLNQ  EQU   *-RQUPVAL                                                        
         ORG   RQUPVAL                                                          
                                                                                
RQCCLMOD DS    CL1                 MODE                                         
RQCCLRTV EQU   1                   RETRIEVE                                     
RQCCLVFY EQU   2                   VERIFY                                       
RQCCLEXE EQU   3                   EXECUTE                                      
RQCCLSTF DS    CL8                 STAFF CODE                                   
RQCCLCOM DS    XL4                 STAFF CODE                                   
RQCCLSEQ DS    XL2                 SEQUENCE NUMBER                              
RQCCLWID DS    CL18                WEB APPLICATION ID                           
RQCCLLNQ EQU   *-RQCCLMOD                                                       
                                                                                
I$EROV   DS    A                   A(ERROR OVERRIDES)                           
I$CLMC   DS    A                   A(MAP CODES TO CLEAR)                        
SAVEL    EQU   *-SAVED                                                          
                                                                                
* INCLUDED DSECTS                                                               
         PRINT OFF                                                              
       ++INCLUDE TALNKWRK                                                       
       ++INCLUDE DDLINKIOD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001TALNK20   08/25/13'                                      
         END                                                                    
