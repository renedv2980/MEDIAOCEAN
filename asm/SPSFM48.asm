*          DATA SET SPSFM48    AT LEVEL 068 AS OF 03/14/02                      
*PHASE T21748A                                                                  
*                                                                   L01         
         TITLE 'T21748  BUCH RECORDS'                                           
T21748   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21748,R7,RR=R3                                                
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         ST    R3,RELO                                                          
         OI    GENSTAT3,MULTFILS                                                
*                                                                               
         BAS   RE,OPTPFM           LIST & PFM?                                  
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,DISPREC        VALIDATE RECORD                              
         BE    DR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         CLI   MODE,SETFILE        SET FILES                                    
         BE    SF                                                               
*                                                                               
EXIT     XIT1                                                                   
                                                                                
*******************************************************************             
*                   SET FILE                                      *             
*******************************************************************             
                                                                                
SF       BAS   RE,SSV                                                           
         B     EXIT                                                             
                                                                                
*******************************************************************             
*                  VALIDATE KEY ROUTINE                           *             
*******************************************************************             
                                                                                
VK       LA    R6,MYKEY                                                         
         USING BGRKEYD,R6                                                       
*                                                                               
         BAS   RE,RSV              RESET SYSTEM VALUES                          
*                                                                               
         XC    MYKEY,MYKEY                                                      
         XC    KEY,KEY                                                          
         MVI   BGRKTYPE,BGRKTYPQ   X'0E'                                        
         MVI   BGRKSUB,BGRKSUBQ    X'04'                                        
*                                                                               
         LA    R2,BCHMEDH          MEDIA                                        
         GOTO1 VALIMED             REQUIRED FOR LIST, MAINTENANCE               
         MVC   BGRKAM,BAGYMD                                                    
*                                                                               
         CLI   ACTNUM,ACTLIST                                                   
         BNE   VK10                                                             
*                                                                               
*                                                                               
         CLI   BCLCLIH+5,0         FILTER BY CLIENT?                            
         BE    VK19                NO NEED TO FILTER BY PROD. OR EST            
*                                                                               
VK10     LA    R2,BCHCLIH          CLIENT                                       
         GOTO1 VALICLT                                                          
         MVC   BGRKCLT,BCLT                                                     
         OI    FILTFLAG,CLIENTF                                                 
         MVC   SVCLT,BCLT                                                       
*                                                                               
VK12     CLI   ACTNUM,ACTLIST                                                   
         BNE   VK15                                                             
         CLI   BCHPRDH+5,0                                                      
         BE    VK19                SKIP ESTIMATE                                
*                                                                               
VK15     LA    R2,BCHPRDH          PRODUCT                                      
         GOTO1 VALIPRD                                                          
         MVC   BGRKPRD,BPRD                                                     
         OI    FILTFLAG,PRODUCTF                                                
         MVC   SVPRD,BPRD                                                       
*                                                                               
         CLI   ACTNUM,ACTLIST                                                   
         BNE   VK17                                                             
         CLI   BCHESTH+5,0                                                      
         BE    VK19                SKIP ESTIMATE                                
*                                                                               
VK17     LA    R2,BCHESTH          ESTIMATE                                     
         GOTO1 VALIEST                                                          
         MVC   BGRKEST,BEST                                                     
         OI    FILTFLAG,ESTIMF                                                  
         MVC   SVEST,BEST                                                       
*                                                                               
VK19     CLI   ACTNUM,ACTLIST                                                   
         BNE   VK20                                                             
         CLI   BCHMKTH+5,0                                                      
         BE    VK30                MARKET?                                      
*                                                                               
VK20     LA    R2,BCHMKTH          MARKET                                       
         GOTO1 VALIMKT                                                          
         MVC   BGRKMKT,BMKT                                                     
         OI    FILTFLAG,MARKETF                                                 
         MVC   SVMKT,BMKT                                                       
*                                                                               
VK30     BAS   RE,SSV              SET SYSTEM VALUES (XSPDIR AND XSPF)          
*                                                                               
         CLI   ACTNUM,ACTLIST      DON'T NEED TO BUILD WHOLE                    
*                                  IF ACTION IS LIST                            
         BE    VKX                                                              
*                                                                               
         MVC   KEY,MYKEY                                                        
         GOTO1 HIGH                BUILD WHOLE KEY                              
         CLC   KEY(L'BGRKMAST),KEYSAVE                                          
         BE    VKX                                                              
         MVI   ERROR,NOTFOUND                                                   
         GOTO1 ERREX                                                            
*                                                                               
VKX      B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
                                                                                
********************************************************************            
*                     DISPLAY  RECORD                              *            
********************************************************************            
                                                                                
DR       DS    0H                                                               
         BAS   RE,CF               CLEAR FIELDS                                 
         L     R6,AIO                                                           
         MVI   ELCODE,BGRSTELQ     GUIDELINE STATUS ELEM                        
         BAS   RE,GETEL                                                         
         BNE   DR15                LOOK FOR MIS STATUS ELEM                     
*                                                                               
         USING BGRSTELD,R6                                                      
         OC    BGRSRDAT,BGRSRDAT                                                
         BZ    DR5                                                              
         GOTO1 DATCON,DMCB,(0,BGRSRDAT),(11,BCHRDT)                             
         OI    BCHRDTH+6,X'80'     RUN DATE                                     
*                                                                               
DR5      OC    BGRSAST,BGRSAST                                                  
         BZ    DR10                                                             
         MVC   BCHAST,BGRSAST      APP STATUS                                   
         OI    BCHASTH+6,X'80'                                                  
*                                                                               
DR10     OC    BGRSADAT,BGRSADAT                                                
         BZ    DR12                                                             
         GOTO1 DATCON,DMCB,(0,BGRSADAT),(11,BCHADT)                             
         OI    BCHADTH+6,X'80'     APP DATE                                     
*                                                                               
DR12     MVC   BCHPER,BGRSAPER     APP PERSON                                   
         OI    BCHPERH+6,X'80'                                                  
*                                                                               
DR15     L     R6,AIO                                                           
         MVI   ELCODE,BGRMSELQ     MIS STATUS ELEM                              
         BAS   RE,GETEL                                                         
         BNE   DRX                                                              
         USING BGRMSELD,R6                                                      
*                                                                               
         OC    BGRMSDAT,BGRMSDAT                                                
         BZ    DR20                                                             
         GOTO1 DATCON,DMCB,(0,BGRMSDAT),(11,BCHMRDT)                            
         OI    BCHMRDTH+6,X'80'    MIS REPORT RUN DATE                          
*                                                                               
DR20     OC    BGRMSTAT,BGRMSTAT                                                
         BZ    DRX                                                              
         GOTO1 HEXOUT,DMCB,BGRMSTAT,BCHMST,L'BGRMSTAT                           
         OI    BCHMSTH+6,X'80'     MIS STATUS                                   
*                                                                               
DRX      DS    0H                                                               
         DROP  R6                                                               
         B     EXIT                                                             
                                                                                
*********************************************************************           
*                  DISPLAY KEY                                      *           
*********************************************************************           
                                                                                
DK       LA    R4,KEY                                                           
         USING BGRKEYD,R4                                                       
*                                                                               
         GOTO1 CLUNPK,DMCB,BGRKCLT,BCHCLI                                       
         OI    BCHCLIH+6,X'80'                                                  
*                                                                               
         BAS   RE,PRUNPK           CONVERT PRODUCT TO EBCIDIC FORMAT            
         MVC   BCHPRD(L'TEMPPR),TEMPPR                                          
         OI    BCHPRDH+6,X'80'                                                  
*                                                                               
         EDIT  BGRKEST,BCHEST,ALIGN=LEFT                                        
         OI    BCHESTH+6,X'80'                                                  
*                                                                               
         GOTO1 MSUNPK,DMCB,BGRKMKT,BCHMKT                                       
         OI    BCHMKTH+6,X'80'                                                  
*                                                                               
         B     EXIT                                                             
         DROP  R4                                                               
                                                                                
*********************************************************************           
*                  FIND PRODUCT MNEMONIC                            *           
*********************************************************************           
                                                                                
PRUNPK   NTR1                                                                   
         L     R4,AIO                                                           
         USING BGRKEYD,R4                                                       
*                                                                               
         MVC   TEMPKEY,KEY                                                      
         XC    KEY,KEY                                                          
         MVC   KEY+1(L'BGRKAM),BGRKAM                                           
         MVC   KEY+2(L'BGRKCLT),BGRKCLT                                         
         BAS   RE,RSV              SET SYSTEM'S VALUES                          
         MVC   AIO,AIO3                                                         
         GOTO1 READ                                                             
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING CLTHDRD,R6                                                       
         LA    R5,CLIST                                                         
*                                                                               
PRU10    CLC   BGRKPRD,3(R5)       FIND PRODUCT                                 
         BE    PRU20                                                            
         LA    R5,4(R5)            BUMP TO THE NEXT ENTRY                       
         B     PRU10                                                            
*                                                                               
PRU20    MVC   TEMPPR,0(R5)        SAVE MNEMONIC VALUE OF PRODUCT               
         BAS   RE,SSV              RESTORE SYSTEM VALUES                        
         MVC   KEY,TEMPKEY         RESTORE KEY AND AIO                          
         MVC   AIO,AIO1                                                         
         DROP  R4                                                               
         DROP  R6                                                               
         XIT1                                                                   
                                                                                
*********************************************************************           
*                  LIST BUCH RECORDS                                *           
*********************************************************************           
                                                                                
LR       LA    R5,KEY                                                           
         USING BGRKEYD,R5                                                       
*                                                                               
         BAS   RE,HIERAR           CHECK HIERARCHY OF FILTERS                   
*                                                                               
         OC    KEY,KEY             FIRST TIME THROUGH?                          
         BNZ   LR2                                                              
         MVC   KEY,MYKEY                                                        
         MVC   CHKEY,MYKEY         PREVIOUS KEY                                 
*                                                                               
LR2      GOTO1 HIGH                                                             
*                                                                               
LR5      CLC   KEY(3),CHKEY        MORE RECORDS?                                
         BNE   LRX                                                              
*                                                                               
         CLC   KEY(L'BGRKMAST),CHKEY         DON'T DISPLAY MINIO                
*                                            SUB RECORDS                        
         BE    LR10                                                             
*                                                                               
         TM    FILTFLAG,CLIENTF    CLIENT FILTER                                
         BZ    LR7                                                              
         CLC   BGRKCLT,SVCLT                                                    
         BNE   LR10                MARKET?                                      
*                                                                               
         TM    FILTFLAG,PRODUCTF   PRODUCT FILTER                               
         BZ    LR7                                                              
         CLC   BGRKPRD,SVPRD                                                    
         BNE   LR10                MARKET?                                      
*                                                                               
         TM    FILTFLAG,ESTIMF     ESTIMATE FILTER                              
         BZ    LR7                                                              
         CLC   BGRKEST,SVEST                                                    
         BNE   LR10                MARKET?                                      
*                                                                               
LR7      TM    FILTFLAG,MARKETF    MARKET FILTER                                
         BZ    LR8                 CLIENT?                                      
         CLC   BGRKMKT,SVMKT                                                    
         BNE   LR10                                                             
*                                                                               
LR8      GOTO1 GETREC              SAVE DISK ADDR                               
         XC    LISTAR,LISTAR                                                    
         MVC   SAVEKEY,KEY         CLUNPK MIGHT BE USING KEY FIELD              
         BAS   RE,RSV              MIGHT BE READING RECORDS FROM                
*                                  SPOTFIL, SPOTDIR FILES                       
*                                                                               
         GOTO1 CLUNPK,DMCB,BGRKCLT,CLIENT                                       
*                                                                               
         BAS   RE,PRUNPK           CONVERT PRODUCT TO EBCIDIC FORMAT            
         MVC   PRODUCT,TEMPPR                                                   
*                                                                               
         EDIT  BGRKEST,ESTIM,FILL=0                                             
*                                                                               
         GOTO1 MSUNPK,DMCB,BGRKMKT,MARKET                                       
*                                                                               
         BAS   RE,SSV                                                           
         MVC   KEY,SAVEKEY                                                      
*                                                                               
         GOTO1 GETREC              SAVE DISK ADDR                               
         GOTO1 LISTMON                                                          
         MVC   CHKEY,KEY           SAVE PREVIOUS KEY                            
*                                                                               
LR10     GOTO1 SEQ                                                              
         B     LR5                                                              
*                                                                               
LRX      B     EXIT                                                             
         DROP  R5                                                               
                                                                                
********************************************************************            
*                     SET SYSTEM VALUES                            *            
********************************************************************            
                                                                                
SSV      NTR1                                                                   
         MVC   LKEY,=H'32'             DETAILS OF DIRECTORY AND KEY             
         MVC   LSTATUS,=H'4'                                                    
         MVC   DATADISP,=H'42'                                                  
         MVC   SYSFIL,=C'XSPFIL  '                                              
         MVC   SYSDIR,=C'XSPDIR  '                                              
         XIT1                                                                   
                                                                                
********************************************************************            
*                   RESET SYSTEM VALUES                            *            
********************************************************************            
                                                                                
RSV      NTR1                                                                   
         MVC   LKEY,=H'13'         DETAILS OF DIRECTORY AND KEY                 
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'24'     USUALLY SPOTFILE                             
         MVC   SYSFIL,=C'SPTFIL  '                                              
         MVC   SYSDIR,=C'SPTDIR  '                                              
         XIT1                                                                   
                                                                                
*********************************************************************           
*                  CHECK FOR HIERARCHY                              *           
*********************************************************************           
                                                                                
HIERAR   NTR1                                                                   
         CLI   BCLCLIH+5,0                                                      
         BNE   HIER20                                                           
*                                                                               
HIER10   CLI   BCLPRDH+5,0                                                      
         BE    HIER20                                                           
         LA    R2,BCLCLIH                                                       
         MVI   ERROR,MISSING       CLIENT MISSING                               
         GOTO1 ERREX                                                            
*                                                                               
HIER20   CLI   BCLESTH+5,0                                                      
         BE    HIERX                                                            
         CLI   BCLPRDH+5,0                                                      
         BNE   HIERX                                                            
         LA    R2,BCLPRDH                                                       
         MVI   ERROR,MISSING       PRODUCT MISSING                              
         GOTO1 ERREX                                                            
*                                                                               
HIERX    XIT1                                                                   
                                                                                
*********************************************************************           
*                  CLEAR FIELDS                                     *           
*********************************************************************           
                                                                                
CF       NTR1                                                                   
         XC    BCHRDT,BCHRDT                                                    
         OI    BCHRDTH+6,X'80'                                                  
         XC    BCHADT,BCHADT                                                    
         OI    BCHADTH+6,X'80'                                                  
         XC    BCHPER,BCHPER                                                    
         OI    BCHPERH+6,X'80'                                                  
         XC    BCHAST,BCHAST                                                    
         OI    BCHASTH+6,X'80'                                                  
         XC    BCHMRDT,BCHMRDT                                                  
         OI    BCHMRDTH+6,X'80'                                                 
         XC    BCHMST,BCHMST                                                    
         OI    BCHMSTH+6,X'80'                                                  
*                                                                               
CFX      XIT1                                                                   
                                                                                
*********************************************************************           
*                  IS THERE PFM OPTION?                             *           
*********************************************************************           
                                                                                
OPTPFM   NTR1                                                                   
         CLI   ACTNUM,ACTLIST                                                   
         BNE   OPTPFMX             SKIP IF NOT LIST                             
*                                                                               
         LA    R4,BCLSELH          POINT TO FIRST SEL                           
         LA    R5,BCLLAST          LAST LINE ON THE SCREEN                      
         SR    R6,R6               ACCUMULATOR                                  
*                                                                               
OPTPFM10 CR    R4,R5                                                            
         BNL   OPTPFMX             ARE ALL ENTRIES CHECKED?                     
         LA    R6,1(R6)            INCREMENT ACCUMULATOR                        
         CLC   8(L'BCLSEL,R4),=C'PFM'                                           
         BNE   OPTPFM20                                                         
*                                                                               
         BAS   RE,CALLPFM          PFM IS FOUND, SET VALUES                     
         FOUT  (R4),=C'S  '        SETS INDICATOR AND MOVES DATA                
         B     EXIT                EXIT GENCON                                  
*                                                                               
OPTPFM20 ZIC   R3,0(R4)            BUMP POINTERS                                
         AR    R4,R3                                                            
         ZIC   R3,0(R4)                                                         
         AR    R4,R3                                                            
         B     OPTPFM10                                                         
*                                                                               
OPTPFMX  XIT1                                                                   
                                                                                
*********************************************************************           
*                  AT THIS POINT WE KNOW THAT                       *           
*          ACTION IS LIST AND USER WANT TO CALL PFM                 *           
*********************************************************************           
                                                                                
CALLPFM  NTR1                                                                   
         BCTR  R6,0                                                             
         MH    R6,=H'6'            DISPLACEMENT INTO DISK ADDR LIST             
         L     RF,ACOMFACS                                                      
         USING COMFACSD,RF                                                      
         MVC   VGLOBBER,CGLOBBER                                                
         DROP  RF                                                               
         XC    ELEM,ELEM                                                        
         LA    R1,ELEM                                                          
         USING GLPFMFIL,R1                                                      
         MVC   GLPFMFIL(6),=C'XSPFIL'                                           
         LA    R5,LISTDIR          LIST DIR LIST                                
         AR    R5,R6                                                            
         MVC   GLPFMDA,2(R5)       DISK ADDR                                    
         MVC   GLPFMKEY(4),=C'*   '                                             
         GOTO1 VGLOBBER,DMCB,=C'PUTD',ELEM,54,GLPFMCDQ                          
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R1,ELEM                                                          
         USING GLVXFRSY,R1                                                      
*                                                                               
         MVC   GLVXFRSY,=C'SPO'                                                 
         MVC   GLVXTOSY,=C'SPO'                                                 
         MVC   GLVXFRPR,=C'SFM'                                                 
         MVC   GLVXTOPR,=C'PFM'                                                 
         OI    GLVXFLG1,GLV1SEPS                                                
         GOTO1 VGLOBBER,DMCB,=C'PUTD',ELEM,14,GLVXCTL                           
         XIT1                                                                   
         DROP  R1                                                               
                                                                                
*********************************************************************           
         GETEL R6,DATADISP,ELCODE                                               
RELO     DS    F                                                                
MYKEY    DS    CL48                                                             
TEMPKEY  DS    CL48                                                             
TEMPPR   DS    CL3                                                              
FILTFLAG DS    X                   FILTER FLAG                                  
CLIENTF  EQU   X'80'                                                            
PRODUCTF EQU   X'40'                                                            
ESTIMF   EQU   X'20'                                                            
MARKETF  EQU   X'10'                                                            
VGLOBBER DS    V                                                                
*********************************************************************           
         LTORG                                                                  
         EJECT                                                                  
*        PRINT OFF                                                              
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE SPSFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SCSFM7ED                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SCSFM7FD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE SPSFMWORKD                                                     
*                                                                               
         ORG   SYSSPARE           1K APPLICAITON AREA                           
CHKEY    DS    CL48                                                             
SAVEKEY  DS    CL48                                                             
SVTYPE   DS    X                                                                
SVSUB    DS    X                                                                
SVMED    DS    X                                                                
SVCLT    DS    XL2                                                              
SVPRD    DS    X                                                                
SVSPARE  DS    X                                                                
SVEST    DS    X                                                                
SVMKT    DS    XL2                                                              
*                                                                               
         SPACE 2                                                                
         EJECT                                                                  
       ++INCLUDE DDPSTBLK                                                       
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
         EJECT                                                                  
BILHDRD  DSECT                                                                  
       ++INCLUDE SPGENBILL                                                      
         EJECT                                                                  
       ++INCLUDE SPGENBGR                                                       
*        PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDFLDIND                                                       
       ++INCLUDE FATIOB                                                         
       ++INCLUDE DDMINBLK                                                       
       ++INCLUDE DDGLVXCTLD                                                     
       ++INCLUDE DDGLPFMD                                                       
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
CLIENT   DS    CL3                                                              
         DS    CL5                                                              
PRODUCT  DS    CL3                                                              
         DS    CL6                                                              
ESTIM    DS    CL3                                                              
         DS    CL5                                                              
MARKET   DS    CL4                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'068SPSFM48   03/14/02'                                      
         END                                                                    
