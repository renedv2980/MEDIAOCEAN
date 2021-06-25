*          DATA SET PPADS02    AT LEVEL 067 AS OF 01/04/99                      
*PHASE T40B02A,*                                                                
*INCLUDE SRCHCALL                                                               
*        TITLE 'PPADS02 SRDS LIST SCREEN'                                       
         TITLE 'PPADS02 SRDS LIST SCREEN - LOG'                                 
***********************************************************************         
*                                                                     *         
*        CHANGE LOG                                                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
*SEP18/98 BOBY CREATION                                                         
*                                                                               
         TITLE 'PPADS02 SRDS LIST SCREEN - INIT'                                
***********************************************************************         
*                                                                     *         
*        PROGRAM INITIALIZATION                                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
T40B02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T40B02,RR=R3                                                   
*                                                                               
         L     RC,0(R1)            ESTABLISH GENCON WORKING STORAGE             
         USING GEND,RC                                                          
*                                                                               
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
*                                                                               
         L     R9,ASYSD            ESTABLISH SYSTEM WORKING STORAGE             
         USING SYSD,R9                                                          
*                                                                               
         L     R8,ASPOOLD          ESTABLISH SPOLL WORKING STORAGE              
         USING SPOOLD,R8                                                        
*                                                                               
         ST    R3,RELO             SAVE RELOCATION FACTOR                       
*===>                                                                           
         MVI   CONSERVH+6,X'81'    FORCE SRV REQ FIELD MODIFIED                 
*===>                                                                           
         OI    GENSTAT4,NODELLST   STOP DELETING FROM LIST SCREEN               
*                                  # OF LIST LINES                              
         MVI   NLISTS,(LSTSELLH-LSTSEL1H)/(LSTSEL2H-LSTSEL1H)+1                 
*                                                                               
         GOTO1 VHELPCHK            CHECK IF IN MIDST OF A HELP CALL             
*                                                                               
         TITLE 'PPADS02 SRDS LIST SCREEN - MODANAL'                             
***********************************************************************         
*                                                                     *         
*        MODE ANALYSIS                                                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
MODANAL  DS    0H                                                               
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,DISPKEY        DISPLAY  RECORD KEY                          
         BE    DK                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         B     XIT                                                              
*                                                                               
EXIT     DS    0H                                                               
*                                                                               
*        IF PFKEY HIT THEN CURSOR REMAINS WHERE IT WAS                          
*                                                                               
         CLI   PFKEY,0             EXIT IF PFKEY NOT HIT                        
         BE    EXITX                                                            
*                                                                               
         CLI   PFKEY,12            SKIP IF PFKEY 12  HIT                        
         BE    EXITX                                                            
*                                                                               
         CLI   PFKEY,24            SKIP IF PFKEY 24  HIT                        
         BE    EXITX                                                            
*                                                                               
         OI    GENSTAT2,RETEQSEL   HAVE GENCON RETURN THIS SCREEN               
*                                                                               
         OC    ACURFORC,ACURFORC   IF NO CURSOR ADDRESS SET                     
         BNZ   *+12                                                             
         LA    R2,CONACTH             PLACE IT AT ACTION FIELD                  
         ST    R2,ACURFORC                                                      
*                                                                               
EXITX    DS    0H                                                               
*                                                                               
XIT      XIT1                                                                   
*                                                                               
         TITLE 'PPADS02 SRDS LIST SCREEN - VALKEY'                              
***********************************************************************         
*                                                                     *         
*        VALIDATE LIST KEY                                            *         
*                                                                     *         
*        TYPE IS ONLY REQUIRED KEY FIELD                              *         
*                                                                     *         
*        FILTERING ALLOWED ON TITLE                                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VK       DS    0H                                                               
*                                                                               
VKTYP    DS    0H                  VALIDATE SRDS PUBLICATION TYPE               
*                                                                               
         XC    LSRDTYP,LSRDTYP     INIT TYPE                                    
         XC    LSRDTYPD,LSRDTYPD   INIT TYPE DESCRIPTION                        
*                                                                               
         LA    R2,LSTTYPH          TYPE - REQUIRED                              
*                                                                               
         GOTO1 VVALSTYP                                                         
*                                                                               
         MVC   LSRDTYP,SRDTYP      SAVE STARTING TYPE                           
         MVC   LSRDTYPD,SRDTYPDS   SAVE STARTING TYPE DESCRIPTION               
*                                                                               
VKTYPX   DS    0H                                                               
*                                                                               
VKTIT    DS    0H                  VALIDATE SRDS TITLE                          
*                                                                               
         LA    R2,LSTTITH          SRDS STARTING TITLE                          
*                                                                               
         XC    LSRDNUM,LSRDNUM     INIT SRDS NUMBER                             
         XC    LSRDTIT,LSRDTIT     INIT SRDS TITLE                              
*                                                                               
         GOTO1 VVALSTIT                                                         
*                                                                               
         MVC   LSRDNUM,SRDNUM      SET LIST STARTING NUMBER                     
         MVC   LSRDTIT,SRDTIT      SET LIST STARTING TITLE                      
*                                                                               
VKTITX   DS    0H                                                               
*                                                                               
VKDAT    DS    0H                  VALIDATE SRDS PUBLICATION TYPE               
*                                                                               
         XC    LSRDDATA,LSRDDATA   INIT DATA TYPE                               
*                                                                               
         LA    R2,LSTDATAH         DATA TYPE - REQUIRED                         
*                                                                               
         GOTO1 VVALSDAT                                                         
*                                                                               
         MVC   LSRDDATA,SRDDATA    SAVE STARTING DATA TYPE                      
*                                                                               
VKDATX   DS    0H                                                               
*                                                                               
*        BUILD HEADER RECORD KEY                                                
*                                                                               
VKKEY    DS    0H                                                               
*                                                                               
*        USE NAME SEQUENCE PASSIVE KEY                                          
*                                                                               
         XC    KEY,KEY             INIT KEY                                     
         LA    R6,KEY              ESTABLISH AS CONTROL PUB KEY                 
         USING GPUBNKYD,R6                                                      
*                                                                               
         MVI   GPUBNREC,GPUBNRCQ   SET RECORD CODE                              
         MVI   GPUBNTYP,GPUBNTYQ   SET RECORD TYPE                              
*                                                                               
         MVC   GPUBNPBT,LSRDTYP    SET PUB TYPE                                 
         MVC   GPUBNNAM,LSRDTIT    SET PUB NAME                                 
*                                                                               
VKEYX    DS    0H                                                               
*                                                                               
         XIT1                                                                   
*                                                                               
         TITLE 'PPADS02 SRDS LIST SCREEN - DISKEY'                              
***********************************************************************         
*                                                                     *         
*        DISPLAY KEY                                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DK       DS    0H                                                               
*                                                                               
         DC    H'0'                SHOULD NOT BE HERE                           
*                                                                               
         B     VK                  VALIDATE THE KEY                             
*                                                                               
         TITLE 'PPADS02 SRDS LIST SCREEN - LR'                                  
***********************************************************************         
*                                                                     *         
*        LIST RECORDS                                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
LR       DS    0H                                                               
*                                                                               
         GOTO1 =A(LISTREC),RR=RELO                                              
*                                                                               
LRX      DS    0H                                                               
         XIT1                                                                   
*                                                                               
*                                                                               
*                                                                               
* PRINT REPORT                                                                  
*                                                                               
PR       L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
******                                                                          
******   LA    R1,HEDSPECS         SET UP HEADHOOK AND SPECS                    
******   ST    R1,SPECS                                                         
******   LA    R1,HOOK                                                          
******   ST    R1,HEADHOOK                                                      
         B     LR                  USE LIST REC LOGIC                           
*                                                                               
PRX      XIT1                                                                   
         EJECT                                                                  
HOOK     NTR1                      HEADLINE ROUTINES                            
*                                                                               
HOOKX    XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPADS02 SRDS LIST SCREEN - LISTREC'                             
***********************************************************************         
*                                                                     *         
*        LIST RECORDS                                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
LISTREC  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         USING SYSD,R9             ESTABLISH PROGRAM WORKING STORAGE            
         USING SPOOLD,R8           ESTABLISH SPOOLER WORKING STORAGE            
*                                                                               
         OI    GLSTSTAT,RETEXTRA                                                
*                                                                               
         LA    R4,KEY              ESTABLISH SRD PUB NAME PASSIVE               
         USING GPUBNKYD,R4                                                      
*                                                                               
         OC    KEY,KEY             IF NO PREVIOUS KEY AROUND                    
         BNZ   LRKEY10                                                          
*                                                                               
         MVI   GPUBNREC,GPUBNRCQ      SET RECORD CODE                           
         MVI   GPUBNTYP,GPUBNTYQ      SET RECORD TYPE                           
*                                                                               
         MVC   GPUBNPBT,LSRDTYP       SET PUB TYPE                              
         MVC   GPUBNNAM,LSRDTIT       SET PUB NAME                              
*                                                                               
         GOTO1 HIGH                READ FIRST RECORD                            
*                                                                               
         B     LRKEYX                                                           
*                                                                               
*        FIND PASSIVE FROM PREVIOUS KEY                                         
*                                                                               
LRKEY10  DS    0H                                                               
*                                                                               
         XC    KEY,KEY             ESTABLISH KEY AS PASSIVE                     
         USING GPUBNKYD,R4                                                      
*                                                                               
*        BUILD PASSIVE KEY                                                      
*                                                                               
         L     R6,AIO              POINT TO FOUND RECORD                        
*                                                                               
*        RECORD WAS READ IN BY GENCON                                           
*                                                                               
         MVI   GPUBNREC,GPUBNRCQ      SET RECORD CODE                           
         MVI   GPUBNTYP,GPUBNTYQ      SET RECORD TYPE                           
*                                                                               
         MVC   GPUBNPBT,GPUBPUBT-GPUBKEYD(R6)  SET PUB TYPE                     
*                                                                               
         MVC   GPUBNPUB,GPUBPUB-GPUBKEYD(R6)   SRDS PUBCODE                     
*                                                                               
         MVI   ELCODE,GPUBHEQU     FIND HEADER ELEMENT                          
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST FIND IT                                 
*                                                                               
         USING GPUBHEL,R6          ESTABLISH HEADER ELEMENT                     
*                                                                               
         MVC   GPUBNNAM,GPUBHALN   SET PUB NAME                                 
*                                                                               
         GOTO1 HIGH                READ LAST KEY ON PREV PAGE                   
*                                                                               
         GOTO1 SEQ                 READ NEXT PASSIVE ON FILE                    
*                                                                               
         B     LRKEYX                                                           
*                                                                               
LRKEYX   DS    0H                                                               
*                                                                               
LRLOOP   DS    0H                                                               
*                                                                               
         CLC   GPUBNKY(GPUBNNAM-GPUBNKYD),KEYSAVE DONE ON CHG IN TYPE           
         BNE   LREND                                                            
*                                                                               
         GOTO1 GETREC                GET PUB RECORD                             
*                                                                               
         L     R4,AIO              POINT TO FOUND RECORD                        
         USING GPUBKEYD,R4         ESTABLISH RECORD                             
*                                                                               
         CLC   GPUBAGY,AGENCY      SKIP IF NOT THIS AGENCY                      
         BE    *+10                                                             
         CLC   GPUBAGY,=C'ZZ'           OR GENERIC AGENCY                       
         BNE   LRCONT                                                           
*                                                                               
         CLI   GPUBSTYP,0          SKIP IF NOT HEADER RECORD                    
         BNE   LRCONT                                                           
*                                                                               
         MVC   LISTAR,SPACES                                                    
         LA    R5,LISTAR           ESTABLISH LIST LINE                          
         USING LISTD,R5                                                         
*                                                                               
         LR    R6,R4                                                            
         MVI   ELCODE,GPUBHEQU     FIND MAIN ELEMENT                            
         BRAS  RE,GETEL                                                         
*                                                                               
         USING GPUBHEL,R6          ESTABLISH MAIN ELEMENT                       
*                                                                               
         MVC   LSSRDTIT,GPUBHALN   DISPLAY PUB TITLE                            
*                                                                               
         CLC   LSSRDTIT,SPACES     IF NO NAME                                   
         BH    *+10                                                             
         MVC   LSSRDTIT,GPUBHCOR      USE THE CORPORATE NAME                    
*                                                                               
         CLI   TWAOFFC,C'*'        IF DDS TERMINAL                              
         BNE   *+10                                                             
         MVC   LSSRDNUM,GPUBPUB       DISPLAY PUB NUMBER                        
*                                                                               
         CLI   TWAOFFC,C'*'        IF DDS TERMINAL                              
         BNE   *+22                                                             
         UNPK  WORK(9),DMDSKADD(5) UNPACK DISKADDR                              
         MVC   LSDSKADD,WORK       MOVE IN ADDRESS PORTION                      
         TR    LSDSKADD,HEXTABLE-C'0'   MAKE PRINTABLE                          
*                                                                               
         GOTO1 LISTMON             CALL LISTMON                                 
*                                                                               
         LA    R4,KEY              RE-ESTABLISH SRD PUB KEY                     
*                                                                               
LRCONT   DS    0H                                                               
*                                                                               
         GOTO1 SEQ                 READ NEXT RECORD                             
         B     LRLOOP                                                           
*                                                                               
LRDONE   DS    0H                                                               
*                                                                               
         B     LISTRECX                                                         
*                                                                               
LREND    DS    0H                  END OF LIST                                  
*                                                                               
         XC    KEY,KEY             CLEAR RECORD KEY                             
*                                                                               
LISTRECX XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DS    CL240                                                            
HEXTABLE DC    C'0123456789ABCDEF'                                              
*                                                                               
         TITLE 'PPADS10 - SRDS LIST SCREEN - ERRFLD'                            
***********************************************************************         
*                                                                     *         
* HANDLE FIELD IN ERROR - R1 -POINTS TO FIELD                         *         
*         HIGHLIGHT FIELD                                             *         
*         ERROR MESSAGE IS IN ERROR                                   *         
*         IF SAVMSGNO IS NOT FVFOK THEN THIS IS NOT FIRST ERROR       *         
*            ROUTINE RESTORES ERROR TO SAVMSGNO                       *         
*         ELSE                                                        *         
*            ROUTINE SETS CURSOR TO THIS FIELD                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         USING FLDHDRD,R1          ESTABLISH HEADER                             
ERRFLD   DS    0H                                                               
         OI    FLDATB,FATBHIGH     HIGHLIGHT FIELD                              
         OI    FLDOIND,FOUTTRN     TRANSMIT FIELD                               
         NI    FLDIIND,X'FF'-FINPVAL TURN OFF VALID INDICATOR                   
*                                                                               
         ST    R1,ACURFORC         PUT CURSOR HERE                              
*                                                                               
ERRFLDX  DS    0H                                                               
         BR    RE                                                               
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPADS02 - SRDS LIST SCREEN - BUMP'                              
***********************************************************************         
*                                                                     *         
*        BUMP TO NEXT FIELD ON SCREEN                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
BUMP     ZIC   RF,0(R2)            BUMP TO NEXT SCREEN FIELD                    
         AR    R2,RF                                                            
         CLI   0(R2),0             EOS- RETURN =                                
         BR    RE                                                               
*                                                                               
BUMPU    ZIC   RF,0(R2)            BUMP TO NEXT UNPROTECTED FIELD               
         AR    R2,RF                                                            
         CLI   0(R2),0             EOS- RETRUN =                                
         BER   RE                                                               
         TM    1(R2),X'20'                                                      
         JNZ   BUMPU                                                            
         LTR   RE,RE               NOT EOS- RETURN NOT =                        
         BR    RE                                                               
*                                                                               
         TITLE 'PPADS02 - SRDS LIST SCREEN - HEDSPECS'                          
***********************************************************************         
*                                                                     *         
*        REPORT HEADLINES                                             *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
*                                                                               
HEDSPECS SSPEC H1,57,C'SRDS PUBS REPORT'                                        
         SSPEC H2,57,C'----------------'                                        
         SSPEC H1,95,AGYNAME                                                    
         SSPEC H2,95,AGYADD                                                     
         SSPEC H4,95,RUN                                                        
         SSPEC H5,95,REPORT                                                     
         SSPEC H1,1,REQUESTOR                                                   
         DC    X'00'                                                            
*                                                                               
         TITLE 'PPADS02 - SRDS LIST SCREEN - GETEL MACRO'                       
***********************************************************************         
*                                                                     *         
*        GETEL MACRO                                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
*                                                                               
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         USING SYSD,R9             ESTABLISH PROGRAM WORKING STORAGE            
         USING SPOOLD,R8           ESTABLISH SPOOLER WORKING STORAGE            
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
         TITLE 'PPADS02 - SRDS LIST SCREEN - LISTD'                             
***********************************************************************         
*                                                                     *         
*        ON-SCREEN LIST LINE                                          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
LISTD    DSECT                                                                  
LSSRDTIT DS    CL50                SRDS PUBLICATION TITLE                       
         DS    CL04                                                             
LSSRDNUM DS    CL9                 SRDS PUBLICATION NUMBER                      
         DS    CL1                   ONLY SHOWN FOR DDS TERMINALS               
LSDSKADD DS    CL8                 DISK ADDRESS                                 
         DS    CL06                                                             
*                                    ONLY SHOWN FOR ACTION PFM AND DDS          
*                                                                               
         TITLE 'PPADS02 - SRDS LIST SCREEN - PPADSWORKD'                        
***********************************************************************         
*                                                                     *         
*        SYSTEM WORKING STORAGE                                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
       ++INCLUDE PPADSWORKD                                                     
*                                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        PROGRAM WORKING STORAGE                                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
SYSD     DSECT                                                                  
         ORG   SYSSPARE                                                         
*                                                                               
         TITLE 'PPADS02 - SRDS LIST SCREEN - SCREEN OUTLAY - PPADSE0D'          
***********************************************************************         
*                                                                     *         
*        LIST SCREEN OUTLAY                                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
       ++INCLUDE PPADSFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE PPADSE0D                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         TITLE 'PPADS02 - SRDS LIST SCREEN - HIDDEN DSECTS - DSECTS'            
***********************************************************************         
*                                                                     *         
*        HIDDEN DSECTS                                                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
*DDSPOOLD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
*DDCOMFACS                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
*DDFLDHDR                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDFLDHDR                                                       
         PRINT ON                                                               
*PPSRCHPARM                                                                     
         PRINT OFF                                                              
       ++INCLUDE PPSRCHPARM                                                     
         PRINT ON                                                               
*PRGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE PRGENFILE                                                      
         PRINT ON                                                               
*PRGLOBEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE PRGLOBEQUS                                                     
         PRINT ON                                                               
*DDGLOBEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDGLOBEQUS                                                     
         PRINT ON                                                               
*DDGLVXCTLD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDGLVXCTLD                                                     
         PRINT ON                                                               
*FAGETTXTD                                                                      
         PRINT OFF                                                              
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
*DDSPLWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
*FATIOB                                                                         
         PRINT OFF                                                              
       ++INCLUDE FATIOB                                                         
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'067PPADS02   01/04/99'                                      
         END                                                                    
