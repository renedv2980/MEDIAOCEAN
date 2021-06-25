*          DATA SET SPSFM68    AT LEVEL 099 AS OF 05/18/05                      
*PHASE T21768A                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        T21768  -- GFEST MAINTENANCE                         *         
*                                                                     *         
*  INPUTS:       SCREEN SCSFM44 (MAINT) & SCSFM45 (LIST)              *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- SCREEN FIELD HEADER                            *         
*                R3 -- WORK                                           *         
*                R4 -- WORK                                           *         
*                R5 -- WORK                                           *         
*                R6 -- GETEL REGISTER                                 *         
*                R7 -- SECOND BASE                                    *         
*                R8 -- SPOOL                                          *         
*                R9 -- SYSD                                           *         
*                RA -- TWA                                            *         
*                RB -- FIRST BASE                                     *         
*                RC -- GEND                                           *         
*                RD -- SYSTEM                                         *         
*                RE -- SYSTEM                                         *         
*                RF -- SYSTEM                                         *         
*                                                                     *         
***********************************************************************         
         TITLE 'T21768 - GFEST MAINTENANCE'                                     
T21768   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1768**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
*                                                                               
         BAS   RE,SETUP                                                         
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY (FOR LIST)                       
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,RECDEL         DELETE RECORD                                
         BE    DELR                                                             
         CLI   MODE,LISTRECS       DISPLAY RECORD                               
         BE    LR                                                               
         CLI   MODE,XRECADD        RECORD JUST ADDED?                           
         BE    AR                  YES, ADD MEDIA N RECORD                      
         CLI   MODE,XRECPUT        RECORD JUST CHANGED?                         
         BE    AR                  YES, ADD MEDIA N RECORD                      
         CLI   MODE,PRINTREP       PRINT REPORT BASED ON LIST                   
         BNE   XIT                                                              
         LA    R5,HDHOOK                                                        
         ST    R5,HEADHOOK                                                      
         LA    R5,HEDSPECS                                                      
         ST    R5,SPECS                                                         
         B     LR                                                               
*                                                                               
EQXIT    CR    RB,RB                                                            
         B     XIT                                                              
NEQXIT   LTR   RB,RB                                                            
XIT      XIT1                                                                   
***********************************************************************         
*                       VALIDATE KEY                                  *         
***********************************************************************         
VK       DS    0H                                                               
*                                                                               
         MVI   FILTFLAG,X'00'                                                   
*                                                                               
         LA    R3,KEY1                                                          
         USING PGESTD,R3                                                        
*                                                                               
         XC    PGKEY,PGKEY                                                      
         MVI   PGKRID,PGKNDIRQ       RECORD TYPE                                
         MVI   PGKSID,PGKNDISQ              SUBTYPE                             
*                                                                               
         MVC   GFEMEDE,SPACES        CLEAR MEDIA NAME AND CLIENT NAME           
         OI    GFEMEDEH+6,X'80'      AND PRODUCT NAME                           
         MVC   GFECLIE,SPACES                                                   
         OI    GFECLIEH+6,X'80'                                                 
         MVC   GFEPRDE,SPACES                                                   
         OI    GFEPRDEH+6,X'80'                                                 
*                                                                               
* MEDIA                                                                         
         LA    R2,GFEMEDSH           MEDIA                                      
         GOTO1 VALIMED               VALIDATE MEDIA CODE AND TRANSMIT           
         MVC   GFEMEDE,MEDNM         MEDIA NAME                                 
         OI    GFEMEDEH+6,X'80'                                                 
*                                                                               
         CLI   ACTNUM,ACTDIS                                                    
         BE    VK05                                                             
         CLI   SVAPROF+7,C'C'      IF CANADIAN                                  
         BNE   VK05                                                             
         CLI   QMED,C'C'           MEDIUM C AND N ONLY FOR DISPLAY              
         BE    ERRINV                                                           
***      CLI   QMED,C'N'                                                        
***      BE    ERRINV                                                           
*                                                                               
VK05     DS    0H                                                               
         MVC   PGKAM,BAGYMD          COPY MEDIA INTO KEY                        
*                                                                               
* CLIENT                                                                        
         LA    R2,GFECLISH           CLIENT                                     
         GOTO1 VALICLT               VALIDATE CLIENT CODE AND TRANSMIT          
         MVC   GFECLIE,CLTNM         CLIENT NAME                                
         OI    GFECLIEH+6,X'80'                                                 
         MVC   PGKCLT,BCLT          COPY CLIENT INTO KEY                        
*                                                                               
* PRODUCT                                                                       
         LA    R2,GFEPRDSH           PRODUCT                                    
*                                                                               
         CLI   ACTEQU,ACTLIST                                                   
         BNE   VK08                  IF NOT LIST, PROCEED AS USUAL              
*                                                                               
         CLI   GFEPRDSH+5,0          INPUT IN PRODUCT?                          
         BE    VK12                  NO, SKIP TO ESTIMATE                       
*                                                                               
VK08     DS    0H                                                               
         MVI   AAAOK,C'Y'                                                       
         GOTO1 VALIPRD                                                          
         MVI   AAAOK,C'N'                                                       
         MVC   GFEPRDE,PRDNM         VALIDATE PRODUCT CODE AND TRANSMIT         
         OI    GFEPRDEH+6,X'80'      PRODUCT NAME                               
*                                                                               
VK10     MVC   PGKPRD,QPRD          COPY PRODUCT INTO KEY                       
         OI    FILTFLAG,FPRODQ       IF NO PRODUCT ENTERED                      
         OC    PGKPRD,SPACES                                                    
*                                                                               
* ESTIMATE                                                                      
VK12     DS    0H                                                               
         LA    R2,GFEESTSH           ESTIMATE                                   
*                                                                               
         CLI   ACTEQU,ACTLIST                                                   
         BNE   VK13                                                             
         CLI   GFEESTSH+5,0          ANY INPUT FOR ESTIMATE?                    
         BE    VKX                                                              
*                                                                               
VK13     DS    0H                                                               
         CLI   GFEESTSH+5,0                                                     
         BE    ERRMIS                                                           
*                                                                               
VK20     DS    0H                                                               
*                                                                               
         MVC   ERRNUM,=AL2(ESTERR1)  ESTIMATE CODE MUST BE NUMERIC              
         TM    GFEESTSH+4,X'08'      AND HAVE A LENGTH <=3                      
         BZ    SPERREX                                                          
*                                                                               
VK30     DS    0H                                                               
         ZIC   RE,5(R2)              CONVERT ESTIMATE CODE TO BINARY            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   RE,DUB                                                           
         MVC   ERRNUM,=AL2(ESTERR2)  EST MUST BE BETWEEN 0 - 255                
         CHI   RE,0                                                             
         BL    SPERREX                                                          
         CHI   RE,255                                                           
         BH    SPERREX                                                          
         STC   RE,BEST               STORE BINARY ESTIMATE                      
*                                                                               
VK35     MVC   PGKEST,BEST          SAVE ESTIMATE CODE INTO KEY                 
         OI    FILTFLAG,FESTQ                                                   
VKX      DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(L'PGKEY),PGKEY                                               
         MVC   SVGFEKEY,KEY                                                     
*                                                                               
*        CLI   PGKEST,0                                                         
*        BNE   XIT                                                              
*        OI    GFEBRNH+6,X'40'                                                  
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
         DROP  R3                                                               
*                                                                               
***********************************************************************         
*                       VALIDATE RECORD                               *         
***********************************************************************         
VR       DS    0H                                                               
         USING PGSTELMD,R6                                                      
*                                                                               
         CLI   ACTNUM,ACTADD        ACTION ADD                                  
         BE    VRSKPDEL                                                         
*                                                                               
* DELETE ALL X'10' ELEMENTS                                                     
         GOTO1 HELLO,DMCB,(C'D',=C'SPTFIL'),('PGSTEIDQ',AIO),0                  
         CLI   DMCB+12,0                                                        
         BNZ   ERRINV                                                           
*                                                                               
VRSKPDEL DS    0H                                                               
*                                                                               
VR01     CLI   GFEDBCH+5,0                                                      
         BE    VR02                                                             
*                                                                               
         BAS   RE,SETUPEL                                                       
         MVI   PGSTESEQ,X'01'      FIRST ELEMENT                                
         MVC   PGSTNAME,=C'DIVBRNCD'                                            
         MVC   PGSTDATA(L'GFEDBC),GFEDBC                                        
         GOTO1 HELLO,DMCB,(C'P',=C'SPTFIL'),(0,AIO),ELEM                        
         CLI   DMCB+12,0                                                        
         BNZ   ERRINV                                                           
*                                                                               
VR02     CLI   GFEPRCH+5,0                                                      
         BE    VR03                                                             
*                                                                               
         BAS   RE,SETUPEL                                                       
         MVI   PGSTESEQ,X'02'      SECOND ELEMENT                               
         MVC   PGSTNAME,=C'PROD CD '                                            
         MVC   PGSTDATA(L'GFEPRC),GFEPRC                                        
         GOTO1 HELLO,DMCB,(C'P',=C'SPTFIL'),(0,AIO),ELEM                        
         CLI   DMCB+12,0                                                        
         BNZ   ERRINV                                                           
*                                                                               
VR03     CLI   GFEGFNH+5,0                                                      
         BE    VR04                                                             
*                                                                               
         BAS   RE,SETUPEL                                                       
         MVI   PGSTESEQ,X'03'      THIRD ELEMENT                                
         MVC   PGSTNAME,=C'GF NTRL '                                            
         MVC   PGSTDATA(L'GFEGFN),GFEGFN                                        
         GOTO1 HELLO,DMCB,(C'P',=C'SPTFIL'),(0,AIO),ELEM                        
         CLI   DMCB+12,0                                                        
         BNZ   ERRINV                                                           
*                                                                               
VR04     CLI   GFEGFSH+5,0                                                      
         BE    VR11                                                             
*                                                                               
         BAS   RE,SETUPEL                                                       
         MVI   PGSTESEQ,X'04'      FOURTH ELEMENT                               
         MVC   PGSTNAME,=C'GFSBNTRL'                                            
         MVC   PGSTDATA(L'GFEGFS),GFEGFS                                        
         GOTO1 HELLO,DMCB,(C'P',=C'SPTFIL'),(0,AIO),ELEM                        
         CLI   DMCB+12,0                                                        
         BNZ   ERRINV                                                           
*                                                                               
VR11     DS    0H                                                               
         CLI   GFEBAGYH+5,0                                                     
         BE    VR12                                                             
*                                                                               
         BAS   RE,SETUPEL                                                       
         MVI   PGSTESEQ,X'11'                                                   
         MVC   PGSTNAME,=CL8'GFBILAGY'                                          
         MVC   PGSTDATA(L'GFEBAGY),GFEBAGY                                      
         GOTO1 HELLO,DMCB,(C'P',=C'SPTFIL'),(0,AIO),ELEM                        
         CLI   DMCB+12,0                                                        
         BNZ   ERRINV                                                           
*                                                                               
VR12     DS    0H                                                               
         LA    R2,GFEEXPTH                                                      
         CLI   GFEEXPTH+5,0                                                     
         BE    VR13                                                             
*                                                                               
         CLI   GFEEXPTH+5,6                                                     
         BNE   ERRINV                                                           
         MVI   PGSTESEQ,X'12'                                                   
         MVC   PGSTNAME,=CL8'GFEXPTYP'                                          
         MVC   PGSTDATA(L'GFEEXPT),GFEEXPT                                      
         GOTO1 HELLO,DMCB,(C'P',=C'SPTFIL'),(0,AIO),ELEM                        
         CLI   DMCB+12,0                                                        
         BNZ   ERRINV                                                           
*                                                                               
VR13     DS    0H                                                               
         CLI   GFEPRIDH+5,0                                                     
         BE    VR14                                                             
*                                                                               
         BAS   RE,SETUPEL                                                       
         MVI   PGSTESEQ,X'13'                                                   
         MVC   PGSTNAME,=CL8'GFPRODID'                                          
         MVC   PGSTDATA(L'GFEPRID),GFEPRID                                      
         GOTO1 HELLO,DMCB,(C'P',=C'SPTFIL'),(0,AIO),ELEM                        
         CLI   DMCB+12,0                                                        
         BNZ   ERRINV                                                           
*                                                                               
VR14     DS    0H                                                               
         CLI   GFECAGYH+5,0                                                     
         BE    VR15                                                             
*                                                                               
         BAS   RE,SETUPEL                                                       
         MVI   PGSTESEQ,X'14'                                                   
         MVC   PGSTNAME,=CL8'GFCRTVAG'                                          
         MVC   PGSTDATA(L'GFECAGY),GFECAGY                                      
         GOTO1 HELLO,DMCB,(C'P',=C'SPTFIL'),(0,AIO),ELEM                        
         CLI   DMCB+12,0                                                        
         BNZ   ERRINV                                                           
*                                                                               
VR15     DS    0H                                                               
         CLI   GFESAGYH+5,0                                                     
         BE    VR16                                                             
*                                                                               
         BAS   RE,SETUPEL                                                       
         MVI   PGSTESEQ,X'15'                                                   
         MVC   PGSTNAME,=CL8'GFSRCAGY'                                          
         MVC   PGSTDATA(L'GFESAGY),GFESAGY                                      
         GOTO1 HELLO,DMCB,(C'P',=C'SPTFIL'),(0,AIO),ELEM                        
         CLI   DMCB+12,0                                                        
         BNZ   ERRINV                                                           
*                                                                               
VR16     DS    0H                                                               
         CLI   GFEREQNH+5,0                                                     
         BE    VR17                                                             
*                                                                               
         BAS   RE,SETUPEL                                                       
         MVI   PGSTESEQ,X'16'                                                   
         MVC   PGSTNAME,=CL8'GFREQNUM'                                          
         MVC   PGSTDATA(L'GFEREQN),GFEREQN                                      
         GOTO1 HELLO,DMCB,(C'P',=C'SPTFIL'),(0,AIO),ELEM                        
         CLI   DMCB+12,0                                                        
         BNZ   ERRINV                                                           
*                                                                               
VR17     DS    0H                                                               
         CLI   GFETMKTH+5,0                                                     
         BE    VR18                                                             
*                                                                               
         BAS   RE,SETUPEL                                                       
         MVI   PGSTESEQ,X'17'                                                   
         MVC   PGSTNAME,=CL8'GFTGRMKT'                                          
         MVC   PGSTDATA(L'GFETMKT),GFETMKT                                      
         GOTO1 HELLO,DMCB,(C'P',=C'SPTFIL'),(0,AIO),ELEM                        
         CLI   DMCB+12,0                                                        
         BNZ   ERRINV                                                           
*                                                                               
VR18     DS    0H                                                               
         CLI   GFEDEALH+5,0                                                     
         BE    VRX                                                              
*                                                                               
         BAS   RE,SETUPEL                                                       
         MVI   PGSTESEQ,X'18'                                                   
         MVC   PGSTNAME,=CL8'GFDEALNO'                                          
         MVC   PGSTDATA(L'GFEDEAL),GFEDEAL                                      
         GOTO1 HELLO,DMCB,(C'P',=C'SPTFIL'),(0,AIO),ELEM                        
         CLI   DMCB+12,0                                                        
         BNZ   ERRINV                                                           
*                                                                               
VRX      DS    0H                                                               
         MVI   ACTELOPT,C'N'                                                    
         B     EQXIT               PASSIVE POINTED HAS TO BE RESET              
         EJECT                                                                  
         DROP  R6                                                               
*                                                                               
SETUPEL  DS    0H                                                               
E        USING PGSTELMD,ELEM                                                    
         LA    R6,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   E.PGSTEID,PGSTEIDQ                                               
         MVI   E.PGSTELN,PGSTELNQ                                               
         BR    RE                                                               
         DROP  E                                                                
***********************************************************************         
*          ADD A MEDIA N RECORD IF IT DOES NOT ALREADY EXIST          *         
***********************************************************************         
AR       DS    0H                                                               
         CLI   SVAPROF+7,C'C'       CANADIAN?                                   
         BNE   ADXIT                NO, DONE                                    
*                                                                               
         L     R6,AIO                                                           
         USING PGKEY,R6                                                         
         MVC   BYTE,PGKAM           AGENCY/MEDIA BYTE                           
         NI    BYTE,X'0F'           ISOLATE MEDIA                               
         CLI   BYTE,X'01'           MEDIA T?                                    
         BNE   ADXIT                NO, DON'T ADD MEDIA N                       
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(13),0(R6)        READ THE KEY WE JUST ADDED/CHANGED          
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE      WE BETTER HAVE IT!!!                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         MVC   PGKEY,KEYSAVE                                                    
         OI    PGKAM,X'03'          TURN ON MEDIA N                             
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE      DO WE ALREADY HAVE THIS RECORD?             
         BE    ADXIT                YES, DONE                                   
*                                                                               
         L     R6,AIO                                                           
         OI    PGKAM,X'03'          TURN ON MEDIA N IN RECORD                   
         GOTO1 ADDREC                                                           
         NI    PGKAM,X'F1'          RESTORE MEDIA T IN RECORD                   
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
ADXIT    B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*                       DELETE RECORD                                 *         
***********************************************************************         
DELR     DS    0H                                                               
*                                                                               
         CLI   TWALACT,ACTDEL                                                   
         BE    XIT                                                              
         OI    CONSERVH+1,X'01'                                                 
         OI    CONSERVH+6,X'80'                                                 
         MVC   ERRNUM,=AL2(DELERR)                                              
         B     SPERREX                                                          
***********************************************************************         
*                       DISPLAY KEY                                   *         
***********************************************************************         
DK       DS    0X                                                               
*                                                                               
         L     R3,AIO                                                           
         USING PGKEY,R3                                                         
*                                                                               
         GOTO1 CLUNPK,DMCB,PGKCLT,GFECLIS                                       
         OI    GFECLISH+6,X'80'                                                 
         MVI   GFECLISH+5,3          TRANSMIT CLIENT CODE TO SCREEN             
*                                                                               
         MVC   GFEPRDS,PGKPRD                                                   
         MVI   GFEPRDSH+5,3                                                     
         OI    GFEPRDSH+6,X'80'      TRANSMIT PRODUCT CODE TO SCREEN            
* GET FULL PRODUCT NAME                                                         
         LA    R0,GFEPRDE                                                       
         ST    R0,FULL                                                          
         LHI   R0,L'GFEPRDE                                                     
         STC   R0,FULL                                                          
         BAS   RE,GETPNAM                                                       
         OI    GFEPRDEH+6,X'80'      TRANSMIT PRODUCT NAME TO SCREEN            
*                                                                               
         EDIT  PGKEST,GFEESTS,ALIGN=LEFT,ZERO=NOBLANK                           
         OI    GFEESTSH+6,X'80'      TRANSMIT ESTIMATE CODE TO SCREEN           
         OI    GFEESTSH+4,X'08'      NUMERIC CODE                               
         MVI   GFEESTSH+5,3                                                     
*                                                                               
         CLI   THISLSEL,C'C'         SELECT FOR CHANGE                          
         BNE   DKX                                                              
*                                                                               
         CLI   T217FFD+1,C'*'        TEST DDS TERM                              
         BE    DKX                                                              
         TM    T217FFD+12,X'10'                                                 
         BO    ERRSEC2               ON = NO CHANGE                             
*                                                                               
*                                                                               
DKX      B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
*                       DISPLAY RECORD                                *         
***********************************************************************         
DR       DS    0H                                                               
*                                                                               
         CLI   TWALACT,ACTDEL                                                   
         BNE   *+12                                                             
         OI    CONSERVH+1,X'01'                                                 
         OI    CONSERVH+6,X'80'                                                 
*                                                                               
         L     R6,AIO              A(REC)                                       
         MVI   ELCODE,PGSTEIDQ     X'10'                                        
         TWAXC GFEDBCH                                                          
         USING PGSTELMD,R6                                                      
*                                                                               
         BRAS  RE,GETEL            ESTIMATE <> 0                                
         B     DRLP01                                                           
*                                                                               
DRLP     DS    0H                                                               
         BRAS  RE,NEXTEL                                                        
DRLP01   DS    0H                                                               
         BNE   DRX                                                              
*                                                                               
         CLC   PGSTNAME,=CL8'DIVBRNCD'                                          
         BE    DR01                                                             
         CLC   PGSTNAME,=CL8'PROD CD '                                          
         BE    DR02                                                             
         CLC   PGSTNAME,=CL8'GF NTRL '                                          
         BE    DR03                                                             
         CLC   PGSTNAME,=CL8'GFSBNTRL'                                          
         BE    DR04                                                             
         CLC   PGSTNAME,=CL8'GFBILAGY'                                          
         BE    DR11                                                             
         CLC   PGSTNAME,=CL8'GFEXPTYP'                                          
         BE    DR12                                                             
         CLC   PGSTNAME,=CL8'GFPRODID'                                          
         BE    DR13                                                             
         CLC   PGSTNAME,=CL8'GFCRTVAG'                                          
         BE    DR14                                                             
         CLC   PGSTNAME,=CL8'GFSRCAGY'                                          
         BE    DR15                                                             
         CLC   PGSTNAME,=CL8'GFREQNUM'                                          
         BE    DR16                                                             
         CLC   PGSTNAME,=CL8'GFTGRMKT'                                          
         BE    DR17                                                             
         CLC   PGSTNAME,=CL8'GFDEALNO'                                          
         BE    DR18                                                             
*                                                                               
         B     DRLP                                                             
*                                                                               
DR01     DS    0H                                                               
*                                                                               
         FOUT  GFEDBCH,PGSTDATA,2                                               
         B     DRLP                                                             
*                                                                               
DR02     DS    0H                                                               
*                                                                               
         FOUT  GFEPRCH,PGSTDATA,4                                               
         B     DRLP                                                             
*                                                                               
DR03     DS    0H                                                               
*                                                                               
         FOUT  GFEGFNH,PGSTDATA,3                                               
         B     DRLP                                                             
*                                                                               
DR04     DS    0H                                                               
*                                                                               
         FOUT  GFEGFSH,PGSTDATA,3                                               
         B     DRLP                                                             
*                                                                               
DR11     DS    0H                                                               
*                                                                               
         FOUT  GFEBAGYH,PGSTDATA,8                                              
         B     DRLP                                                             
*                                                                               
DR12     DS    0H                                                               
*                                                                               
         FOUT  GFEEXPTH,PGSTDATA,6                                              
         B     DRLP                                                             
*                                                                               
DR13     DS    0H                                                               
*                                                                               
         FOUT  GFEPRIDH,PGSTDATA,10                                             
         B     DRLP                                                             
*                                                                               
DR14     DS    0H                                                               
*                                                                               
         FOUT  GFECAGYH,PGSTDATA,8                                              
         B     DRLP                                                             
*                                                                               
DR15     DS    0H                                                               
*                                                                               
         FOUT  GFESAGYH,PGSTDATA,8                                              
         B     DRLP                                                             
*                                                                               
DR16     DS    0H                                                               
*                                                                               
         FOUT  GFEREQNH,PGSTDATA,20                                             
         B     DRLP                                                             
*                                                                               
DR17     DS    0H                                                               
*                                                                               
         FOUT  GFETMKTH,PGSTDATA,1                                              
         B     DRLP                                                             
*                                                                               
DR18     DS    0H                                                               
*                                                                               
         FOUT  GFEDEALH,PGSTDATA,10                                             
         B     DRLP                                                             
*                                                                               
DRX      B     XIT                                                              
         EJECT                                                                  
         DROP  R6                                                               
         LTORG                                                                  
*                                                                               
***********************************************************************         
*                       LIST RECORDS                                  *         
***********************************************************************         
LR       DS    0H                                                               
*                                                                               
         XC    GFLTTLE,GFLTTLE                                                  
         XC    GFLUND2,GFLUND2                                                  
         LA    R1,GFLTTLE                                                       
         LA    R2,GFLUND2                                                       
*                                                                               
         TM    FILTFLAG,FPRODQ     PRODUCT FILTER?                              
         BO    LR02                IF YES, DON'T OUTPUT PRODUCT COLUMN          
*                                                                               
         USING PLINED,R1                                                        
         MVC   PPRD,=CL3'PRD'                                                   
         MVC   PPRDE,=CL10'PRD DESC'                                            
         DROP  R1                                                               
*                                                                               
         USING PLINED,R2                                                        
         MVC   PPRD,=3C'-'                                                      
         MVC   PPRDE,=10C'-'                                                    
         DROP  R2                                                               
*                                                                               
         LA    R1,PLINEDLQ(R1)                                                  
         LA    R2,PLINEDLQ(R2)                                                  
*                                                                               
LR02     DS    0H                  OUTPUT TITLE LINE+UNDERLINE                  
*                                                                               
         USING LINED,R1                                                         
         MVC   LEST,=CL3'Est'                                                   
         MVC   LBAGY,=CL8'Bill Agy'                                             
         MVC   LEXPT,=CL6'Expenc'                                               
         MVC   LPRID,=CL10'Product ID'                                          
         MVC   LCAGY,=CL8'Creative'                                             
         MVC   LTMKT,=CL6'Tgt Mk'                                               
         DROP  R1                                                               
*                                                                               
         USING LINED,R2                                                         
         MVC   LEST,=3C'-'                                                      
         MVC   LBAGY,=8C'-'                                                     
         MVC   LEXPT,=6C'-'                                                     
         MVC   LPRID,=10C'-'                                                    
         MVC   LCAGY,=8C'-'                                                     
         MVC   LTMKT,=6C'-'                                                     
         DROP  R2                                                               
*                                                                               
         OI    GFLTTLEH+6,X'80'                                                 
         OI    GFLUND2H+6,X'80'                                                 
*                                                                               
LR08     DS    0H                                                               
*                                                                               
         MVI   NLISTS,12                                                        
         OC    KEY,KEY                                                          
         BNZ   LR09                                                             
         MVC   KEY,SVGFEKEY                                                     
*                                                                               
LR09     DS    0H                                                               
         GOTO1 HIGH                                                             
         B     LR20                                                             
LR10     DS    0H                                                               
         GOTO1 SEQ                                                              
LR20     DS    0H                                                               
*                                                                               
* FILTERING TAKES PLACE HERE                                                    
         LA    R1,KEY                                                           
         USING PGESTD,R1                                                        
*                                                                               
         CLC   KEY(5),SVGFEKEY       SAME A/M, CLIENT?                          
         BNE   LRX                                                              
*                                                                               
         TM    FILTFLAG,FPRODQ       FILTERING BY PRODUCT?                      
         BNO   LR30                  IF NOT, SKIP TO ESTIMATE FILTER            
         CLC   PGKPRD,SVGFEKEY+5     SAME PRODUCT?                              
         BNE   LR10                  IF NOT, GET NEXT RECORD                    
*                                                                               
LR30     DS    0H                                                               
         TM    FILTFLAG,FESTQ        FILTERING BY ESTIMATE?                     
         BNO   LR40                  IF NOT, DISPLAY IT                         
*                                                                               
         CLC   PGKEST,SVGFEKEY+8     SAME ESTIMATE?                             
         BNE   LR10                  IF NOT, GET NEXT RECORD                    
         DROP  R1                                                               
*                                                                               
LR40     DS    0H                                                               
         GOTO1 GETREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* = = = = = = = = = = = = = = = = = = = = = = = = = = *                         
* DATA DISPLAY PART *                                                           
* = = = = = = = = = = = = = = = = = = = = = = = = = = *                         
         L     R6,AIO                                                           
         USING PGESTD,R6                                                        
         LA    R3,P                                                             
         XC    P,P                                                              
*                                                                               
         TM    FILTFLAG,FPRODQ           PRODUCT FILTER?                        
         BO    LR50             YES, NO NEED TO DISPLAY PRODUCT                 
         USING PLINED,R3                                                        
         MVC   PPRD,PGKPRD                                                      
* GET FULL PRODUCT NAME                                                         
         LA    R0,PPRDE                                                         
         ST    R0,FULL                                                          
         LHI   R0,L'PPRDE                                                       
         STC   R0,FULL                                                          
         BAS   RE,GETPNAM                                                       
*                                                                               
         DROP  R3                                                               
         LA    R3,PLINEDLQ(R3)                                                  
         USING LINED,R3                                                         
*                                                                               
LR50     DS    0H                                                               
         EDIT  PGKEST,LEST,ZERO=NOBLANK                                         
*                                                                               
         DROP  R6                                                               
*                                                                               
* NOW DISPLAY INFORMATION STORED IN ELEMENTS                                    
*                                                                               
         L     R6,AIO              A(REC)                                       
         MVI   ELCODE,PGSTEIDQ     X'10'                                        
         USING PGSTELMD,R6                                                      
*                                                                               
         CLI   8(R6),0         ESTIMATE=0?                                      
         BE    LR200                                                            
*                                                                               
         BRAS  RE,GETEL                                                         
         B     LR60                                                             
*                                                                               
LRLP     DS    0H                                                               
         BRAS  RE,NEXTEL                                                        
         BNE   LR210                                                            
*                                                                               
LR60     DS    0H                                                               
         CLC   PGSTNAME,=CL8'GFBILAGY'                                          
         BE    LR111                                                            
         CLC   PGSTNAME,=CL8'GFEXPTYP'                                          
         BE    LR112                                                            
         CLC   PGSTNAME,=CL8'GFPRODID'                                          
         BE    LR113                                                            
         CLC   PGSTNAME,=CL8'GFCRTVAG'                                          
         BE    LR114                                                            
         CLC   PGSTNAME,=CL8'GFTGRMKT'                                          
         BE    LR117                                                            
*                                                                               
         CLI   MODE,PRINTREP                                                    
         BNE   LRLP                                                             
*                                                                               
         CLC   PGSTNAME,=CL8'GFSRCAGY'                                          
         BE    LR115                                                            
         CLC   PGSTNAME,=CL8'GFREQNUM'                                          
         BE    LR116                                                            
         CLC   PGSTNAME,=CL8'GFDEALNO'                                          
         BE    LR118                                                            
         CLC   PGSTNAME,=CL8'DIVBRNCD'                                          
         BE    LR119                                                            
         CLC   PGSTNAME,=CL8'PROD CD '                                          
         BE    LR120                                                            
*                                                                               
         L     RF,AIO              A(REC)                                       
         CLI   8(RF),0         ESTIMATE=0?                                      
         BE    LRLP                                                             
*                                                                               
         CLC   PGSTNAME,=CL8'GF NTRL '                                          
         BE    LR121                                                            
         CLC   PGSTNAME,=CL8'GFSBNTRL'                                          
         BE    LR122                                                            
*                                                                               
         B     LRLP                                                             
*                                                                               
LR111    DS    0H                                                               
*                                                                               
         MVC   LBAGY,PGSTDATA                                                   
         B     LRLP                                                             
*                                                                               
LR112    DS    0H                                                               
*                                                                               
         MVC   LEXPT,PGSTDATA                                                   
         B     LRLP                                                             
*                                                                               
LR113    DS    0H                                                               
*                                                                               
         MVC   LPRID,PGSTDATA                                                   
         B     LRLP                                                             
*                                                                               
LR114    DS    0H                                                               
*                                                                               
         MVC   LCAGY,PGSTDATA                                                   
         B     LRLP                                                             
*                                                                               
LR115    DS    0H                                                               
*                                                                               
         MVC   LSAGY,PGSTDATA                                                   
         B     LRLP                                                             
*                                                                               
LR116    DS    0H                                                               
*                                                                               
         MVC   LREQN,PGSTDATA                                                   
         B     LRLP                                                             
*                                                                               
LR117    DS    0H                                                               
*                                                                               
         MVC   LTMKT(1),PGSTDATA                                                
         B     LRLP                                                             
*                                                                               
LR118    DS    0H                                                               
*                                                                               
         MVC   LDEAL,PGSTDATA                                                   
         B     LRLP                                                             
*                                                                               
LR119    DS    0H                                                               
*                                                                               
         MVC   LDBC(2),PGSTDATA                                                 
         B     LRLP                                                             
*                                                                               
LR120    DS    0H                                                               
*                                                                               
         MVC   LPRC,PGSTDATA                                                    
         B     LRLP                                                             
*                                                                               
LR121    DS    0H                                                               
*                                                                               
         MVC   LGFN,PGSTDATA                                                    
         B     LRLP                                                             
*                                                                               
LR122    DS    0H                                                               
*                                                                               
         MVC   LGFS,PGSTDATA                                                    
         B     LRLP                                                             
*                                                                               
*                                                                               
         SPACE 1                                                                
*==========================================================*                    
* ECHO DATA FOR ESTIMATE = 0                               *                    
*==========================================================*                    
         SPACE 1                                                                
LR200    DS    0H                                                               
         BRAS  RE,GETEL                                                         
         BNE   LR210                                                            
*                                                                               
         CLI   MODE,PRINTREP                                                    
         BNE   LRLP                                                             
*                                                                               
         MVC   LDBC(2),PGSTDATA                                                 
         BRAS  RE,NEXTEL                                                        
         BNE   LR210                                                            
         MVC   LPRC,PGSTDATA                                                    
         MVC   LGFN,SPACES                                                      
         MVC   LGFS,SPACES                                                      
         B     LRLP                                                             
*                                                                               
LR210    DS    0H                                                               
         CLI   MODE,PRINTREP                                                    
         BNE   LR230                                                            
         GOTO1 SPOOL,DMCB,SPOOLD                                                
         B     LR10                                                             
LR230    DS    0H                                                               
         MVC   LISTAR,P                                                         
         GOTO1 LISTMON                                                          
         B     LR10                                                             
*                                                                               
LRX      DS    0H                                                               
         B     EQXIT                                                            
         DROP  R3,R6                                                            
*                                                                               
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *               
* FULL IS EXPECTED TO ADDRESS PRODUCT NAME OUTPUT AREA WITH                     
* FIRST BYTE CONTAINING THE LENGTH                                              
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *               
GETPNAM  NTR1                                                                   
*                                                                               
         MVC   SAVEA(4),FULL                                                    
         MVC   KEY1,KEY                                                         
         XC    KEY,KEY                                                          
         MVC   KEY+1(6),KEY1+2  COPY A/M,CLT,PRD FROM PGEST TO PROD KEY         
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   GETPN10                                                          
*                                                                               
         SR    R3,R3                                                            
         IC    R3,SAVEA                                                         
         BCTR  R3,0                                                             
         L     R2,SAVEA                                                         
         L     R5,AIO                                                           
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC                                                           
         L     R1,AIO                                                           
         USING PRDHDR,R1                                                        
         EXMVC R3,0(R2),PNAME                                                   
         ST    R5,AIO                                                           
         DROP  R1                                                               
*                                                                               
GETPN10  DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(13),KEY1                                                     
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
*                                                                               
         B     EQXIT                                                            
*                                                                               
SETUP    NTR1                                                                   
         OI    GENSTAT4,CONFDEL                                                 
         B     XIT                                                              
*                                                                               
*                                                                               
***********************************************************************         
*        HEADLINE SPECS                                                         
***********************************************************************         
HEDSPECS DS    0H                                                               
         SSPEC H1,1,REQUESTOR                                                   
         SSPEC H1,30,C'SPOT GF ESTIMATES'                                       
*                                                                               
         SSPEC H2,1,C'MEDIA:'                                                   
         SSPEC H2,30,C'---- -- ---------'                                       
         SSPEC H2,71,PAGE                                                       
*                                                                               
         SSPEC H3,1,C'CLIENT:'                                                  
         SSPEC H3,59,REPORT                                                     
*                                                                               
         SSPEC H4,59,RUN                                                        
*                                                                               
         DC    X'00'                                                            
*                                                                               
*                                                                               
HDHOOK   NTR1                                                                   
         L     R3,AIO                                                           
         USING PGKEY,R3                                                         
*                                                                               
         MVC   BYTE,PGKAM            ISOLATE MEDIA CODE                         
         NI    BYTE,X'0F'                                                       
         LA    R5,MEDTAB             FIND MEDIA CODE USING MEDIA TABLE          
HD10     CLC   BYTE,1(R5)                                                       
         BE    HD20                                                             
         LA    R5,MEDTABLQ(R5)                                                  
         CLI   0(R5),X'FF'                                                      
         BNE   HD10                                                             
HD20     MVC   H2+10(1),0(R5)         COPY MEDIA CODE TO REPORT                 
*                                                                               
         GOTO1 CLUNPK,DMCB,PGKCLT,H3+10                                         
*                                                                               
         TM    FILTFLAG,FPRODQ           PRODUCT FILTER?                        
         BNO   HD25                                                             
         MVC   H4(10),=CL10'Product:'                                           
         MVC   H4+10(3),PGKPRD                                                  
*                                                                               
HD25     DS    0H                                                               
* FETCH AGENCY NAME AND ADDRESS FROM AGENCY RECORD                              
         MVC   KEY1,KEY                                                         
         XC    KEY,KEY                                                          
         MVI   KEY,X'06'           AGENCY RECORD TYPE                           
         MVC   KEY+1(2),PGKALAG                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   HD30                                                             
*                                                                               
         L     R5,AIO                                                           
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC                                                           
         L     R1,AIO                                                           
         USING AGYHDR,R1                                                        
         MVC   H1+58(33),AGYNAME                                                
         MVC   H2+58(33),AGYADDR                                                
         DROP  R1                                                               
*                                                                               
HD30     DS    0H                                                               
         ST    R5,AIO                                                           
         XC    KEY,KEY                                                          
         MVC   KEY(13),KEY1                                                     
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
* PRINT HEADLINES HERE                                                          
*                                                                               
         LA    R1,H6                                                            
         LA    R2,H7                                                            
         LA    R3,H8                                                            
*                                                                               
         TM    FILTFLAG,FPRODQ                                                  
         BO    HD40                                                             
*                                                                               
         USING PLINED,R2                                                        
         MVC   PPRD,=CL3'Prd'                                                   
         MVC   PPRDE,=CL10'PRD DESC'                                            
         DROP  R2                                                               
         USING PLINED,R3                                                        
         MVC   PPRD,=3C'-'                                                      
         MVC   PPRDE,=10C'-'                                                    
         DROP  R3                                                               
*                                                                               
         LA    R1,PLINEDLQ(R1)                                                  
         LA    R2,PLINEDLQ(R2)                                                  
         LA    R3,PLINEDLQ(R3)                                                  
*                                                                               
HD40     DS    0H                                                               
*                                                                               
         USING LINED,R1                                                         
         DROP  R1                                                               
*                                                                               
         USING LINED,R2                                                         
         MVC   LEST,=CL3'Est'                                                   
         MVC   LBAGY,=CL8'Bill Agy'                                             
         MVC   LEXPT,=CL6'Expenc'                                               
         MVC   LPRID,=CL10'Product ID'                                          
         MVC   LCAGY,=CL8'Creative'                                             
         MVC   LTMKT,=CL6'Tgt Mk'                                               
*                                                                               
         MVC   LDBC,=CL3'DBC'                                                   
         MVC   LPRC,=CL4'PrC'                                                   
         MVC   LGFN,=CL3'GFN'                                                   
         MVC   LGFS,=CL3'GFS'                                                   
         MVC   LSAGY,=CL8'Src Agy'                                              
         MVC   LREQN,=CL20'Request Number'                                      
         MVC   LDEAL,=CL10'Deal No'                                             
         DROP  R2                                                               
*                                                                               
         USING LINED,R3                                                         
         MVC   LEST,=3C'-'                                                      
         MVC   LBAGY,=8C'-'                                                     
         MVC   LEXPT,=6C'-'                                                     
         MVC   LPRID,=10C'-'                                                    
         MVC   LCAGY,=8C'-'                                                     
         MVC   LTMKT,=6C'-'                                                     
*                                                                               
         MVC   LDBC,=3C'-'                                                      
         MVC   LPRC,=4C'-'                                                      
         MVC   LGFN,=3C'-'                                                      
         MVC   LGFS,=3C'-'                                                      
         MVC   LSAGY,=8C'-'                                                     
         MVC   LREQN,=20C'-'                                                    
         MVC   LDEAL,=10C'-'                                                    
         DROP  R3                                                               
*                                                                               
         J     EQXIT                                                            
*                                                                               
***********************************************************************         
*                          ERROR MESSAGES                             *         
***********************************************************************         
*                                                                               
ERRINV   MVI   ERROR,INVALID                                                    
         B     VSFMERR                                                          
ERRMIS   MVI   ERROR,MISSING                                                    
         B     VSFMERR                                                          
ERRSEC2  MVC   ERRNUM,=AL2(NOTAUTH)                                             
         B     SPERREX                                                          
MSGERR   MVI   ERROR,0                                                          
         GOTO1 ERREX2                                                           
*                                                                               
SPERREX  OI    GENSTAT2,USGETTXT                                                
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVC   GTMSGNO,ERRNUM                                                   
         MVI   GTMTYP,GTMERR                                                    
         MVI   GTMSYS,2                                                         
VSFMERR  MVC   AIO,AIO1                                                         
         GOTO1 ERREX                                                            
         DROP  RF                                                               
         SPACE 2                                                                
*                                  SHORT DESP OF ERROR MSGS                     
NOTAUTH  EQU   175                 NOT AUTHORIZED FOR THIS FUNCTION             
ESTERR1  EQU   563                 ESTIMATE CODE MUST BE NUMERIC                
ESTERR2  EQU   564                 ESTIMATE CODE BETWEEN 1 - 255                
DELERR   EQU   997                 press enter to confirm delete                
*                                                                               
         SPACE 2                                                                
         GETEL R6,DATADISP,ELCODE                                               
         LTORG                                                                  
***********************************************************************         
*        TABLES, CONSTANTS                                            *         
***********************************************************************         
MEDTAB   DS    0X                                                               
         DC    CL1'T',XL1'01'                                                   
MEDTABLQ EQU   *-MEDTAB                                                         
         DC    CL1'R',XL1'02'                                                   
         DC    CL1'N',XL1'03'                                                   
         DC    CL1'X',XL1'04'                                                   
         DC    CL1'C',XL1'08'                                                   
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        DSECTS                                                       *         
***********************************************************************         
LINED    DSECT                                                                  
LEST     DS    CL3                                                              
         DS    CL1                                                              
LBAGY    DS    CL8                                                              
         DS    CL1                                                              
LEXPT    DS    CL6                                                              
         DS    CL1                                                              
LPRID    DS    CL10                                                             
         DS    CL1                                                              
LCAGY    DS    CL8                                                              
         DS    CL1                                                              
LTMKT    DS    CL6                 Actual field length = 1                      
         DS    CL1                                                              
*                                                                               
LDBC     DS    CL3                 !NOTE! length 1 greater than needed          
         DS    CL1                                                              
LPRC     DS    CL4                                                              
         DS    CL1                                                              
LGFN     DS    CL3                                                              
         DS    CL1                                                              
LGFS     DS    CL3                                                              
         DS    CL1                                                              
LSAGY    DS    CL8                                                              
         DS    CL1                                                              
LREQN    DS    CL20                                                             
         DS    CL1                                                              
LDEAL    DS    CL10                                                             
LINEDLQ  EQU   *-LINED                                                          
*                                                                               
PLINED   DSECT                                                                  
PPRD     DS    CL3                                                              
         DS    CL2                                                              
PPRDE    DS    CL10                                                             
         DS    CL2                                                              
PLINEDLQ EQU   *-PLINED                                                         
*                                                                               
CLTRECD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
ADVHDRD  DSECT                                                                  
       ++INCLUDE SPGENADV                                                       
         EJECT                                                                  
       ++INCLUDE SPSFMWORKD                                                     
         EJECT                                                                  
***********************************************************************         
*        SAVED STORAGE DSECT                                          *         
***********************************************************************         
*                                                                               
         ORG   SYSSPARE                                                         
RELO     DS    A                   RELOCATION FACTOR                            
SAVEA    DS    A                   ADDRESS LOCATION                             
*                                                                               
SVGFEKEY DS    CL48                BACK UP KEY                                  
*                                                                               
ERRNUM   DS    XL2                                                              
*                                                                               
*                                                                               
KEY1     DS    CL48                                                             
*                                                                               
FILTFLAG DS    X                                                                
FPRODQ   EQU   X'01'                                                            
FESTQ    EQU   X'02'                                                            
*                                                                               
         EJECT                                                                  
***********************************************************************         
       ++INCLUDE DDFLDIND                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
*                                                                               
       ++INCLUDE SPSFMFFD                                                       
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE SCSFM44D          MAINTENACE SCREEN                            
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE SCSFM45D          LIST SCREEN                                  
*                                                                               
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE FAFACTS           FOR FACTSD IN VALACC                         
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY          AGENCY PROFILES                              
       ++INCLUDE FAGETTXTD         ERROR MSGS                                   
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE SPGENPGEST                                                     
       ++INCLUDE SPGENPRD                                                       
*                                                                               
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'099SPSFM68   05/18/05'                                      
         END                                                                    
