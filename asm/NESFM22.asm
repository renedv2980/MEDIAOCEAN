*          DATA SET NESFM22    AT LEVEL 028 AS OF 10/31/05                      
*PHASE T31C22A                                                                  
         TITLE 'T31C22  GF ESTIMATE RECORDS'                                    
T31C22   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*GFEST*                                                        
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    LR                                                               
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
* VALIDATE KEY                                                                  
*                                                                               
VK       LA    R6,SVKEY                                                         
         USING PGKEY,R6                                                         
         XC    SVKEY,SVKEY                                                      
         MVI   PGKRID,PGKNDIRQ                                                  
         MVI   PGKSID,PGKNDISQ                                                  
         MVI   NOPTFLG,0                                                        
*                                                                               
         LA    R2,GFFMEDH          MEDIA                                        
         GOTO1 VALIMED                                                          
         MVC   PGKAM,BAGYMD                                                     
*                                                                               
         CLI   ACTNUM,ACTLIST      IF LIST                                      
         BNE   *+8                                                              
         MVI   NOPTFLG,1           FIELDS ARE OPTIONAL                          
*                                                                               
         LA    R2,GFFCLIH          CLIENT                                       
         GOTO1 VALIFLD                                                          
         BZ    VK10                                                             
         GOTO1 VALICLT                                                          
         MVC   PGKCLT,BCLT                                                      
         MVC   GFFCLNM,CLTNM                                                    
         OI    GFFCLNMH+6,X'80'                                                 
*                                                                               
VK10     LA    R2,GFFPROH          PRODUCT                                      
         GOTO1 VALIFLD                                                          
         BZ    VK20                                                             
         GOTO1 VALIPRD                                                          
         MVC   PGKNETP,QPRD                                                     
         MVC   GFFPRNM,PRDNM                                                    
         OI    GFFPRNMH+6,X'80'                                                 
*                                                                               
VK20     LA    R2,GFFESTH          ESTIMATE                                     
         GOTO1 VALIFLD                                                          
         BZ    VK50                                                             
         CLI   NFLD,C'0'            IF EST=0                                    
         BNE   VK25                                                             
         CLC   GFFPRO,=C'POL'       CAN NOT BE POL                              
         BE    VK25                                                             
         MVI   PGKNETE,0                                                        
         MVI   ESTSV,0                                                          
         B     VK50                                                             
VK25     GOTO1 VALIEST              IF EST NOT = 0                              
         CLC   GFFPRO,=C'POL'       MUST BE POL                                 
         BNE   INVERR                                                           
         MVC   PGKNETE,BEST                                                     
         MVC   ESTSV,BEST                                                       
*                                                                               
*                                                                               
VK50     MVC   KEY(13),SVKEY       SET KEY                                      
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
* DISPLAY KEY                                                                   
*                                                                               
DK       L     R6,AIO                                                           
         USING PGKEY,R6                                                         
         MVC   GFLMED,QMED                                                      
         GOTO1 CLUNPK,DMCB,PGKCLT,GFLCLI                                        
         OI    GFLCLIH+6,X'80'                                                  
         MVC   QPRD,PGKNETP                                                     
         MVC   GFLPRO,PGKNETP                                                   
         OI    GFLPROH+6,X'80'                                                  
         ZIC   RE,PGKNETE                                                       
         EDIT  (RE),(3,GFLEST),ALIGN=LEFT,ZERO=NOBLANK                          
         OI    GFLESTH+6,X'80'                                                  
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
* VALIDATE RECORD                                                               
*                                                                               
VR       DS    0H                                                               
         MVI   ELCODE,X'40'                                                     
         GOTO1 REMELEM                                                          
         XC    ELEM,ELEM           BUILD TYPE ELEMENT                           
         LA    R6,ELEM                                                          
         USING GFNETELM,R6                                                      
         MVI   GFNETID,GFNETIDQ                                                 
         MVI   GFNETELN,GFNETLNQ                                                
*                                                                               
         CLC   GFFPRO,=C'POL'      IF POL                                       
         BNE   *+8                                                              
         MVI   NOPTFLG,1           SET OPTIONAL FLAG                            
*                                                                               
         LA    R2,GFFDIVH          DIVISION                                     
         GOTO1 VALIFLD                                                          
         LTR   R1,R1                                                            
         BZ    VR10                                                             
         CLC   GFFPRO,=C'POL'      IF DIVISION INPUT                            
         BE    INVERR              CAN NOT BE POL                               
         MVC   GFNETDIV,NFLD                                                    
*                                                                               
VR10     LA    R2,GFFPRDH          GF PRODUCT                                   
         GOTO1 VALIFLD                                                          
         LTR   R1,R1                                                            
         BZ    VR20                                                             
         CLC   GFFPRO,=C'POL'      IF PRODUCT INPUT                             
         BE    INVERR              CAN NOT BE POL                               
         MVC   GFNETPRD,NFLD       SET PRODUCT                                  
         LA    R2,GFFNATH          AND CLEAR REST OF SCREEN                     
         XC    GFFNAT,GFFNAT                                                    
         FOUT  (R2)                                                             
         LA    R2,GFFSUBH                                                       
         XC    GFFSUB,GFFSUB                                                    
         FOUT  (R2)                                                             
         B     VR50                                                             
*                                                                               
VR20     MVI   NOPTFLG,0                                                        
         LA    R2,GFFNATH          NATURAL                                      
         GOTO1 VALIFLD                                                          
         CLC   GFFPRO,=C'POL'      MUST BE POL                                  
         BNE   INVERR                                                           
         MVC   GFNETNAT,NFLD                                                    
*                                                                               
         LA    R2,GFFSUBH          SUB-NATURAL                                  
         GOTO1 VALIFLD                                                          
         CLC   GFFPRO,=C'POL'      MUST BE POL                                  
         BNE   INVERR                                                           
         MVC   GFNETSUB,NFLD                                                    
*                                                                               
VR50     MVI   NOPTFLG,1           OPTIONAL                                     
         LA    R2,GFFASSH          ASSIGNED OPTION                              
         GOTO1 VALIFLD                                                          
         BZ    VR100                                                            
         CLI   NFLD,C'Y'                                                        
         BE    VR52                                                             
         CLI   NFLD,C'N'                                                        
         BNE   INVERR                                                           
VR52     MVC   GFNETASS,NFLD                                                    
*                                                                               
VR100    GOTO1 ADDELEM                                                          
*                                                                               
VRX      B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
* DISPLAY RECORD                                                                
*                                                                               
DR       LA    R2,GFFDIVH                                                       
         BAS   RE,CLRSCRN          CLEAR SCREEN                                 
         L     R6,AIO                                                           
         MVI   ELCODE,X'40'                                                     
         USING GFNETELM,R6                                                      
         BAS   RE,GETEL                                                         
         BNE   DRX                                                              
         MVC   GFFDIV,GFNETDIV     DIVISION                                     
         FOUT  (R2)                                                             
*                                                                               
         LA    R2,GFFPRDH                                                       
         MVC   GFFPRD,GFNETPRD     GF PRODUCT                                   
         FOUT  (R2)                                                             
*                                                                               
         LA    R2,GFFNATH                                                       
         MVC   GFFNAT,GFNETNAT     NATURAL                                      
         FOUT  (R2)                                                             
*                                                                               
         LA    R2,GFFSUBH                                                       
         MVC   GFFSUB,GFNETSUB     SUB-NATURAL                                  
         FOUT  (R2)                                                             
*                                                                               
         LA    R2,GFFASSH                                                       
         MVC   GFFASS,GFNETASS                                                  
         FOUT  (R2)                                                             
*                                                                               
*                                                                               
DRX      B     EXIT                                                             
         EJECT                                                                  
* LIST RECORDS                                                                  
*                                                                               
LR       MVI   NLISTS,13           SET NUM OF LIST LINES                        
         CLI   MODE,PRINTREP                                                    
         BNE   LR1                                                              
         LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
         LA    R1,HDRTN                                                         
         ST    R1,HEADHOOK                                                      
LR1      OC    KEY(13),KEY                                                      
         BNZ   LR2                                                              
         MVC   KEY(13),SVKEY                                                    
*                                                                               
LR2      GOTO1 HIGH                                                             
         B     LR6                                                              
*                                                                               
LR4      GOTO1 SEQ                                                              
*                                                                               
LR6      DS    0H                                                               
         CLC   KEY(3),KEYSAVE                                                   
         BNE   LRX                                                              
         OC    SVKEY+3(2),SVKEY+3  CLIENT                                       
         BZ    *+14                                                             
         CLC   SVKEY+3(2),KEY+3                                                 
         BNE   LR4                                                              
         OC    SVKEY+5(1),SVKEY+5  ESTIMATE                                     
         BZ    *+14                                                             
         CLC   SVKEY+5(1),KEY+5                                                 
         BNE   LR4                                                              
         OC    SVKEY+6(3),SVKEY+6  PRODUCT                                      
         BZ    *+14                                                             
         CLC   SVKEY+6(3),KEY+6                                                 
         BNE   LR4                                                              
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         MVC   LISTAR,SPACES                                                    
         LA    R4,LISTAR                                                        
         USING LINED,R4                                                         
         L     R6,AIO                                                           
         USING PGKEY,R6                                                         
         GOTO1 CLUNPK,DMCB,PGKCLT,LLCLT                                         
         EDIT  PGKNETE,(3,LLEST),FILL=0                                         
         MVC   LLPRD,PGKNETP                                                    
         MVI   ELCODE,X'40'                                                     
         BAS   RE,GETEL                                                         
         BNE   LR4                                                              
         USING GFNETELM,R6                                                      
         MVC   LLDIV,GFNETDIV                                                   
         MVC   LLPRO,GFNETPRD                                                   
         MVC   LLNAT,GFNETNAT                                                   
         MVC   LLSUB,GFNETSUB                                                   
         MVC   LLASS,GFNETASS                                                   
         CLI   MODE,PRINTREP                                                    
         BE    LR20                                                             
LR10     GOTO1 LISTMON                                                          
         B     LR4                                                              
*                                                                               
LR20     DS    0H                                                               
         MVC   P+10(LINELNQ),LINED                                              
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     LR4                                                              
*                                                                               
LRX      B     EXIT                                                             
         DROP  R4,R6                                                            
         EJECT                                                                  
HEADING  DS    0H                                                               
         SSPEC H1,3,REQUESTOR                                                   
         SSPEC H1,46,C'GENERAL FOODS TAPE'                                      
         SSPEC H2,46,C'------------------'                                      
         SSPEC H1,93,AGYNAME                                                    
         SSPEC H2,93,AGYADD                                                     
         SSPEC H3,93,REPORT                                                     
         SSPEC H4,93,RUN                                                        
         SSPEC H5,103,PAGE                                                      
         DC    X'00'                                                            
*                                                                               
HDRTN    NTR1                                                                   
         LA    R2,H8+10                                                         
         USING LINED,R2                                                         
         MVC   LLCLT,=C'CLT'                                                    
         MVC   LLPRD,=C'PRD'                                                    
         MVC   LLEST,=C'EST'                                                    
         MVC   LLDIV(3),=C'DIV'                                                 
         MVC   LLDIV+4(5),=C'GFPRD'                                             
         MVC   LLNAT,=C'NAT'                                                    
         MVC   LLSUB,=C'SUB'                                                    
         MVC   LLASS(3),=C'ASG'                                                 
         L     R3,ABOX                                                          
         LTR   R3,R3                                                            
         BZ    HDX                                                              
         USING BOXD,R3                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
         MVC   BOXROWS,SPACES                                                   
         LA    R4,BOXROWS                                                       
         LA    R4,5(R4)                                                         
         MVI   0(R4),C'T'                                                       
         LA    R4,2(R4)                                                         
         MVI   0(R4),C'M'                                                       
         LA    R4,65(R4)                                                        
         MVI   0(R4),C'B'                                                       
         LA    R4,BOXCOLS                                                       
         MVC   BOXCOLS,SPACES                                                   
         MVI   8(R4),C'L'                                                       
         MVI   21(R4),C'C'                                                      
         LA    R4,BOXCOLS                                                       
         LA    R4,LINELNQ(R4)                                                   
         MVI   0(R4),C'R'                                                       
HDX      B     EXIT                                                             
         DROP  R2,R3                                                            
         EJECT                                                                  
*                                                                               
* ROUTINE TO CLEAR THE SCREEN                                                   
* FROM FIELD AT R2                                                              
*                                                                               
CLRSCRN  NTR1                                                                   
         SR    RE,RE                                                            
*                                                                               
CS2      IC    RE,0(R2)                                                         
         SH    RE,=H'9'                                                         
         TM    1(R2),X'20'         SKIP PROTECTED FIELDS                        
         BO    CS4                                                              
         EX    RE,CSCLC                                                         
         BE    CS4                                                              
         EX    RE,CSOC                                                          
         BZ    CS4                                                              
         EX    RE,CSXC                                                          
         OI    6(R2),X'80'                                                      
*                                                                               
CS4      LA    R2,9(RE,R2)                                                      
         CLI   0(R2),9                                                          
         BH    CS2                                                              
         B     EXIT                                                             
*                                                                               
CSCLC    CLC   8(0,R2),SPACES                                                   
CSOC     OC    8(0,R2),8(R2)                                                    
CSXC     XC    8(0,R2),8(R2)                                                    
         EJECT                                                                  
* ERROR EXITS                                                                   
*                                                                               
MISSERR  MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
PRDERR   MVI   ERROR,INVPROD                                                    
         B     TRAPERR                                                          
*                                                                               
INVERR   MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
*                                                                               
MSGERR   MVI   ERROR,0                                                          
         GOTO1 ERREX2                                                           
         SPACE 2                                                                
         GETEL (R6),DATADISP,ELCODE                                             
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDFLDIND                                                       
       ++INCLUDE DDBIGBOX                                                       
         EJECT                                                                  
       ++INCLUDE NESFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
         PRINT ON                                                               
       ++INCLUDE NESFMD6D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE NESFMD7D                                                       
         EJECT                                                                  
       ++INCLUDE NESFMWORKD                                                     
         PRINT ON                                                               
         SPACE 1                                                                
         ORG   SYSSPARE                                                         
MYWORK   DS    0CL1          ***** MY WORK AREA *****                           
MYKEY    DS    CL20                                                             
ESTSV    DS    CL1                                                              
         EJECT                                                                  
LINED    DSECT                     LIST LINE DSECT                              
LLCLT    DS    CL3                                                              
         DS    CL1                                                              
LLPRD    DS    CL3                                                              
         DS    CL1                                                              
LLEST    DS    CL3                                                              
         DS    CL4                                                              
LLDIV    DS    CL2                                                              
         DS    CL3                                                              
LLPRO    DS    CL4                                                              
         DS    CL2                                                              
LLNAT    DS    CL3                                                              
         DS    CL2                                                              
LLSUB    DS    CL3                                                              
         DS    CL2                                                              
LLASS    DS    CL1                                                              
LINELNQ  EQU   *-LLCLT                                                          
         EJECT                                                                  
       ++INCLUDE SPGENPGEST                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'028NESFM22   10/31/05'                                      
         END                                                                    
