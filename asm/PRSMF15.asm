*          DATA SET PRSMF15    AT LEVEL 058 AS OF 05/01/02                      
*PHASE T41C0CA,*                                                                
*INCLUDE SRCHCALL                                                               
*INCLUDE PUBEDIT                                                                
*INCLUDE NUMED                                                                  
*INCLUDE BINSRCH2                                                               
         TITLE 'T41C0C SPACE CU RECORDS'                                        
*                                                                               
*   CHANGE LOG                                                                  
*                                                                               
T41C0C   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T41C0C,RR=R3                                                   
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         ST    R3,RELO                                                          
         L     R7,=A(SUBROUTS)     SET UP ADDRESSABILITY TO SUBROUTINES         
         A     R7,RELO             RELOCATE ADDRESS                             
         USING SUBROUTS,R7         ESTABLISH ADDRESSABILITY                     
*===>                                                                           
         MVI   CONSERVH+6,X'81'    FORCE SRV REQ FIELD MODIFIED                 
*===>                                                                           
         MVI   IPSTAT,0            INIT INPUT STATISTICS                        
         MVI   SAVMSGNO,0          INIT MESSAGE NUMBER SAVEAREA                 
         MVI   ERROR,0             INIT MESSAGE NUMBER                          
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,XRECADD        RE-DISPLAY RECORD                            
         NOP   DR                                                               
         CLI   MODE,XRECPUT        RE-DISPLAY RECORD                            
         NOP   DR                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    PR                                                               
         CLI   MODE,RECDEL         DELETE RECORD                                
         BE    DR                                                               
         CLI   MODE,RECREST        RESTORE RECORD                               
         BE    DR                                                               
         B     XIT                                                              
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
*                                                                               
*     VALIDATE KEY ROUTINE                                                      
*                                                                               
VK       DS    0H                                                               
         CLI   ACTNUM,ACTLIST      LIST                                         
         BE    VKL                                                              
*                                                                               
         CLI   ACTNUM,ACTREP       REPORT                                       
         BE    VKL                                                              
*                                                                               
         LA    R2,FSIMEDH         MEDIA                                         
         GOTO1 VALIMED                                                          
*                                                                               
*                                                                               
VK5      LA    R2,FSIPUBH                                                       
         BAS   RE,MYPUBVAL                                                      
         MVC   FSIPUBN,PUBNM       DISPLAY PUB NAME                             
         OI    FSIPUBNH+6,X'80'                                                 
         MVC   FSIPUBZ,PUBZNM      DISPLAY PUB ZONE                             
         OI    FSIPUBZH+6,X'80'                                                 
*                                                                               
         LA    R2,FSICLTH          CLIENT                                       
         MVC   CLTNM,SPACES                                                     
         MVC   QCLT,=X'FFFFFF'     SET TO DEFAULT                               
         CLI   5(R2),0                                                          
         BE    VK20                                                             
         CLC   =C'ALL',8(R2)                                                    
         BE    VK20                                                             
         GOTO1 VALICLT                                                          
*                                                                               
VK20     MVC   FSICLTN,CLTNM       DISPLAY CLT NAME                             
         OI    FSICLTNH+6,X'80'                                                 
*                                                                               
         XC    KEY,KEY             BUILD KEY                                    
         LA    R4,KEY                                                           
         USING PSCUREC,R4                                                       
         MVC   PSCUKAGY,AGENCY                                                  
         MVC   PSCUKMED,QMED                                                    
         MVI   PSCUKTYP,X'28'                                                   
         MVC   PSCUKCLT,QCLT                                                    
         MVC   PSCUKPUB,BPUB                                                    
         MVC   ORIGKEY,KEY         SAVE THIS KEY (USE WITH LIST)                
         CLC   TWAKEYSV(L'PSCUKEY),KEY   CHECK FOR NEWKEY                       
         BE    *+8                                                              
         MVI   NEWKEY,C'Y'                                                      
         B     VKXIT                                                            
*                                                                               
*        VALIDATE LIST KEY                                                      
*                                                                               
VKL      LA    R2,FSLMEDH          MEDIA                                        
         GOTO1 VALIMED                                                          
*                                                                               
         CLI   FSIMED,C'O'                                                      
         BNE   VKL5                                                             
         MVI   ERROR,INVMED                                                     
         B     ERRX                                                             
*                                                                               
VKL5     LA    R2,FSIPUBH          PUB                                          
         XC    BPUB,BPUB                                                        
         XC    FSIPUBN,FSIPUBN                                                  
         XC    FSIPUBZ,FSIPUBZ                                                  
         OI    FSIPUBNH+6,X'80'                                                 
         OI    FSIPUBZH+6,X'80'                                                 
         CLI   5(R2),0                                                          
         BE    VKL10                                                            
         BAS   RE,MYPUBVAL                                                      
         MVC   FSIPUBN,PUBNM       DISPLAY PUB NAME                             
         OI    FSIPUBNH+6,X'80'                                                 
         MVC   FSIPUBZ,PUBZNM      DISPLAY PUB ZONE                             
         OI    FSIPUBZH+6,X'80'                                                 
*                                                                               
*                                                                               
VKL10    XC    QCLT,QCLT           FOR LISTING CLEAR QCLT                       
         XC    FSICLTN,FSICLTN                                                  
         OI    FSICLTNH+6,X'80'                                                 
         LA    R2,FSLCLTH          CLIENT                                       
         CLI   5(R2),0                                                          
         BE    VKXIT                                                            
         CLC   =C'ALL',8(R2)                                                    
         BNE   *+14                                                             
         MVC   QCLT,=X'FFFFFF'     SET FOR CLIENT 'ALL'                         
         B     VKXIT                                                            
*                                                                               
         GOTO1 VALICLT                                                          
         MVC   FSICLTN,CLTNM                                                    
*                                                                               
VKXIT    B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*        VALIDATE PUB FIELD                                                     
*                                                                               
MYPUBVAL NTR1                                                                   
         LR    R4,R2                                                            
         XC    BPUB(6),BPUB                                                     
         MVI   ALLZE,C'N'          INITIALIZE                                   
         MVI   ERROR,MISSING                                                    
         CLI   5(R2),0                                                          
         BNE   VKPUB5                                                           
         CLI   ACTNUM,ACTREP       SEE IF REPORTING                             
         BE    VKPX                                                             
         B     ERRX                IF NOT THE MUST HAVE PUB                     
*                                                                               
VKPUB5   DS    0H                                                               
         CLI   5(R2),3                                                          
         BNE   VKPUB7                                                           
         CLC   8(3,R2),=C'ALL'                                                  
         BNE   VKPUB7                                                           
         MVC   BPUB(6),=6X'FF'                                                  
         B     VKPX                                                             
*                                                                               
VKPUB7   CLI   8(R2),C'='          PUB NAME SEARCH                              
         BNE   VKPUB10                                                          
         SR    R2,RA                                                            
         LA    R3,WORK                                                          
         USING DSPARM,R3                                                        
         XC    DSPARM(DSPARML),DSPARM                                           
         MVC   DSMEDCOD,QMED                                                    
         GOTO1 =V(SRCHCALL),DMCB,(3,(R2)),(X'80',(RA)),ACOMFACS,       X        
               ('DSPARML',WORK),(1,=CL8'PUB'),0,RR=RELO                         
         B     VKPUB30                                                          
         DROP  R3                                                               
*                                                                               
VKPUB10  DS    0H                                                               
         LR    R2,R4                                                            
         MVI   ERROR,INVALID                                                    
         XC    SCANBLK,SCANBLK                                                  
         GOTO1 SCANNER,DMCB,(R2),(2,SCANBLK)                                    
         CLI   DMCB+4,0                                                         
         BE    ERRX                                                             
         LA    R1,SCANBLK                                                       
         CLC   =C'ALL',44(R1)                                                   
         BNE   VKPUB30                                                          
         MVI   ALLZE,C'Y'          WANT ONLY ACROSS ALL ZONES AND ED            
         ZIC   R3,0(R1)                                                         
         STC   R3,5(R2)                                                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),12(R1)                                                   
*                                                                               
VKPUB30  LR    R2,R4                                                            
         GOTO1 VALIPUB                                                          
         CLI   ALLZE,C'Y'                                                       
         BNE   VKPX                                                             
         MVC   BPUB+4(2),=X'FFFF'  ALL ZONES/EDTNS                              
         ZIC   R1,5(R2)                                                         
         AH    R1,=H'4'            PUT BACK THE ORIGINAL LEN                    
         STC   R1,5(R2)                                                         
*                                                                               
VKPX     B     XIT                                                              
         EJECT                                                                  
*                                                                               
* DISPLAY KEY                                                                   
*                                                                               
DK       L     R6,AIO                                                           
         USING PSCUREC,R6                                                       
         MVC   FSIMED,PSCUKMED     MEDIA                                        
         OI    FSIMEDH+6,X'80'     TRANSMIT                                     
*                                                                               
         MVI   NEWKEY,C'Y'         FORCE RE-DISPLAY FROM START OF PUB           
*                                                                               
         MVC   FSICLT,PSCUKCLT     CLIENT                                       
         CLC   PSCUKCLT,=X'FFFFFF'                                              
         BNE   DK5                                                              
         MVC   CLTNM,SPACES                                                     
         MVC   FSICLT,=C'ALL'                                                   
         B     DK10                                                             
*                                                                               
DK5      BAS   RE,MYVCLT           READS THE CLIENT                             
*                                                                               
DK10     OI    FSICLTH+6,X'80'                                                  
         MVC   FSICLTN,CLTNM       DISPLAY CLT NAME                             
         OI    FSICLTNH+6,X'80'                                                 
*                                                                               
         CLC   PSCUKPUB,=6X'FF'                                                 
         BNE   DK12                                                             
         MVC   FSIPUB(3),=C'ALL'                                                
         OI    FSIPUBH+6,X'80'                                                  
         XC    FSIPUBN,FSIPUBN                                                  
         OI    FSIPUBNH+6,X'80'                                                 
         XC    FSIPUBZ,FSIPUBZ                                                  
         OI    FSIPUBZH+6,X'80'                                                 
         B     XIT                                                              
*                                                                               
DK12     GOTO1 =V(PUBEDIT),DMCB,(C'0',PSCUKPUB),(0,FSIPUB),RR=RELO              
         OI    FSIPUBH+6,X'80'                                                  
*                                                                               
         MVC   MYPUB,PSCUKPUB                                                   
         BAS   RE,MYVPUB                                                        
         MVC   FSIPUBN,PUBNM       DISPLAY PUB NAME                             
         OI    FSIPUBNH+6,X'80'                                                 
         MVC   FSIPUBZ,PUBZNM      DISPLAY PUB ZONE                             
         OI    FSIPUBZH+6,X'80'                                                 
*                                                                               
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
* VALIDATE RECORD                                                               
*                                                                               
VR       DS    0H                                                               
***********************************************************************         
*                                                                     *         
* VALIDATE DETAIL INPUT VIA LINUP                                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         LA    R0,SVLSVTAB         SET UP FOR MVCL                              
         LA    R1,L'SVLSVTAB                                                    
         LA    RE,LSVTAB                                                        
         LR    RF,R1                                                            
         MVCL  R0,RE               SAVE SAVE COPY OF LINUP SAVE TABLE           
*                                  TO RESTORE AFTER VALIDATION ERRORS           
         GOTO1 =A(LINSET),RR=RELO  LINUP INTERFACE                              
*                                                                               
         MVI   NEWKEY,0            RESET NEWKEY SWITCH                          
*                                                                               
VRX      DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
* DISPLAY RECORD                                                                
*                                                                               
DR       DS    0H                                                               
***********************************************************************         
*                                                                     *         
*  DISPLAY DETAILS VIA LINUP                                          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         GOTO1 =A(LINSET),RR=RELO    INTERFACE WITH LINUP                       
*                                                                               
         MVI   NEWKEY,0            RESET NEWKEY SWITCH                          
*                                                                               
DRX      DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
* LIST RECORDS                                                                  
*                                                                               
LR       DS    0H                                                               
         OI    GLSTSTAT,RETEXTRA                                                
         LA    R6,KEY                                                           
         USING PSCUREC,R6                                                       
         OC    KEY,KEY             TEST FIRST TIME                              
         BNZ   LR030               KEY IS LAST RECORD READ                      
*                                  SO GO CHECK VS. KEYSAVE                      
*                                                                               
         MVC   PSCUKAGY,AGENCY     CREATE KEY - AGENCY                          
         MVC   PSCUKMED,QMED                    MEDIA CODE                      
         MVI   PSCUKTYP,X'28'                  TYPE                             
         CLI   FSLPUBH+5,0                                                      
         BE    *+10                                                             
         MVC   PSCUKPUB,BPUB                    PUB                             
         CLI   FSLCLTH+5,0                                                      
         BE    *+10                                                             
         MVC   PSCUKCLT,QCLT                    CLIENT                          
*                                                                               
LR010    GOTO1 HIGH                                                             
         B     LR030                                                            
*                                                                               
LR020    GOTO1 SEQ                                                              
*                                                                               
LR030    DS    0H                                                               
         CLC   KEY(4),KEYSAVE      TEST FOR ALL DONE                            
         BE    LR035                                                            
         XC    KEY,KEY                                                          
         B     LRX                                                              
*                                                                               
LR035    DS    0H                                                               
         OC    BPUB,BPUB          SEE IF PUB GIVEN                              
         BZ    LR037                                                            
         CLC   PSCUKPUB,BPUB                                                    
         BE    LR037                                                            
         XC    KEY,KEY                                                          
         B     LRX                                                              
*                                                                               
LR037    OC    QCLT,QCLT          SEE IF CLIENT GIVEN                           
         BZ    LR039                                                            
         CLC   PSCUKCLT,QCLT                                                    
         BNE   LR020               MUST GO TO SEQ                               
*                                                                               
LR039    DS    0H                                                               
         GOTO1 GETREC              GET THE COST RECORD                          
         MVC   MYDSKADD,DMDSKADD   SAVE D/A FOR LIST                            
*                                                                               
         L     R6,AIO                                                           
         USING LISTD,R5                                                         
         LA    R5,P1               USE P LINES                                  
         CLI   MODE,PRINTREP                                                    
         BE    *+8                                                              
         LA    R5,LISTAR           OR LIST AREA                                 
         MVC   LISTAR,SPACES                                                    
*                                                                               
         CLC   PSCUKPUB,=6X'FF'                                                 
         BNE   LR39D                                                            
         MVC   LPUB(3),=C'ALL'                                                  
         B     LR39X                                                            
*                                                                               
LR39D    GOTO1 =V(PUBEDIT),DMCB,(C'0',PSCUKPUB),(0,LPUB),RR=RELO                
         MVC   MYPUB,PSCUKPUB                                                   
         BAS   RE,MYVPUB                                                        
         MVC   LPUBN,PUBNM         DISPLAY PUB NAME                             
*                                                                               
LR39X    MVC   LCLT,PSCUKCLT       CLIENT                                       
         CLC   PSCUKCLT,=X'FFFFFF'                                              
         BNE   *+10                                                             
         MVC   LCLT,=C'ALL'                                                     
         CLI   MODE,PRINTREP       SEE IF PRINTING REPORT                       
         BNE   LR085               NO THEN GO TO NEXT RECORD                    
*                                                                               
         USING PSCUEL01,R6                                                      
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BNE   LR080                                                            
         B     LR050                                                            
*                                                                               
LR040    DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BNE   LR080                                                            
*                                                                               
LR050    MVC   LSPACE,PSCUDESC                                                  
         OC    PSCUXCU,PSCUXCU                                                  
         BZ    LR055                                                            
         MVC   LPCU(1),C'0'                                                     
         CLC   PSCUXCU,=X'000001'                                               
         BE    LR055                                                            
         EDIT  PSCUXCU,(7,LXCU),4                                               
*                                                                               
LR055    OC    PSCUPCU,PSCUPCU                                                  
         BZ    LR070                                                            
         MVC   LPCU(1),C'0'                                                     
         CLC   PSCUPCU,=X'000001'                                               
         BE    LR070                                                            
         EDIT  PSCUPCU,(7,LPCU),4                                               
*                                                                               
LR070    DS    0H                                                               
         CLI   MODE,PRINTREP       SEE IF PRINTING REPORT                       
         BE    LR090                                                            
*                                                                               
LR080    DS    0H                  END OF RECORD                                
*                                                                               
         CLI   MODE,PRINTREP       SEE IF PRINTING REPORT                       
         BNE   LR085                                                            
         GOTO1 SPOOL,DMCB,SPOOLD   SKIP A LINE                                  
         LA    R6,KEY              MUST RESET R6 TO KEY                         
         B     LR020               GO TO SEQ READ                               
*                                                                               
LR085    MVC   DMDSKADD,MYDSKADD   RESTORE D/A                                  
         GOTO1 LISTMON             CALL LISTMON                                 
         LA    R6,KEY              MUST RESET R6 TO KEY                         
         B     LR020                                                            
*                                                                               
LR090    DS    0H                                                               
         GOTO1 SPOOL,DMCB,SPOOLD                                                
         B     LR040               GO BACK AND DO NEXT ELEM                     
*                                                                               
LRX      B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
* SET UP THE PUB NUMBER & GET NAME                                              
*                                                                               
MYVPUB   NTR1                                                                   
*                                                                               
         MVC   MYKEY,KEY           SAVE KEY                                     
         MVC   PUBNM,SPACES                                                     
         MVC   PUBZNM,SPACES                                                    
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PUBRECD,R4                                                       
         MVC   PUBKMED,QMED                                                     
         MVC   PUBKPUB(6),MYPUB    MOVE PUB/ZONE/EDTN                           
         CLC   MYPUB+4(2),=X'FFFF'    ALL ZONES/EDTS                            
         BNE   *+10                                                             
         XC    PUBKPUB+4(2),PUBKPUB+4   READ "BASE" PUB                         
         MVC   PUBKAGY,AGENCY                                                   
         MVI   PUBKCOD,X'81'                                                    
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'PUBDIR'                                            
         GOTO1 HIGH                                                             
         XC    FILENAME,FILENAME                                                
*                                                                               
         CLC   KEY(25),KEYSAVE                                                  
         BNE   VPNO                                                             
*                                                                               
         L     R6,AIO2                                                          
         ST    R6,AIO                                                           
         MVC   FILENAME,=CL8'PUBFILE'                                           
         GOTO1 GETREC                                                           
         XC    FILENAME,FILENAME                                                
*                                                                               
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PUBNAMEL,R6                                                      
         MVC   PUBNM,PUBNAME                                                    
         MVC   PUBZNM(3),=C'ALL'                                                
         CLC   MYPUB+4(2),=X'FFFF'   ALL ZONES/EDTS                             
         BE    *+10                                                             
         MVC   PUBZNM,PUBZNAME                                                  
*                                                                               
         MVC   AIO,AIO1                                                         
         MVC   KEY,MYKEY           RESTORE KEY                                  
         GOTO1 HIGH                                                             
         B     XIT                                                              
*                                                                               
VPNO     MVC   AIO,AIO1                                                         
         MVC   KEY,MYKEY           RESTORE KEY                                  
         GOTO1 HIGH                                                             
         LTR   RB,RB                                                            
         B     XIT                                                              
         DROP  R4,R6                                                            
         EJECT                                                                  
*                                                                               
MYVCLT   NTR1                                                                   
*                                                                               
         MVC   MYKEY,KEY           SAVE KEY                                     
         MVC   CLTNM,SPACES                                                     
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PCLTRECD,R4                                                      
         MVC   PCLTKMED,QMED                                                    
         MVC   PCLTKAGY,AGENCY                                                  
         MVI   KEY+3,X'02'                                                      
         MVC   KEY+4(3),FSICLT                                                  
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'PRTDIR'                                            
         GOTO1 HIGH                                                             
         XC    FILENAME,FILENAME                                                
*                                                                               
         CLC   KEY(10),KEYSAVE                                                  
         BNE   VCNO                                                             
*                                                                               
         L     R4,AIO2                                                          
         ST    R4,AIO                                                           
         MVC   FILENAME,=CL8'PRTFILE'                                           
         GOTO1 GETREC                                                           
         XC    FILENAME,FILENAME                                                
         MVC   CLTNM,PCLTNAME                                                   
*                                                                               
         MVC   AIO,AIO1                                                         
         MVC   KEY,MYKEY           RESTORE KEY                                  
         GOTO1 HIGH                                                             
         B     XIT                                                              
*                                                                               
VCNO     MVC   AIO,AIO1                                                         
         MVC   KEY,MYKEY           RESTORE KEY                                  
         GOTO1 HIGH                                                             
         LTR   RB,RB                                                            
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*                                                                               
* PRINT REPORT                                                                  
*                                                                               
PR       L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         LA    R1,HEDSPECS         SET UP HEADHOOK AND SPECS                    
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         B     LR                  USE LIST REC LOGIC                           
*                                                                               
PRX      B     XIT                                                              
         EJECT                                                                  
HOOK     NTR1                      HEADLINE ROUTINES                            
*                                                                               
HOOKX    B     XIT                                                              
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
*   LINUP INTERFACE                                                  *          
*     - BUILD LUBLK, CREATE ELEMENT TABLE, CALL LINUP                *          
*                                                                    *          
**********************************************************************          
         SPACE 2                                                                
         DS    0D                                                               
LINSET   NMOD1 0,**LINSET                                                       
*                                                                               
         LA    RC,SPOOLEND         POINT TO WORKING STORAGE                     
*                                                                               
         LA    R5,LUBLK            POINT TO  LINUP CONTROL BLOCK                
         USING LUBLKD,R5           ESTABLISH LINUP CONTROL BLOCK                
         XC    LUBLKD(LUBLKL),LUBLKD   CLEAR LINUP CONTROL BLOCK                
*                                                                               
         MVC   LUNEW,NEWKEY        SET NEW OR OLD RECORD INDICATOR              
*                                                                               
         MVC   LUATWA,ATWA         PASS A(TWA)                                  
         L     R1,SYSPARMS         GET A(TIOB)                                  
         L     R1,0(R1)                                                         
         ST    R1,LUATIOB                                                       
*                                                                               
         MVI   LUNLINS,NLINS       SET NUMBER OF LINES ON SCREEN                
         MVI   LUNFLDS,3               FIELDS PER LINE                          
*                                                                               
*                                  BUILD LIST OF FIELD DISPLACEMENTS            
*                                                                               
         LA    R3,LINDSPS          POINT TO LIST OF DISPLACEMENTS               
         ST    R3,LUADSPS          A(LIST OF DISPLACEMENTS)                     
*                                                                               
         LA    R2,FSISP1H          A(FIRST FIELD)                               
         ZIC   R4,LUNLINS          NUMBER OF LINES                              
         SRL   R4,1                HALVE SINCE TWO SETS ON A LINE               
*                                                                               
LS04     DS    0H                                                               
*                                                                               
         LA    RF,0(R2)            POINT TO FIRST FIELD ON NEXT LINE            
         S     RF,ATWA             GET DISPLACEMENT                             
         STH   RF,0(R3)            SET DISPLACEMENT IN LIST                     
*                                                                               
         LA    R3,2(R3)            BUMP TO NEXT SLOT IN LIST                    
*                                                                               
         ZIC   R0,LUNFLDS          GET NUMBER OF FIELDS ON LINE                 
         SLL   R0,1                DOUBLE SINCE 2 SETS ON A LINE                
         BAS   RE,BUMP             BUMP TO START OF NEXT LINE                   
         BCT   R0,*-4                BY BUMPING THRU ALL FLDS ON LINE           
*                                                                               
         BCT   R4,LS04             ADD NEXT LINE TO LIST                        
*                                                                               
         LA    R2,FSISP2H          A(FIRST FIELD OF SECOND COLUMN)              
         ZIC   R4,LUNLINS          NUMBER OF LINES                              
         SRL   R4,1                HALVE SINCE TWO SETS ON A LINE               
*                                                                               
LS05     DS    0H                                                               
*                                                                               
         LA    RF,0(R2)            POINT TO FIRST FIELD ON NEXT LINE            
         S     RF,ATWA             GET DISPLACEMENT                             
         STH   RF,0(R3)            SET DISPLACEMENT IN LIST                     
*                                                                               
         LA    R3,2(R3)            BUMP TO NEXT SLOT IN LIST                    
*                                                                               
         ZIC   R0,LUNFLDS          GET NUMBER OF FIELDS ON LINE                 
         SLL   R0,1                DOUBLE SINCE 2 SETS ON A LINE                
         BAS   RE,BUMP             BUMP TO START OF NEXT LINE                   
         BCT   R0,*-4                BY BUMPING THRU ALL FLDS ON LINE           
*                                                                               
         BCT   R4,LS05             ADD NEXT LINE TO LIST                        
*                                                                               
         XC    0(2,R3),0(R3)       FORCE NULLS AT END OF LIST                   
*                                                                               
         MVI   LUSCROLL,LUPAGEQ    SCROLL FACTOR OF A PAGE IS DEFAULT           
*                                                                               
         CLI   FSISCRL,C'P'        CHECK FOR PAGE SCROLL                        
         BE    *+8                                                              
         CLI   FSISCRL,X'97'         LOWERCASE 'P'                              
         BNE   *+8                                                              
         MVI   LUSCROLL,LUPAGEQ                                                 
*                                                                               
         CLI   FSISCRL,C'H'        CHECK FOR HALF PAGE SCROLL                   
         BE    *+8                                                              
         CLI   FSISCRL,X'88'            LOWERCASE 'H'                           
         BNE   *+8                                                              
         MVI   LUSCROLL,LUHALFQ                                                 
*                                                                               
         TM    FSISCRLH+4,X'08'    SKIP IF NOT A NUMERIC FIELD                  
         BNO   LS051                                                            
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,FSISCRLH+5     FIELD INPUT LENGTH                           
         BZ    LS051               NO ENTRY                                     
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB,FSISCRL(0)      CONVERT SCROLL AMOUNT TO NUMBER              
*                                                                               
         CVB   RF,DUB                                                           
         STC   RF,LUSCROLL         PASS SCROLL AMOUNT                           
*                                                                               
LS051    DS    0H                                                               
*                                                                               
         CLI   PFAID,19            CHECK FOR UP KEY                             
         BE    *+8                                                              
         CLI   PFAID,7             CHECK FOR UP KEY                             
         BNE   *+8                                                              
         MVI   LUPFKEY,LUPFUPQ                                                  
*                                                                               
         CLI   PFAID,20            CHECK FOR DOWN KEY                           
         BE    *+8                                                              
         CLI   PFAID,8             CHECK FOR DOWN KEY                           
         BNE   *+8                                                              
         MVI   LUPFKEY,LUPFDNQ                                                  
*                                                                               
*        CLI   LUPFKEY,0           IF VALID PFKEY HIT                           
*        BE    *+8                                                              
*        OI    GENSTAT2,RETEQSEL   RE-DISPLAY SAME SCREEN                       
*                                                                               
         CLI   MODE,VALREC         SET LINUP MODE                               
         BNE   *+8                                                              
         MVI   LUAPMODE,LUAPVALQ   VALIDATE                                     
*                                                                               
         CLI   MODE,DISPREC        SET LINUP MODE - DISPREC                     
         BE    *+8                                                              
         CLI   MODE,XRECADD        RE-DISPLAY AFTER ADD                         
         BE    *+8                                                              
         CLI   MODE,XRECPUT        RE-DISPLAY AFTER CHANGE                      
         BE    *+8                                                              
         CLI   MODE,XRECDEL        RE-DISPLAY AFTER DELETE                      
         BE    *+8                                                              
         CLI   MODE,XRECREST       RE-DISPLAY AFTER RESTORE                     
         BNE   *+8                                                              
         MVI   LUAPMODE,LUAPDSPQ   DISPLAY                                      
*                                                                               
         MVI   LUCNTL,LUBACKQ      WINDOW SUPPORTS BACKWARD SCROLLING           
*                                                                               
         TM    IPSTAT,LUWCURSQ     CHECK IF CURSOR FOUND YET                    
         BNO   *+8                 NO - NO CURSOR SELECT                        
         OI    LUCNTL,LUCURSQ      YES - MAKE CURSOR SENSITIVE                  
*                                                                               
         LA    RF,LINHOOK          PROCESSING ROUTINE                           
         ST    RF,LUHOOK                                                        
*                                                                               
         MVC   LUSVLEN,=AL2(LSVTABL) SAVED BYTES PER LINE                       
*                                                                               
         LA    RF,LSVTAB           LINUP SAVE AREA                              
         ST    RF,LUSVTAB                                                       
*                                                                               
*              BUILD TABLE OF ELEMENTS                                          
*                                                                               
         BAS   RE,LSBLDTAB            BUILD ELEM TABLE                          
*                                                                               
*                                     FIRST LINE UP CALL                        
*                                     ------------------                        
*                                                                               
         GOTO1 VLINUP,DMCB,LUBLKD     LINUP                                     
*                                                                               
         OC    IPSTAT,LUWSTAT      OR IN WINDOW INPUT STATUS                    
*                                                                               
         CLI   LUAPMODE,LUAPVALQ      TEST VALIDATING                           
         BNE   LSMOR                                                            
*                                                                               
         TM    IPSTAT,LUWVERRQ     CONTINUE IF NO ERRORS                        
         BNO   LS22                                                             
*                                  ELSE                                         
         LA    R0,SVLSVTAB         SET UP FOR MVCL                              
         LA    R1,L'SVLSVTAB                                                    
         LA    RE,LSVTAB                                                        
         LR    RF,R1                                                            
         MVCL  RE,R0               RESTORE LINUP SAVE TABLE                     
*                                                                               
         OI    GENSTAT2,RETEQSEL   HAVE GENCON RETURN THIS SCREEN               
*                                                                               
         B     LSX                 AND DON'T UPDATE ELEMS                       
*                                                                               
LS22     DS    0H                                                               
*                                                                               
         TM    LUWSTAT,LUSNPVQ     UPDATE RECORD IF THERE WAS AT LEAST          
         BNO   *+8                 ONE NON-PREVIOUSLY VALIDATED FIELD           
         TM    LUWSTAT,LUSDATQ     AND DATA ENTERED IN SOME FIELD               
         BNO   *+8                                                              
         BAS   RE,LSWRTTAB         WRITES CHANGES TO RECORD                     
*                                                                               
         TM    IPSTAT,LUSNPVQ      IF ALL PREVIOUSLY VALIDATED                  
         BO    LSNCHA                                                           
         CLI   ACTNUM,ACTCHA       AND ACTION IS CHANGE                         
         BNE   LSNCHA              THEN WANT TO RE-DISPLAY IN CASE OF           
*                                    NEED TO SCROLL                             
         TM    LUSTAT,LUCLEARQ     SKIP IF CLEAR COMMAND ISSUED                 
         BO    LSNCHA                                                           
*                                    NEED TO SCROLL                             
         MVI   LUAPMODE,LUAPDSPQ   SET FOR DISPLAY                              
         MVI   MODE,DISPREC        SET FOR DISPLAY RECORD                       
         MVI   LUWSTAT,0           RESET WINDOW STAT                            
*                                                                               
         GOTO1 VLINUP,DMCB,LUBLKD SCROLL IF NEEDED                              
*                                                                               
LSNCHA   DS    0X                                                               
*                                                                               
LSMOR    DS    0X                  SET 'MORE' FIELDS                            
*                                                                               
         TM    LUSTAT,LUCLEARQ     CLEAR LOWER MORE FIELD IF SCREEN             
         BO    LSLOW                 CLEARED - LEAVE UPPER AS IS                
*                                                                               
         CLI   LUAPMODE,LUAPDSPQ   ONLY IF IN DISPLAY MODE                      
         BNE   LSMORX                                                           
*                                                                               
         MVC   FLD,SPACES          INIT WORK OUTPUT AREA                        
         LA    R1,FSIMOR1H         POINT TO FIRST MORE FIELD                    
*                                                                               
         L     R3,LUSVTAB          POINT TO FIRST SAVED ENTRY                   
         USING LSVTABD,R3          ESTABLISH ENTRY                              
*                                                                               
         L     R4,BSPATAB          POINT TO FIRST TABLE ENTRY                   
         USING ELTABD,R4           ESTABLISH ENTRY                              
*                                                                               
         CLC   LSVKEY,ELTKEY       IF SCREEN DOES NOT START AT START            
         BNH   *+10                OF TABLE THEN SET UP INDICATOR               
         MVC   FLD(2),=C'<<'                                                    
*                                                                               
         BAS   RE,DSPFLD           DISPLAY IT                                   
*                                                                               
LSLOW    DS    0H                                                               
*                                                                               
         MVC   FLD,SPACES          INIT WORK OUTPUT AREA                        
         LA    R1,FSIMORLH         POINT TO LAST MORE FIELD                     
*                                                                               
         TM    LUSTAT,LUCLEARQ     CLEAR LOWER MORE FIELD IF SCREEN             
         BO    LSLOWOUT              CLEARED                                    
*                                                                               
         ZIC   RF,LUNLINS          GET NUMBER OF LINES ON SCREEN                
         BCTR  RF,0                DECREMENT FOR INDEXING                       
         MH    RF,=Y(LSVTABL)      GET INDEX                                    
         AR    R3,RF               POINT TO LAST ELEMENT IN TABLE               
         L     R4,ELTLAST          POINT TO LAST ELEMENT IN TABLE               
*                                                                               
         OC    LSVKEY,LSVKEY       NULLS INDICATE END OF TABLE ALREADY          
         BZ    LSLOWOUT            ON SCREEN                                    
*                                                                               
         CLC   LSVKEY,ELTKEY       IF SCREEN DOES NOT END AT END                
         BNL   *+10                OF TABLE THEN SET DOWN INDICATOR             
         MVC   FLD(2),=C'>>'                                                    
*                                                                               
LSLOWOUT DS    0H                                                               
*                                                                               
         BAS   RE,DSPFLD           DISPLAY IT                                   
*                                                                               
LSMORX   DS    0X                                                               
*                                                                               
LSX      DS    0H                                                               
*                                                                               
         CLI   LUERR,0             RESET CC                                     
*                                                                               
         XIT1                                                                   
         SPACE 2                                                                
*                                  LINES IN WINDOW                              
*NLINS    EQU   6                                                               
NLINS    EQU   ((FSISPLH-FSISP1H)/(FSISP2H-FSISP1H))+1                          
         DROP  R3                                                               
         DROP  R4                                                               
         EJECT                                                                  
**********************************************************************          
*                                                                    *          
*   LINHOOK - LINUP PROCESSING HOOK                                  *          
*                                                                    *          
**********************************************************************          
         SPACE 2                                                                
LINHOOK  NTR1                                                                   
         CLI   LUMODE,LUVALQ       VALIDATE                                     
         BE    LHVAL                                                            
         CLI   LUMODE,LUDSPQ       DISPLAY                                      
         BE    LHDIS                                                            
         CLI   LUMODE,LUMOREQ      MORE TO DISPLAY                              
         BE    LHMORE                                                           
         DC    H'0'                INVALID MODE                                 
         EJECT                                                                  
**********************************************************************          
*   LINHOOK - LINUP VALIDATION HOOK ROUTINE                          *          
**********************************************************************          
         SPACE 2                                                                
LHVAL    DS    0H                                                               
*                                                                               
*              IF FIRST INPUT FIELD HAS '++' THEN USER WANTS TO CLEAR           
*              OUT WINDOW FROM THIS POINT ON                                    
*                                                                               
         L     R2,LUACLIN          TEST FOR SCREEN CLEAR REQUEST                
*                                                                               
         CLC   8(2,R2),=C'++'      CHECK FOR CLEARING INDICATOR                 
         BNE   LHV04                                                            
*                                                                               
         NI    LULSTAT,X'FF'-LUSNPVQ  TURN OFF NOT VALIDATED INDICATOR          
         OI    LUSTAT,LUCLEARQ     TELL LINUP TO CLEAR FROM HERE ON             
*                                                                               
         OC    ACURFORC,ACURFORC   SET CURSOR IF IT IS NOT SET ALREADY          
         BNZ   *+8                                                              
         ST    R2,ACURFORC         FORCE CURSOR TO THIS FIELD                   
*                                                                               
         OI    GENSTAT2,RETEQSEL   HAVE GENCON RETURN THIS SCREEN               
*                                                                               
         B     LHVALX                                                           
*                                                                               
LHV04    DS    0H                                                               
*                                                                               
         BAS   RE,LHVALLIN         YES, VALIDATE LINE AND ADD TO TABLE          
         BNE   LHVALX              IF ERROR DONT DELETE OLD AND DONT            
*                                  CLEAR SAVE TABLE ENTRY                       
         EJECT                                                                  
LHV06    DS    0H                                                               
         L     R2,LUACTAB          POINT TO CURRENT SAVED TABLE ENTRY           
         USING LSVTABD,R2          ESTABLISH ENTRY                              
*                                                                               
         OC    LSVKEY,LSVKEY       WAS ANYTHING THERE BEFORE                    
         BZ    LHV10               NO                                           
*                                  YES, MARK OLD ENTRY DELETED                  
*                                  NOTE- ADD+DEL=CHANGE, THIS ENTRY             
*                                        MAY BE SAME AS ABOVE                   
         GOTO1 =V(BINSRCH),BSPPRMS,('BSPFIND',(R2)),RR=RELO                     
         CLI   BSPCNTL,BSPNF       MUST FIND ELEMENT                            
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,BSPAREC          POINT TO FOUND ELEMENT                       
         USING ELTABD,RF           ESTABLISH ELEMENT                            
*                                                                               
         OI    ELTCTL,ELTDELQ      SET ENTRY DELETED                            
         DROP  RF                                                               
*                                                                               
LHV10    DS    0H                  SET NEW SAVE TABLE ENTRY                     
         XC    LSVKEY,LSVKEY       INIT SAVE TABLE ENTRY                        
         ICM   RF,15,ELTENT        GET ADDRESS OF ELEMENT                       
         BZ    *+10                PUT IN TABLE IF FOUND                        
         MVC   LSVKEY,ELTKEY-ELTABD(RF)                                         
*                                                                               
LHVALX   DS    0H                                                               
         B     LHOOKX                                                           
         EJECT                                                                  
**********************************************************************          
*   LINHOOK - LINUP DISPLAY HOOK ROUTINE                             *          
**********************************************************************          
         SPACE 2                                                                
LHDIS    DS    0H                                                               
         MVI   LUSTAT,0            INIT STATUS                                  
         L     R4,LUACTAB          CURRENT SAVE TABLE ENTRY                     
         USING LSVTABD,R4                                                       
         XC    0(LSVTABL,R4),0(R4)  CLEAR IT                                    
*                                                                               
         BAS   RE,LHSRCH           FIND ELEM TO DISPLAY                         
         BAS   RE,LHDISLIN         BUILD SCREEN LINE                            
*                                                                               
LHDISX   DS    0H                                                               
         B     LHOOKX                                                           
         EJECT                                                                  
*                                  DO 'MORE' MESSAGE                            
*                                  -----------------                            
LHMORE   DS    0H                                                               
         B     LHOOKX                                                           
*                                                                               
LHOOKX   DS    0H                                                               
         XIT1                                                                   
         DROP  R2                                                               
         DROP  R4                                                               
         TITLE 'PRSFM0C - BUILD TABLE OF ELEMENTS ON FILE'                      
***********************************************************************         
*                                                                     *         
* ROUTINE TO BUILD ELEMENT TABLE FOR LINUP                            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
LSBLDTAB NTR1                                                                   
         XC    BSPPRMS(BSPPRML),BSPPRMS INIT BINSRCH PARAMETERS                 
         L     R1,AIO2             STORE TABLE IN I/O2 & I/O3                   
         ST    R1,BSPATAB          PASS TABLE ADDRESS                           
         LA    R1,ELTABL           PASS ENTRY LENGTH                            
         ST    R1,BSPLENR                                                       
         LA    R1,ELTKEYL          PASS KEY LENGTH                              
         ST    R1,BSPLENK                                                       
         MVI   BSPKEYD,0           PASS KEY DISPLACEMENT                        
         LA    R1,ELTMAX           PASS MAXIMUM COUNT                           
         ST    R1,BSPMAX                                                        
*                                                                               
         LA    R4,WRKELTAB         POINT TO ELTAB WORK ELEMENT                  
         USING ELTABD,R4           ESTABLISH TABLE ENTRY                        
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'                                                     
         USING PSCUEL01,R6                                                      
         BAS   RE,GETEL                                                         
         B     LSBT20                                                           
*                                                                               
LSBT10   BAS   RE,NEXTEL                                                        
*                                                                               
LSBT20   BNE   LSBTX               NO ELEMENTS LEFT                             
*                                                                               
         XC    ELTABD(ELTABL),ELTABD INIT ENTRY                                 
*                                                                               
         MVC   ELTELEM,PSCUEL01    ADD ELEMENT TO TABLE                         
         MVC   ELTSORT,PSCUDESC    SET KEY                                      
*                                                                               
LSBT30   DS    0H                                                               
*                                                                               
         GOTO1 =V(BINSRCH),BSPPRMS,('BSPADD',WRKELTAB),RR=RELO                  
         OC    1(3,R1),1(R1)       TEST IF ELEMENT FIT INTO TABLE               
         BNZ   *+6                                                              
         DC    H'0'                TOO MANY LINES (SHOULD NOT HAPPEN)           
*                                                                               
         B     LSBT10                                                           
*                                                                               
LSBTX    DS    0H                                                               
*                                                                               
         ICM   R1,15,BSPNOR        NUMBER OF ENTRIES                            
         BZ    *+10                                                             
         BCTR  R1,R0               MINUS ONE                                    
         MH    R1,=Y(ELTABL)       TIMES ENTRY LENGTH                           
         A     R1,BSPATAB          PLUS START OF TABLE                          
         ST    R1,ELTLAST          SET A(LAST)                                  
*                                                                               
         XIT1                                                                   
         DROP  R4                                                               
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
* ROUTINE TO SEARCH TABLE FOR ELEMENT                                 *         
* AND SET ADDRESS IN ELTENT                                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
LHSRCH   NTR1                                                                   
         XC    ELTENT,ELTENT       INIT ELEMENT ADDRESS RETURN                  
         NI    LUSTAT,255-LUEOLQ   SET OFF END OF LIST INDICATOR                
*                                                                               
         OC    BSPNOR,BSPNOR       IF NO ENTRIES                                
         BZ    LHSRCHX             RETURN EMPTY-HANDED                          
*                                                                               
         L     R4,BSPATAB          DEFAULT TO FIRST ENTRY IN TABLE              
         USING ELTABD,R4                                                        
*                                                                               
         L     R3,LUAPTAB          A(PREVIOUS SAVE TABLE)                       
         USING LSVTABD,R3                                                       
*                                                                               
         OC    LSVKEY,LSVKEY       NO PREVIOUS ENTRY MEANS FIRST TIME           
         BZ    LHSRCH11            OR SCROLLING FROM A NON-FILLED               
*                                  SCREEN - USE DEFAULT                         
*                                                                               
         CLC   LSVKEY,HIVALS       IF PREVIOUS IS X'FF'S                        
         BNE   LHSRCH05                                                         
*                                                                               
         L     RE,BSPNOR              USE LAST ENTRY IN TABLE                   
         BCTR  RE,0                   DECREMENT FOR INDEXING                    
         L     RF,BSPLENR             RECORD LENGTH                             
         MR    RE,RE                  INDEX TO LAST ENTRY                       
         LA    R4,0(RF,R4)            A(LAST TABLE ENTRY)                       
         B     LHSRCH10                                                         
*                                                                               
LHSRCH05 DS    0H                                                               
*                                                                               
         GOTO1 =V(BINSRCH),BSPPRMS,('BSPRDHI',LSVKEY),RR=RELO                   
*                                                                               
         L     R4,BSPAREC          GET TABLE ENTRY ADDRESS                      
*                                                                               
         CLI   BSPCNTL,BSPNF       DEFAULT TO FIRST OF TABLE IF                 
         BNE   LHSRCH10            PREVIOUS ENTRY WAS NOT FOUND                 
*                                                                               
         L     R4,BSPATAB          NO, POINT TO FIRST-(END OF TABLE)            
         B     LHSRCH11            DONE (NO MOVEMENT)                           
         EJECT                                                                  
LHSRCH10 DS    0H                                                               
         CLI   LUDIR,C'-'          CHECK FOR BACKWARD SCROLLING                 
         BE    LHSRCH16                                                         
*                                                                               
         CLI   LUDIR,C'='          SKIP BUMPING ENTRY IF RE-DISPLAYING          
         BE    *+8                                                              
         LA    R4,ELTABL(R4)       BUMP TO NEXT ENTRY IN TABLE                  
*                                                                               
LHSRCH11 DS    0H                                                               
*                                                                               
         C     R4,ELTLAST          DONE IF NOT AT END OF TABLE                  
         BL    LHSRCH30                                                         
         BE    LHSRCH12            AT END OF TABLE TELL LINUP                   
*                                  PAST END - ONLY IF SCROLLING DOWN            
*                                  AND PRIOR SCREEN ENDED WITH LAST             
*                                  ELEMENT IN TABLE - USE DEFAULT               
*                                  OR UP SCROLLING AND TABLE HAS ONLY           
*                                  ONE ELEMENT - STOP WITH NO DISPLAY           
         L     R4,BSPATAB          POINT TO FIRST ENTRY IN TABLE                
         CLC   BSPNOR,=F'1'        DONE IF MORE THAN ONE ENTRY IN TABLE         
         BH    LHSRCH30                                                         
*                                  EXIT WITHOUT FINDING ELEMENT                 
         OI    LUSTAT,LUEOLQ       TELL LINUP THIS IS LAST                      
         B     LHSRCHX                                                          
*                                                                               
LHSRCH12 DS    0H                                                               
         OI    LUSTAT,LUEOLQ       TELL LINUP THIS IS LAST                      
         B     LHSRCH30                                                         
         EJECT                                                                  
LHSRCH16 DS    0H                  GOING BACKWARDS                              
         C     R4,BSPATAB          IF AT START                                  
         BNH   LHSRCH18            DONT GO FURTHER                              
         SH    R4,=Y(ELTABL)       BACK UP AN ENTRY                             
LHSRCH17 DS    0H                                                               
         C     R4,BSPATAB          IF AT START                                  
         BH    *+8                                                              
LHSRCH18 DS    0H                                                               
         OI    LUSTAT,LUEOLQ       TELL LINUP THIS IS LAST                      
*                                                                               
LHSRCH30 DS    0H                                                               
         TM    ELTCTL,ELTDELQ+ELTADDQ DISPLAY CHANGED ELEMENTS                  
         BO    LHSRCH40                                                         
         TM    ELTCTL,ELTDELQ      BYPASS DELETED ELEMENTS                      
         BNO   LHSRCH40                                                         
         TM    LUSTAT,LUEOLQ       EXIT IF AT END OF LIST                       
         BO    LHSRCHX                                                          
         CLI   LUDIR,C'='          QUIT IF RE-DISPLAY                           
         BE    LHSRCH40                                                         
         B     LHSRCH10            ELSE GO FIND NEXT ELEMENT                    
*                                                                               
LHSRCH40 DS    0H                                                               
         ST    R4,ELTENT           RETURN TABLE ENTRY ADDRESS                   
         L     R3,LUACTAB          POINT TO CURRENT SAVE TABLE ENTRY            
         MVC   LSVKEY,ELTKEY       SAVE APPROPRIATE DATA                        
*                                                                               
LHSRCHX  DS    0H                                                               
         XIT1                                                                   
         DROP  R3                                                               
         DROP  R4                                                               
HIVALS   DC    32X'FF'             HIGH VALUES                                  
*                                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
* ROUTINE TO VALIDATE WINDOW LINE                                     *         
* BUILD ENTRY IN APWORK AND ADD TO ELEM TABLE                         *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
LHVALLIN NTR1                                                                   
*                                                                               
         XC    ELTENT,ELTENT       CLEAR A(ENTRY)                               
*                                                                               
         TM    LULSTAT,LUSNPVQ     SKIP IF ALL FIELDS PREVIOUSLY                
         BO    *+8                   VALIDATED                                  
         TM    LULSTAT,LUSDATQ     AND NO DATA IN FIELDS ON THIS LINE           
         BZ    LHVLX                                                            
*                                                                               
         XC    WRKELTAB,WRKELTAB   INIT WORK AREA                               
         LA    R4,WRKELTAB         SET TO BUILD TABLE ELEMENT                   
         USING ELTABD,R4           IN WORK AREA                                 
*                                                                               
         LA    R3,ELTELEM          INITIALIZE ELEMENT                           
         USING PSCUEL01,R3         ESTABLISH STANDARD CONTRACT UNIT ELM         
*                                                                               
         SLR   R4,R4               INIT TABLE ELEMENT POINTER                   
*                                                                               
         L     R2,LUACLIN          POINT TO FIRST FIELD ON LINE =SPACE          
*                                                                               
         CLI   5(R2),0             IF NO INPUT IN SPACE FIELD                   
         BNE   LHV30                                                            
         LR    R0,R2               SAVE R2                                      
         BAS   RE,BUMP             & SEE IF ANY INPUT ON THIS LINE              
         CLI   5(R2),0                                                          
         BNE   LHV20                                                            
         BAS   RE,BUMP             ANY INPUT ON THIS LINE                       
         CLI   5(R2),0                                                          
         BE    LHV50                                                            
LHV20    LR    R2,R0               RESET R2 FOR ERROR                           
         B     MISSERR                                                          
*                                                                               
LHV30    DS    0H                                                               
*                                                                               
         CLC   =C'DELETE',8(R2)    DELETE THIS ENTRY                            
         BE    LHVSPOK             IGNORE ANY OTHER INPUT                       
*                                                                               
         LA    R4,WRKELTAB         SET TO BUILD TABLE ELEMENT                   
*                                                                               
         CLI   FSIMED,C'N'                                                      
         BNE   LHV40D                                                           
         CLI   5(R2),8             MAX LENGTH IS 8 FOR NEWSPAPRES               
         BH    INVERR                                                           
         B     LHV40H                                                           
LHV40D   DS    0H                                                               
         CLI   FSIMED,C'O'         SEE IF OUTDOOR                               
         BNE   LHV40H                                                           
         CLC   8(4,R2),=C'SRI='    CHK FOR SPECIAL SRI INPUT                    
         BNE   LHV40H                                                           
         SPACE 2                                                                
CHKOUT   DS    0H                 OUTPUT IS X'FF' FOLLOWED BY                   
*                                 3-3 BYTE PACKED FIELDS                        
         XC    WORK(10),WORK                                                    
         MVI   WORK,X'FF'                                                       
         ZAP   WORK+1(3),=P'99999'                                              
         LA    R6,15(R2)                                                        
         CLC   12(3,R2),=C'SPC'        SPECIAL                                  
         BE    CO4                                                              
         GOTO1 =V(NUMED),DMCB,12(R2),DUB,RR=RELO                                
*                                                                               
         CP    DUB,=P'100'          SHOWING CAN'T EXCEED 100                    
         BH    COERR                                                            
*                                                                               
         ZAP   WORK+1(3),DUB                                                    
         L     R6,DMCB                                                          
         CLI   0(R6),C' '                                                       
         BE    COERR                                                            
CO4      DS    0H                                                               
         GOTO1 =V(NUMED),DMCB,1(R6),DUB,RR=RELO                                 
*                                                                               
         ZAP   WORK+4(3),DUB                                                    
         L     R6,DMCB                                                          
         CLI   0(R6),C' '                                                       
         BE    COERR                                                            
         GOTO1 (RF),(R1),1(R5)                                                  
*                                                                               
         ZAP   WORK+7(3),DUB                                                    
         L     R6,DMCB                                                          
         CLI   0(R6),C' '                                                       
         BH    COERR                                                            
COX      DS    0H                                                               
         MVI   0(R3),X'01'         ELEMENT CODE                                 
         MVI   1(R3),PSCUELLN      ELEMENT LENGTH                               
         MVC   PSCUDESC(10),WORK                                                
         B     LHV40H5                                                          
*                                                                               
COERR    B     INVERR              INVALID SRI= FORMAT                          
*                                                                               
LHV40H   DS    0H                                                               
         MVI   0(R3),X'01'         ELEMENT CODE                                 
         MVI   1(R3),PSCUELLN      ELEMENT LENGTH                               
         MVC   PSCUDESC,8(R2)                                                   
         OC    PSCUDESC,SPACES                                                  
LHV40H5  DS    0H                                                               
*                                                                               
         MVC   ELTSORT,PSCUDESC    SET SORT KEY TO SPACE                        
*                                                                               
LHVSPOK  DS    0H                                                               
*                                                                               
         OI    4(R2),X'20'         INDICATE FIELD VALIDATED                     
         OI    6(R2),X'80'         TRANSMIT FIELD                               
*                                                                               
LHVCU1   DS    0H                                                               
*                                                                               
         BAS   RE,BUMP             BUMP TO FIRST CU FIELD                       
*                                                                               
         LTR   R4,R4               IF DELETING ENTRY                            
         BZ    LHVCU1OK                                                         
*                                                                               
         ZIC   RF,5(R2)                                                         
         LTR   RF,RF                                                            
         BZ    LHV45               NO INPUT CHECK SECOND FIELD                  
         GOTO1 CASHVAL,DMCB,(4,8(R2)),(RF)                                      
         CLI   0(R1),X'FF'                                                      
         BE    INVERR                                                           
         L     R0,DMCB+4                                                        
         C     R0,=F'0'          CAN'T BE MINUS                                 
         BL    INVERR                                                           
         C     R0,=F'999999'     CAN'T EXCEED 99.9999                           
         BH    INVERR                                                           
         LTR   R0,R0                                                            
         BNZ   LHV42                                                            
         MVC   PSCUXCU,=X'000001'     MEANS 0                                   
         B     LHV45                                                            
*                                                                               
LHV42    MVC   PSCUXCU,DMCB+5       LAST 3 BYTES                                
*                                                                               
LHV45    DS    0H                                                               
*                                                                               
LHVCU1OK DS    0H                                                               
*                                                                               
         OI    4(R2),X'20'         INDICATE FIELD VALIDATED                     
         OI    6(R2),X'80'         TRANSMIT FIELD                               
*                                                                               
LHVCU2   DS    0H                                                               
*                                                                               
         BAS   RE,BUMP             BUMP TO SECOND CU FIELD                      
*                                                                               
         LTR   R4,R4               SKIP IF DELETING FIELD                       
         BZ    LHVCU2OK                                                         
*                                                                               
         ZIC   RF,5(R2)                                                         
         LTR   RF,RF                                                            
         BZ    LHV49               NO INPUT CHECK SECOND FIELD                  
         GOTO1 CASHVAL,DMCB,(4,8(R2)),(RF)                                      
         CLI   0(R1),X'FF'                                                      
         BE    INVERR                                                           
         L     R0,DMCB+4                                                        
         C     R0,=F'0'          CAN'T BE MINUS                                 
         BL    INVERR                                                           
         C     R0,=F'999999'     CAN'T EXCEED 99.9999                           
         BH    INVERR                                                           
         LTR   R0,R0                                                            
         BNZ   LHV48                                                            
         MVC   PSCUPCU,=X'000001'     MEANS 0                                   
         B     LHV49                                                            
*                                                                               
LHV48    MVC   PSCUPCU,DMCB+5       LAST 3 BYTES                                
*                                                                               
LHV49    DS    0H                                                               
         OC    PSCUXCU(6),PSCUXCU    SEE IF EITHER ENTERED                      
         BZ    LHV49X              SKIP THAT SPACE                              
         OI    ELTCTL,ELTADDQ      ADD ELEMENT                                  
         B     *+8                                                              
LHV49X   OI    ELTCTL,ELTDELQ      DELETE ELEMENT                               
*                                                                               
LHV50    DS    0H                  BUMP TO NEXT LINE                            
*                                                                               
*                                                                               
LHVCU2OK DS    0H                                                               
*                                                                               
         OI    4(R2),X'20'         INDICATE FIELD VALIDATED                     
         OI    6(R2),X'80'         TRANSMIT FIELD                               
*                                                                               
LHVCMP   DS    0H                                                               
*                                                                               
         LTR   R4,R4               DONE IF NOT BUILDING AN ELEMENT              
         BZ    LHVLX                                                            
*                                                                               
         MVC   SAVMSGNO,ERROR      SAVE CURRENT MESSAGE NUMBER                  
*                                                                               
         GOTO1 =V(BINSRCH),BSPPRMS,('BSPADD',ELTABD),RR=RELO                    
*                                                                               
         OC    1(3,R1),1(R1)       TEST ROOM                                    
         BZ    LHVCMPE1                                                         
*                                                                               
         L     R4,BSPAREC          POINT TO FOUND TABLE ELEMENT                 
*                                                                               
         STCM  R4,7,ELTENT+1                                                    
*                                                                               
         CLI   BSPCNTL,BSPNF       TEST IF NO MATCH FOUND                       
         BE    LHVL92              YES - NEW ENTRY FOR TABLE                    
*                                                                               
         L     R2,LUACTAB          POINT TO CURRENT SAVED ELEMENT               
         USING LSVTABD,R2                                                       
*                                                                               
         CLC   LSVKEY,ELTKEY       ALSO OK IF ENTRY KEY NOT CHANGED             
         BNE   LHVCMPE2                                                         
*                                                                               
LHVL92   DS    0H                                                               
*                                                                               
         OI    ELTCTL,ELTADDQ      SET ADDED (NB- ADD+DEL=CHA)                  
         MVC   ELTELEM,PSCUEL01    SET NEW ELEM IN TABLE                        
*                                  SET NEW ELTLAST                              
         ICM   R1,15,BSPNOR        NUMBER OF ENTRIES                            
         BZ    *+10                                                             
         BCTR  R1,R0                                                            
         MH    R1,=Y(ELTABL)                                                    
         A     R1,BSPATAB          PLUS START OF TABLE                          
         ST    R1,ELTLAST          SET A(LAST)                                  
         B     LHVLX                                                            
*                                                                               
         EJECT                                                                  
LHVCMPE1 DS    0H                                                               
         MVI   ERROR,RECFULL             TOO MANY DETAIL LINES                  
         B     LHVCMPER                                                         
*                                                                               
LHVCMPE2 DS    0H                                                               
         L     R2,LUACLIN          POINT TO FIRST FIELD ON LINE =SPACE          
         MVI   ERROR,DUPEDATA      DUPLICATE                                    
         B     LHVCMPER                                                         
*                                                                               
LHVCMPER DS    0H                                                               
*                                                                               
         NI    4(R2),X'FF'-X'20'   TURN OFF VALIDATED STATUS                    
         GOTO1 ERREX               HANDLE FIELD IN ERROR                        
*                                                                               
LHVLX    DS    0H                                                               
         CLI   ERROR,0             SET CC                                       
         XIT1                                                                   
         DROP  R4                                                               
         DROP  R3                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        ROUTINE TO BUILD WINDOW LINE                                 *         
*        FROM TABLE ENTRY IN ELTENT                                   *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
LHDISLIN NTR1                                                                   
*                                                                               
         L     R2,LUACLIN          A(FIRST FIELD)                               
*                                                                               
         MVC   FLD,SPACES          DEFAULT TO CLEARING                          
*                                                                               
         TWAXC (R2),(R2)           CLEAR FIELD                                  
*                                                                               
         ICM   R4,15,ELTENT        POINT TO ELEMENT IN TABLE                    
         BZ    LHDSPCX             CHECK IF NONE FOUND                          
*                                                                               
         USING ELTABD,R4           ESTABLISH TABLE ELEMENT                      
*                                                                               
         LA    R3,ELTELEM          ESTABLISH RECORD ELEMENT PART                
         USING PSCUEL01,R3                                                      
*                                                                               
*              DISPLAY SPACE                                                    
*                                                                               
         OC    PSCUDESC,PSCUDESC                                                
         BZ    LHD25                                                            
         CLI   PSCUDESC,X'FF'       CHK FOR OUTDOOR SRI=                        
         BNE   LHD24                                                            
         LA    RF,16(R2)                                                        
         MVC   8(8,R2),=C'SRI=SPC,'                                             
         CP    PSCUDESC+1(3),=P'99999'                                          
         BE    LHD20C                                                           
         LA    RF,12(R2)                                                        
         LA    R6,PSCUDESC+1                                                    
         BAS   RE,EDT                                                           
         MVI   0(RF),C','                                                       
         LA    RF,1(RF)                                                         
LHD20C   DS    0H                                                               
         LA    R6,PSCUDESC+4                                                    
         BAS   RE,EDT                                                           
         MVI   0(RF),C','                                                       
         LA    R6,PSCUDESC+7                                                    
         LA    RF,1(RF)                                                         
         BAS   RE,EDT                                                           
         B     LHD25                                                            
*                                                                               
LHD24    MVC   8(17,R2),PSCUDESC                                                
*                                                                               
         OI    4(R2),X'20'         INDICATE FIELD VALIDATED                     
*                                                                               
LHDSPCX  DS    0H                                                               
*                                                                               
LHD25    BAS   RE,BUMP                                                          
         TWAXC (R2),(R2)           CLEAR FIELD                                  
*                                                                               
         LTR   R4,R4               SKIP IF NOT ELEMENT TO DISPLAY               
         BZ    LHD27                                                            
*                                                                               
         OI    4(R2),X'20'         INDICATE FIELD VALIDATED                     
*                                                                               
         OC    PSCUXCU,PSCUXCU                                                  
         BZ    LHD27                                                            
         MVI   8(R2),C'0'                                                       
         CLC   PSCUXCU,=X'000001'                                               
         BE    LHD27                                                            
         EDIT  PSCUXCU,(7,8(R2)),4,ALIGN=LEFT                                   
*                                                                               
LHD27    DS    0H                                                               
         BAS   RE,BUMP                                                          
         TWAXC (R2),(R2)           CLEAR FIELD                                  
*                                                                               
         LTR   R4,R4               SKIP IF NOT ELEMENT TO DISPLAY               
         BZ    LHD30                                                            
*                                                                               
         OI    4(R2),X'20'         INDICATE FIELD VALIDATED                     
*                                                                               
         OC    PSCUPCU,PSCUPCU                                                  
         BZ    LHD30                                                            
         MVI   8(R2),C'0'                                                       
         CLC   PSCUPCU,=X'000001'                                               
         BE    LHD30                                                            
         EDIT  PSCUPCU,(7,8(R2)),4,ALIGN=LEFT                                   
*                                                                               
LHD30    DS    0H                                                               
*                                                                               
LHDLX    DS    0H                                                               
         XIT1                                                                   
         DROP  R3                                                               
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
* ROUTINE TO UPDATE RECORD BASED ON ELEMENT TABLE CHANGES             *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
LSWRTTAB NTR1                                                                   
*                                                                               
         OI    GENSTAT2,RETEQSEL   HAVE GENCON RETURN THIS SCREEN               
         MVI   ACTELOPT,C'N'       NO ACTIVITY ELEMENT                          
*                                                                               
         MVC   SAVMSGNO,ERROR      SAVE MESSAGE NUMBER                          
*                                                                               
*        FIRST DELETE ALL CURRENT ELEMENTS                                      
*                                                                               
         L     R6,AIO1             GET RECORD ADDRESS                           
         ST    R6,DMCB             PASS TO RECUP                                
         MVI   DMCB,C'P'           INDICATE PRINT SYSTEM                        
*                                                                               
         LA    R6,PSCUELEM-PSCUREC(R6)  POINT TO FIRST ELEMENT IN REC           
*                                                                               
         XC    DMCB+8(4),DMCB+8    SET FOR DELETE                               
*                                                                               
         SR    R0,R0                                                            
         LA    R1,DMCB             POINT TO PARAMETER LIST                      
         L     RF,VRECUP           POINT TO UPDATING ROUTINE                    
*                                                                               
LSWTDELL DS    0H                                                               
*                                                                               
         USING PSCUEL01,R6         ESTABLISH SCU ELEMENT                        
*                                                                               
         CLI   PSCUEL01,0          DONE IF EOR REACHED                          
         BE    LSWTDELD                                                         
*                                                                               
         CLI   PSCUEL01,X'01'      SKIP IF NOT SCU ELEMENT                      
         BNE   LSWTDELC                                                         
*                                                                               
         GOTO1 (RF),(R1),,(R6)     DELETE ELEMENT                               
*                                                                               
         B     LSWTDELX            BECAUSE R6 ==> NEXT ELM NOW                  
*                                                                               
LSWTDELC DS    0H                                                               
*                                                                               
         IC    R0,PSCUEL01+1       ELEMENT LENGTH                               
         AR    R6,R0               NEXT ELEMENT                                 
*                                                                               
LSWTDELX DS    0H                                                               
*                                                                               
         B     LSWTDELL                                                         
*                                                                               
LSWTDELD DS    0H                  ALL ELEMENTS DELETED                         
*                                                                               
         DROP  R6                                                               
*                                                                               
*        ADD ELEMENTS IN TABLE TO RECORD                                        
*                                                                               
         L     R4,BSPATAB          START OF TABLE                               
         USING ELTABD,R4           ESTABLISH ENTRY IN TABLE                     
*                                                                               
         OC    BSPNOR,BSPNOR       SKIP IF NO ENTRIES                           
         BZ    LSWTLPDN                                                         
*                                                                               
         L     R6,AIO1             GET RECORD ADDRESS                           
         ST    R6,DMCB             PASS TO RECUP                                
         MVI   DMCB,C'P'           INDICATE PRINT SYSTEM                        
*                                                                               
         LA    R6,PSCUELEM-PSCUREC(R6)  POINT TO FIRST ELEMENT IN REC           
*                                                                               
         SR    R0,R0                                                            
         LA    R1,DMCB             POINT TO PARAMETER LIST                      
*                                                                               
LSWTLOOP DS    0H                                                               
*                                                                               
         TM    ELTCTL,ELTADDQ      ADD ELEMENTS FLAGGED FOR ADD                 
         BO    LSWTADD                                                          
         TM    ELTCTL,ELTDELQ      AND THOSE NOT TO BE DELETED                  
         BO    LSWTNADD                                                         
*                                                                               
LSWTADD  DS    0H                                                               
*                                                                               
         XC    ELEMENT,ELEMENT     CLEAR WORKAREA                               
*                                                                               
         LA    R3,ELTELEM          POINT TO ELEMENT IN TABLE                    
         USING PSCUEL01,R3         ESTABLISH SCU ELEMENT                        
*                                                                               
         CLI   PSCUEL01+1,0        GET ELEMENT LENGTH                           
         BZ    LSWTADDX            SKIP IF NO ENTRY                             
*                                                                               
         GOTO1 (RF),(R1),,(R3),(C'R',(R6))  ADD ELEMENT                         
*                                                                               
         CLI   DMCB+8,0            ONLY ERROR CAN BE NO ROOM IN REC             
         BE    NOMORE              NO ERROR TOLERATED                           
*                                                                               
         IC    R0,1(R6)            ELEMENT LENGTH                               
         AR    R6,R0               NEXT INSERTION POINT                         
*                                                                               
LSWTADDX DS    0H                                                               
*                                                                               
         B     LSWTLPCN                                                         
*                                                                               
LSWTNADD DS    0H                                                               
*                                                                               
LSWTLPCN DS    0H                                                               
*                                                                               
         A     R4,BSPLENR          BUMP TO NEXT ENTRY                           
         C     R4,ELTLAST          LOOP IF NOT LAST ENTRY                       
         BNH   LSWTLOOP                                                         
*                                                                               
LSWTLPDN DS    0H                                                               
*                                                                               
LSWRTX   DS    0H                                                               
         MVC   ERROR,SAVMSGNO      RESTORE MESSAGE NUMBER                       
         XIT1                                                                   
         DROP  R3                                                               
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* COMMONLY ADDRESSABLE ROUTINES                                       *         
***********************************************************************         
         SPACE 1                                                                
SUBROUTS DS    0D                                                               
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
ERRFLD   OI    IPSTAT,LUWVERRQ     INDICATE VALIDATION ERROR                    
         OI    FLDATB,FATBHIGH     HIGHLIGHT FIELD                              
         OI    FLDOIND,FOUTTRN     TRANSMIT FIELD                               
         NI    FLDIIND,X'FF'-FINPVAL TURN OFF VALID INDICATOR                   
         CLI   SAVMSGNO,0          IF NOT FIRST ERROR                           
         BE    *+14                                                             
         MVC   ERROR,SAVMSGNO      RESTORE PRIOR MESSAGE                        
         B     ERRFLDX                                                          
*                                                                               
         ST    R1,ACURFORC         PUT CURSOR HERE                              
*                                                                               
ERRFLDX  DS    0H                                                               
         BR    RE                                                               
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* OTHER LITTLE ROUTINES                                               *         
***********************************************************************         
         SPACE 2                                                                
BUMP     ZIC   RF,0(R2)            BUMP TO NEXT SCREEN FIELD                    
         AR    R2,RF                                                            
         CLI   0(R2),0             EOS- RETURN =                                
         BR    RE                                                               
         SPACE 2                                                                
BUMPU    ZIC   RF,0(R2)            BUMP TO NEXT UNPROTECTED FIELD               
         AR    R2,RF                                                            
         CLI   0(R2),0             EOS- RETRUN =                                
         BER   RE                                                               
         TM    1(R2),X'20'                                                      
         BNZ   BUMPU                                                            
         LTR   RE,RE               NOT EOS- RETURN NOT =                        
         BR    RE                                                               
***********************************************************************         
*        DISPLAY DATA IN  FLD IN FIELD POINTED TO BY R1               *         
***********************************************************************         
         SPACE 2                                                                
DSPFLD   NTR1                      BUMP TO NEXT SCREEN FIELD                    
*                                                                               
         USING FLDHDRD,R1          ESTABLISH SCREEN FIELD                       
*                                                                               
         SR    RF,RF                                                            
         IC    RF,FLDLEN           FIELD LENGTH                                 
         SH    RF,=H'8'            HEADER LENGTH                                
*                                                                               
         TM    FLDATB,X'02'        IF THERE IS EXTENED HEADER                   
         BNO   *+8                                                              
         SH    RF,=H'8'               TAKE OFF HEADER LENGTH                    
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R1),FLD         MOVE DATA TO OUTPUT                          
*                                                                               
         OI    FLDOIND,X'80'       TRANSMIT FIELD                               
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
EDT      DS    0H          USED TO DISPLAY OUTDOOR SRI SPACES                   
         MVI   0(RF),C'0'                                                       
         LA    R0,1                                                             
         CP    0(3,R6),=P'0'                                                    
         BE    EDT2                                                             
         EDIT  (P3,0(R6)),(4,0(RF)),ALIGN=LEFT                                  
*                                                                               
EDT2     DS    0H                                                               
         AR    RF,R0                                                            
         BR    RE                                                               
*                                                                               
DATERR   MVI   ERROR,INVDATE                                                    
         B     ERRX                                                             
*                                                                               
INVERR   MVI   ERROR,INVALID                                                    
         B     ERRX                                                             
*                                                                               
DUPERR   MVI   ERROR,DUPEDATA      DUPLICATE DATA                               
         B     ERRX                                                             
*                                                                               
MISSERR  MVI   ERROR,MISSING                                                    
         B     ERRX                                                             
*                                                                               
NOMORE   MVI   ERROR,RECFULL       NO ROOM IN RECORD OR TABLE                   
         LA    R2,FSISP1H                                                       
         B     ERRX                                                             
*                                                                               
ERRX     GOTO1 ERREX                                                            
*                                                                               
DUPEDATA EQU   179                 DUPLICATE DATA                               
RECFULL  EQU   180                 RECORD FULL                                  
         EJECT                                                                  
*                                                                               
HEDSPECS SSPEC H1,58,C'SPACE CU REPORT'                                         
         SSPEC H2,58,C'---------------'                                         
         SSPEC H1,95,AGYNAME                                                    
         SSPEC H2,95,AGYADD                                                     
         SSPEC H4,95,RUN                                                        
         SSPEC H5,95,REPORT                                                     
         SSPEC H1,1,REQUESTOR                                                   
         SSPEC H7,1,C'PUB CODE        PUB NAME             CLIENT'              
         SSPEC H7,46,C'SPACE              TIME CU    PAGE CU'                   
         SSPEC H8,1,C'--------        --------             ------'              
         SSPEC H8,46,C'------             -------    -------'                   
         DC    X'00'                                                            
PUBZNM   DS    CL20                                                             
MYPUB    DS    XL6                                                              
MYDSKADD DS    XL4                                                              
         EJECT                                                                  
         PRINT GEN                                                              
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
       ++INCLUDE PRSFMFFD                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFMECD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFMFCD                                                       
*                                                                               
LSVTAB   DS    XL(NLINS*LSVTABL)                                                
*                                                                               
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE PRSFMWORKD                                                     
*                                                                               
         ORG   SYSSPARE                                                         
RELO     DS    F                                                                
ORIGKEY  DS    XL(L'KEY)                                                        
MYKEY    DS    XL(L'KEY)                                                        
ALLZE    DS    CL1                                                              
SCANBLK  DS    CL70                                                             
WRKELTAB DS    XL(ELTABL)          WORK TABLE ELEMENT                           
SAVMSGNO DS    XL1                 CURRENT MESSAGE NUMBER SAVEAREA              
SAVCURI  DS    XL1                 INDEX OF ERROR INTO FIELD                    
IPSTAT   DS    XL1                 CUMULATIVE INPUT STATISTICS                  
NEWKEY   DS    XL1                 C'Y' - BASIC KEY HAS CHENGED                 
ELTENT   DS    A                   A(ELEM TABLE ENTRY)                          
ELTLAST  DS    A                   A(LAST ENTRY)                                
         DS    0F                                                               
*                                                                               
         DS    0F                                                               
       ++INCLUDE DDBSRPRMD                                                      
         DS    0D                                                               
LUBLK    DS    XL(LUBLKL)          LINUP CONTROL BLOCK                          
         DS    0F                                                               
LINDSPS  DS    XL((NLINS+1)*2)                                                  
SVLSVTAB DS    XL(NLINS*LSVTABL)      HOLD COPY OF LINUP SAVE TABLE             
ELTMAX   EQU   (3900/PSCUELLN)-1      MAX NUMBER OF ELEMENTS IN TABLE           
*                                                                               
*                                                                               
* *******************                                                           
* ON-SCREEN LIST LINE                                                           
* *******************                                                           
*                                                                               
LISTD    DSECT                                                                  
LPUB     DS    CL15                PUBLICATION NUMBER ZONE/EDT                  
         DS    CL1                                                              
LPUBN    DS    CL20                PUB NAME                                     
         DS    CL3                                                              
LCLT     DS    CL3                 CLIENT                                       
         DS    CL3                                                              
LSPACE   DS    CL17                                                             
         DS    CL2                                                              
LXCU     DS    CL7                                                              
         DS    CL2                                                              
LPCU     DS    CL7                                                              
         EJECT                                                                  
       ++INCLUDE PSCUREC                                                        
         EJECT                                                                  
PUBRECD  DSECT                                                                  
       ++INCLUDE PUBREC                                                         
       ++INCLUDE PUBNAMEL                                                       
PCLTRECD DSECT                                                                  
       ++INCLUDE PCLTREC                                                        
         EJECT                                                                  
*DDSPOOLD                                                                       
*DDCOMFACS                                                                      
*DDFLDIND                                                                       
*DDFLDHDR                                                                       
*PPSRCHPARM                                                                     
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDFLDIND                                                       
       ++INCLUDE DDFLDHDR                                                       
       ++INCLUDE PPSRCHPARM                                                     
         EJECT                                                                  
       ++INCLUDE DDLINUPD                                                       
         EJECT                                                                  
         PRINT ON                                                               
*                                                                               
LSVTABD  DSECT                     LINUP SAVE AREA DSECT                        
LSVKEY   DS    0XL(L'LSVSORT)                                                   
LSVSORT  DS    XL(L'PSCUDESC)                                                   
LSVKEYL  EQU   *-LSVTABD                                                        
LSVTABL  EQU   *-LSVTABD                                                        
         SPACE 2                                                                
ELTABD   DSECT                     DSECT FOR ELEM TABLE                         
ELTKEY   DS    0XL(L'ELTSORT)                                                   
ELTSORT  DS    XL(L'PSCUDESC)      SORT VALUE                                   
ELTKEYL  EQU   *-ELTABD            KEY LENGTH                                   
ELTCTL   DS    XL1                 CONTROL BYTE                                 
ELTDELQ  EQU   X'80'                 DELETE                                     
ELTADDQ  EQU   X'40'                 ADD                                        
ELTELEM  DS    CL(PSCUELLN)        ELEMENT                                      
ELTABL   EQU   *-ELTABD            ENTRY LENGTH                                 
*                                                                               
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'058PRSMF15   05/01/02'                                      
         END                                                                    
