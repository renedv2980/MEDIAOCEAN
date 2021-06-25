*          DATA SET PRSFM05    AT LEVEL 067 AS OF 07/15/02                      
*PHASE T41C05A,*                                                                
*INCLUDE SRCHCALL                                                               
*INCLUDE PUBEDIT                                                                
         TITLE 'T41C05 FSI RECORDS'                                             
*                                                                               
*   CHANGE LOG                                                                  
*                                                                               
* SMYE 06/27/02  IN VK IF REPORTING AND LIMIT ACCESS ACTIVE, REQUIRE            
*                CLIENT CODE ENTRY                                              
*                                                                               
* SMYE 11/02/00  ADDED CLIENT VALIDATION (SECURITY) TO DK                       
*                                                                               
* BPLA 9/22/94   IN VK IF ACTNUM IS ACTREP (REPORTING)                          
*                SET QCLT TO BINARY ZEROS (NOT X'FFFFFF')                       
*                                                                               
* BPLA 9/21/94   DISPLAY MEDIA IN HEADHOOK                                      
*                                                                               
* BPLA 1/13/93   NEW SCREEN + USE BPUB AND QCLT AS FILTERS WHEN                 
*                LISTING OR REPORTING RECORDS                                   
*                                                                               
* BPLA 11/19/92  CHANGES IN KEY FORMAT AND LOGIC TO DO REPORT                   
*               +ZONE NAME DISPLAY                                              
*                                                                               
* BPLA 12/21/92  DISALLOW FOR MEDIA O (OUTDOOR)                                 
*                                                                               
T41C05   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T41C05,RR=R3                                                   
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         ST    R3,RELO                                                          
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
         BE    DR                                                               
         CLI   MODE,XRECPUT        RE-DISPLAY RECORD                            
         BE    DR                                                               
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
*NOP*    CLI   ACTNUM,ACTLIST      LIST                                         
*NOP*    BE    VKL                                                              
*                                                                               
         LA    R2,FSIMEDH         MEDIA                                         
         GOTO1 VALIMED                                                          
*                                                                               
         CLI   FSIMED,C'O'                                                      
         BNE   VK5                                                              
         MVI   ERROR,INVMED                                                     
         B     ERRX                                                             
*                                                                               
VK5      LA    R2,FSIPUBH                                                       
         MVC   PUBNM,SPACES                                                     
         MVC   PUBZNM,SPACES                                                    
         XC    BPUB,BPUB                                                        
         XC    FSIPUBN,FSIPUBN                                                  
         XC    FSIPUBZ,FSIPUBZ                                                  
         BAS   RE,MYPUBVAL                                                      
         MVC   FSIPUBN,PUBNM       DISPLAY PUB NAME                             
         OI    FSIPUBNH+6,X'80'                                                 
         MVC   FSIPUBZ,PUBZNM      DISPLAY PUB ZONE                             
         OI    FSIPUBZH+6,X'80'                                                 
*                                                                               
         LA    R2,FSICLTH          CLIENT                                       
         MVC   CLTNM,SPACES                                                     
*                                                                               
         XC    QCLT,QCLT           CLEAR QCLT                                   
         CLI   ACTNUM,ACTREP       SEE IF REPORTING                             
         BE    VK10                                                             
         CLI   ACTNUM,ACTLIST      SEE IF LIST                                  
         BE    VK10                                                             
         MVC   QCLT,=X'FFFFFF'     SET TO DEFAULT                               
*                                                                               
VK10     DS    0H                                                               
         CLI   5(R2),0                                                          
         BE    VK20                                                             
         CLC   =C'ALL',8(R2)                                                    
         BE    VK20                                                             
         GOTO1 VALICLT                                                          
*                                                                               
VK20     DS    0H                                                               
         OC    TWAACCS(2),TWAACCS  ANY LIMIT ACCESS ?                           
         BZ    VK40                NO                                           
         CLI   CONWHENH+5,0        NOW, SOON OR OV ?                            
         BE    VK40                NO                                           
VK30     DS    0H                                                               
         OC    QCLT,QCLT           CLIENT "ALL" OR NOT ENTERED ?                
         BNZ   VK40                NO                                           
         MVI   ERROR,INVCLI                                                     
         CLC   =C'ALL',8(R2)                                                    
         BNE   ERRX                NO                                           
         LA    R2,FSIPUBH                                                       
         CLI   5(R2),0             PUB ENTERED ?                                
         BNE   VK40                YES                                          
         MVI   ERROR,MISSING                                                    
         B     ERRX                NO                                           
*                                                                               
VK40     MVC   FSICLTN,CLTNM       DISPLAY CLT NAME                             
         OI    FSICLTNH+6,X'80'                                                 
*                                                                               
         XC    KEY,KEY             BUILD KEY                                    
         LA    R4,KEY                                                           
         USING PFSIREC,R4                                                       
         MVC   PFSIKAGY,AGENCY                                                  
         MVC   PFSIKMED,QMED                                                    
         MVI   PFSIKTYP,X'27'                                                   
         MVC   PFSIKCLT,QCLT                                                    
         MVC   PFSIKPUB,BPUB                                                    
         MVC   ORIGKEY,KEY         SAVE THIS KEY (USE WITH LIST)                
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
         BE    VKXIT                                                            
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
         CLI   ACTNUM,ACTLIST      SEE IF LIST                                  
         BE    VKPX                                                             
         B     ERRX                IF NOT THE MUST HAVE PUB                     
*                                                                               
VKPUB5   CLI   8(R2),C'='          PUB NAME SEARCH                              
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
         USING PFSIREC,R6                                                       
         MVC   FSIMED,PFSIKMED     MEDIA                                        
         OI    FSIMEDH+6,X'80'     TRANSMIT                                     
*                                                                               
         MVC   FSICLT,PFSIKCLT     CLIENT                                       
         CLC   PFSIKCLT,=X'FFFFFF'                                              
         BNE   DK5                                                              
         MVC   CLTNM,SPACES                                                     
         MVC   FSICLT,=C'ALL'                                                   
         B     DK10                                                             
*                                                                               
DK5      DS    0H                                                               
*                                                                               
         MVC   MYKEY(L'PFSIKEY),PFSIKEY                                         
*                                                                               
         LA    R2,FSIMEDH                                                       
         MVI   5(R2),1                                                          
         GOTO1 VALIMED                                                          
*                                                                               
         OI    FSICLTH+6,X'80'     TRANSMIT                                     
         LA    R2,FSICLTH                                                       
         MVI   5(R2),3                                                          
         GOTO1 VALICLT                                                          
*                                  RESTORE KEY AND RECORD                       
         MVC   KEY(L'PFSIKEY),MYKEY                                             
         GOTO1 HIGH                                                             
         CLC   KEY(L'PFSIKEY),MYKEY         SAME RECORD ?                       
         BE    *+6                 YES - OK                                     
         DC    H'0'                SOMETHING WRONG                              
         GOTO1 GETREC                                                           
*                                                                               
*NOP*    BAS   RE,MYVCLT           READS THE CLIENT                             
*                                                                               
DK10     OI    FSICLTH+6,X'80'                                                  
         MVC   FSICLTN,CLTNM       DISPLAY CLT NAME                             
         OI    FSICLTNH+6,X'80'                                                 
*                                                                               
         GOTO1 =V(PUBEDIT),DMCB,(C'0',PFSIKPUB),(0,FSIPUB),RR=RELO              
         OI    FSIPUBH+6,X'80'                                                  
*                                                                               
         MVC   MYPUB,PFSIKPUB                                                   
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
         LA    R2,FSIEDATH         EFFECTIVE DATE                               
         XR    R5,R5               COUNTER                                      
         XC    DATETAB(L'DATETAB),DATETAB                                       
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'                                                     
         GOTO1 REMELEM             REMOVE ALL OLD ELEMENTS                      
*                                                                               
VR10     LA    R3,ELEMENT                                                       
         USING PFSIEL01,R3                                                      
         XC    ELEMENT,ELEMENT                                                  
         ZAP   PFSINUM,=P'0'                                                    
*                                                                               
         CLI   5(R2),0             IF NO INPUT                                  
         BNE   VR30                                                             
         LR    R0,R2               SAVE R2                                      
         BAS   RE,BUMP             & SEE IF ANY INPUT ON THIS LINE              
         CLI   5(R2),0                                                          
         BE    VR50                                                             
         LR    R2,R0               RESET R2 FOR ERROR                           
         B     MISSERR                                                          
*                                                                               
VR30     CLC   =C'DELETE',8(R2)    DELETE THIS ENTRY                            
         BNE   VR40                                                             
         BAS   RE,BUMP             BUMP TO NEXT LINE                            
         B     VR30                                                             
*                                                                               
VR40     MVI   0(R3),X'01'         ELEMENT CODE                                 
         MVI   1(R3),PFSIELLN      ELEMENT LENGTH                               
         GOTO1 DATVAL,DMCB,8(R2),WORK                                           
         OC    0(4,R1),0(R1)                                                    
         BE    DATERR                                                           
         GOTO1 DATCON,DMCB,(0,WORK),(3,PFSIDATE)                                
         XC    PFSIDATE,=X'FFFFFF'                                              
         BAS   RE,DUPDATE          MAKE SURE NO DUPLICATE DATE ENTRIES          
         BE    DUPERR                                                           
         BAS   RE,BUMP             BUMP TO INSERTIONS FIELD                     
*                                                                               
         ZIC   R4,5(R2)                                                         
         LTR   R4,R4                                                            
         BZ    MISSERR                                                          
         GOTO1 CASHVAL,DMCB,(C'0',8(R2)),(R4)                                   
         CLI   0(R1),X'FF'                                                      
         BE    INVERR                                                           
         OC    4(3,R1),4(R1)                                                    
         BNZ   INVERR                                                           
         MVC   PFSINUM,7(R1)                                                    
         CP    PFSINUM,=P'0'      CAN'T BE ZERO OR MINUS                        
         BNH   INVERR                                                           
*                                                                               
         CH    R5,=H'14'                                                        
         BE    NOMORE                                                           
         GOTO1 ADDELEM                                                          
         LA    R5,1(R5)            COUNTER                                      
*                                                                               
VR50     BAS   RE,BUMP             BUMP TO NEXT LINE                            
         LA    R1,FSILAST                                                       
         CR    R2,R1                                                            
         BL    VR10                                                             
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
* DISPLAY RECORD                                                                
*                                                                               
DR       DS    0H                                                               
         TWAXC FSIEDATH                                                         
         LA    R2,FSIDEDTH                                                      
         LA    R4,14               SET FOR LOOP                                 
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'                                                     
         USING PFSIEL01,R6                                                      
         BAS   RE,GETEL                                                         
         B     DR20                                                             
*                                                                               
DR10     BAS   RE,NEXTEL                                                        
*                                                                               
DR20     BNE   DRX                 NO ELEMENTS LEFT                             
         OC    PFSIDATE,PFSIDATE                                                
         BZ    DR25                                                             
         MVC   FULL,PFSIDATE                                                    
         XC    FULL(3),=X'FFFFFF'                                               
         GOTO1 DATCON,DMCB,(3,FULL),(5,8(R2))                                   
*                                                                               
DR25     BAS   RE,BUMP                                                          
         CLC   PFSINUM,=PL5'0'                                                  
         BE    DR30                                                             
         EDIT  PFSINUM,(10,8(R2)),0,ALIGN=LEFT                                  
*                                                                               
DR30     BAS   RE,BUMP                                                          
         BCT   R4,DR10                                                          
*                                                                               
DRX      B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
* LIST RECORDS                                                                  
*                                                                               
LR       DS    0H                                                               
         OI    GLSTSTAT,RETEXTRA                                                
         LA    R6,KEY                                                           
         USING PFSIREC,R6                                                       
         OC    KEY,KEY             TEST FIRST TIME                              
         BNZ   LR030               KEY IS LAST RECORD READ                      
*                                  SO GO CHECK VS. KEYSAVE                      
*                                                                               
         MVC   PFSIKAGY,AGENCY     CREATE KEY - AGENCY                          
         MVC   PFSIKMED,QMED                    MEDIA CODE                      
         MVI   PFSIKTYP,X'27'                  TYPE                             
         CLI   FSLPUBH+5,0                                                      
         BE    *+10                                                             
         MVC   PFSIKPUB,BPUB                    PUB                             
         CLI   FSLCLTH+5,0                                                      
         BE    *+10                                                             
         MVC   PFSIKCLT,QCLT                    CLIENT                          
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
         CLC   PFSIKPUB,BPUB                                                    
         BE    LR037                                                            
         XC    KEY,KEY                                                          
         B     LRX                                                              
*                                                                               
LR037    OC    QCLT,QCLT          SEE IF CLIENT GIVEN                           
         BZ    LR039                                                            
         CLC   PFSIKCLT,QCLT                                                    
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
         GOTO1 =V(PUBEDIT),DMCB,(C'0',PFSIKPUB),(0,LPUB),RR=RELO                
         MVC   MYPUB,PFSIKPUB                                                   
         BAS   RE,MYVPUB                                                        
         MVC   LPUBN,PUBNM         DISPLAY PUB NAME                             
*                                                                               
         MVC   LCLT,PFSIKCLT       CLIENT                                       
         CLC   PFSIKCLT,=X'FFFFFF'                                              
         BNE   *+10                                                             
         MVC   LCLT,=C'ALL'                                                     
*                                                                               
         USING PFSIEL01,R6                                                      
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BNE   LR080                                                            
         B     LR050                                                            
*                                                                               
LR040    DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BNE   LR080                                                            
*                                                                               
LR050    MVC   FULL,PFSIDATE                                                    
         XC    FULL(3),=X'FFFFFF'                                               
         GOTO1 DATCON,DMCB,(3,FULL),(5,LEFFDT)                                  
         OC    PFSINUM,PFSINUM                                                  
         BZ    LR070                                                            
         EDIT  PFSINUM,(10,LNUM)                                                
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
         MVC   H2(5),=C'MEDIA'                                                  
         MVC   H2+10(1),QMED                                                    
         MVC   H2+15(10),MEDNM                                                  
*                                                                               
HOOKX    B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        CHECK IF DATE ALREADY INPUT                                            
*        R3 - ELEMENT                                                           
*                                                                               
DUPDATE  NTR1                                                                   
         USING PFSIEL01,R3                                                      
         LA    R2,DATETAB          KEEP TABLE OF DATES INPUT                    
         LA    R1,14               MAXIMUM OF 14 DATES                          
*                                                                               
DUP10    OC    0(3,R2),0(R2)       END OF TABLE                                 
         BE    DUP20                                                            
         CLC   PFSIDATE,0(R2)                                                   
         BE    YES                                                              
         LA    R2,3(R2)            BUMP TABLE                                   
         BCT   R1,DUP10                                                         
*                                                                               
DUP20    MVC   0(3,R2),PFSIDATE                                                 
         B     NO                                                               
         DROP  R3                                                               
         SPACE 3                                                                
*                                                                               
*        BUMP TO NEXT FIELD                                                     
*                                                                               
BUMP     ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         BR    RE                                                               
*                                                                               
DATERR   MVI   ERROR,INVDATE                                                    
         B     ERRX                                                             
*                                                                               
INVERR   MVI   ERROR,INVALID                                                    
         B     ERRX                                                             
*                                                                               
DUPERR   MVI   ERROR,DUPDATER                                                   
         B     ERRX                                                             
*                                                                               
MISSERR  MVI   ERROR,MISSING                                                    
         B     ERRX                                                             
*                                                                               
NOMORE   MVI   ERROR,NOMORERM                                                   
         LA    R2,FSIEDATH                                                      
         B     ERRX                                                             
*                                                                               
ERRX     GOTO1 ERREX                                                            
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
HEDSPECS SSPEC H1,58,C'FSI REPORT'                                              
         SSPEC H2,58,C'----------'                                              
         SSPEC H1,95,AGYNAME                                                    
         SSPEC H2,95,AGYADD                                                     
         SSPEC H4,95,RUN                                                        
         SSPEC H5,95,REPORT                                                     
         SSPEC H1,1,REQUESTOR                                                   
         SSPEC H7,1,C'PUB CODE        PUB NAME             CLIENT'              
         SSPEC H7,47,C'EFF DATE   NUM INSERTS'                                  
         SSPEC H8,1,C'--------        --------             ------'              
         SSPEC H8,47,C'--------   -----------'                                  
         DC    X'00'                                                            
         EJECT                                                                  
         PRINT GEN                                                              
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
PUBZNM   DS    CL20                                                             
MYPUB    DS    XL6                                                              
MYDSKADD DS    XL4                                                              
DATETAB  DS    XL(14*3)                                                         
         EJECT                                                                  
       ++INCLUDE PRSFMFFD                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFME5D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFMF5D                                                       
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
         DS    CL4                                                              
LEFFDT   DS    CL8                 EFFECTIVE DATE                               
         DS    CL4                                                              
LNUM     DS    CL10                NUMBER OF INSERTS                            
         EJECT                                                                  
       ++INCLUDE PFSIREC                                                        
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
*PPSRCHPARM                                                                     
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDFLDIND                                                       
       ++INCLUDE PPSRCHPARM                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'067PRSFM05   07/15/02'                                      
         END                                                                    
