*          DATA SET PPMAT03    AT LEVEL 035 AS OF 05/01/02                      
*PHASE T40203A                                                                  
***********************************************************************         
*                                                                               
*  TITLE: T40203 - COMMENTING OF PRINT INVOICES                                 
*                                                                               
*  CALLED FROM: PRINT INVOICE CONTROLLER (T40200), WHICH CALLS                  
*               DDGENCON (T00A30) WHICH CALLS PPMAT02 (T40202)                  
*               WHICH CALLS THIS.                                               
*                                                                               
*  SCREENS:     PPMATCB (T402CB) - COMMENT SCREEN                               
*                                                                               
*  LOCALS: REGISTER USAGE                                                       
*          R0 - WORK                                                            
*          R1 - WORK                                                            
*          R2 - WORK (SCREEN FIELD HEADER)                                      
*          R3 - WORK                                                            
*          R4 - OVERLAY SAVED STORAGE    (MYAREAD)                              
*          R5 - MINIO CONTROL BLOCK      (MINBLKD)                              
*          R6 - MINELEM                                                         
*          R7 - SECOND BASE                                                     
*          R8 - SPOOLD                                                          
*          R9 - SYSD                                                            
*          RA - TWA                                                             
*          RB - FIRST BASE                                                      
*          RC - GEND                                                            
*          RD - SYSTEM                                                          
*          RE - SYSTEM/WORK                                                     
*          RF - SYSTEM/WORK                                                     
*                                                                               
***********************************************************************         
T40203   TITLE 'PPMAT03 - INVOICE DETAIL COMMENT'                               
*                                                                               
*        CHANGE LOG                                                             
*                                                                               
*   BPLA 2/99   IN VR IF INVALID OPTION ENCOUNTER DON'T DIE                     
*               JUST SEND INVALID OPTION ERROR                                  
*                                                                               
T40203   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T40203*,R7,RR=R3                                              
         L     RC,0(R1)            STANDARD CODING                              
         USING GEND,RC                                                          
         L     RA,4(R1)                                                         
         USING CONHEADH-64,RA      BASE SCREEN + OUR SCREEN                     
         L     R9,8(R1)                                                         
         USING SYSD,R9                                                          
         L     R8,12(R1)                                                        
         USING SPOOLD,R8                                                        
         L     R5,16(R1)           MINIO CONTROL BLOCK                          
         USING MINBLKD,R5                                                       
         L     R4,20(R1)           OVERLAY SAVED STORAGE                        
         USING MYAREAD,R4                                                       
         ST    R3,RELO                                                          
         EJECT                                                                  
***********************************************************************         
* CHECK IF IN COMMENT SCREEN ALREADY                                            
***********************************************************************         
VK       DS    0H                                                               
         TM    OVLYFLAG,X'80'                                                   
         BNZ   VR                                                               
*                                                                               
         MVI   PFKEY,0             CLEAR THE PFKEY                              
         L     R1,ATIOB                                                         
         MVI   TIOBAID-TIOBD(R1),0                                              
*                                                                               
         LH    R2,CURDISP          WHERE CURSOR WAS WHEN PF3 PRESSED            
         ST    R2,AOFLINE          SAVE CURSOR POSITION                         
         AR    R2,RA                                                            
*                                                                               
         OI    CHKOPTNH+6,X'20'    PROTECT OPTION FIELD                         
*                                                                               
         XC    JOBREC(256),JOBREC                                               
         CLI   ACTNUM,ACTCHECK     UPDATE AND SUPERCHECK SHARE SAME             
         BE    VK10                    DISPLACEMENTS INTO TWA                   
*                                                                               
         LA    R1,SC1SEL1H                                                      
         MVC   JOBREC+80(L'SC1NET),SC1NET                                       
*                                                                               
         CLI   SCRTYPE,C'N'                                                     
         BE    VK20                                                             
*                                                                               
         LA    R1,SC2SEL1H                                                      
         MVC   JOBREC+80(L'SC2NET),SC2NET                                       
         B     VK20                                                             
*                                                                               
VK10     LA    R1,CK1SEL1H                                                      
         MVC   JOBREC+80(L'CK1NET),CK1NET                                       
*                                                                               
         CLI   SCRTYPE,C'N'                                                     
         BE    VK20                                                             
*                                                                               
         LA    R1,CK2SEL1H                                                      
         MVC   JOBREC+80(L'CK2NET),CK2NET                                       
*                                                                               
VK20     CLI   ACTNUM,ACTUPDTE                                                  
         BE    VK100                                                            
*                                                                               
         CR    R2,R1               CURSOR COULD BE IN UPPER WINDOW              
         BL    NO                                                               
*                                                                               
         CLI   ACTNUM,ACTCHECK                                                  
         BE    VK30                                                             
*                                                                               
         LA    R1,SC1PFLNH                                                      
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         LA    R1,SC2PFLNH                                                      
         B     VK40                                                             
*                                                                               
VK30     LA    R1,CK1ITEMH                                                      
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         LA    R1,CK2ITEMH                                                      
*                                                                               
VK40     CR    R2,R1                                                            
         BL    VK50                                                             
         CLI   ACTNUM,ACTCHECK                                                  
         BE    VK100               CURSOR MAY BE IN LOWER WINDOW                
         B     NO                  UPDATE & SUPERCHECK DON'T HAVE LOWER         
*                                                                               
VK50     LH    R1,2(R2)            GET SEQUENCE # FOR HEADER AND DETAIL         
         SR    R0,R0                                                            
         D     R0,=F'80'                                                        
         STC   R1,BYTE                                                          
*                                                                               
         CLI   ACTNUM,ACTCHECK                                                  
         BE    VK60                                                             
*                                                                               
         LA    R3,SC1SEL1H                                                      
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         LA    R3,SC2SEL1H                                                      
         B     VK70                                                             
*                                                                               
VK60     LA    R3,CK1SEL1H                                                      
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         LA    R3,CK2SEL1H                                                      
*                                                                               
VK70     LH    R1,2(R3)                                                         
         SR    R0,R0                                                            
         D     R0,=F'80'                                                        
         LR    R0,R1                                                            
         ZIC   R1,BYTE                                                          
         SR    R1,R0                                                            
         STC   R1,BYTE             SAVE OFFSET OF LINE FROM FIRST               
         MH    R1,=Y(L'PBUYKEY)                                                 
         LA    R1,UPPERTBL(R1)                                                  
         OC    0(L'PBUYKEY,R1),0(R1)                                            
         BZ    NO                                                               
         CLI   L'PINVMINI(R1),0    BUY KEY?                                     
         BNE   NO                  YES, DON'T HAVE COMMENTS FOR BUY KEY         
         MVC   SEQUENCE,1(R1)                                                   
*                                                                               
         ZIC   R1,BYTE             GET OFFSET OF LINE FROM FIRST                
*                                                                               
         CLI   SCRTYPE,C'N'                                                     
         BNE   *+12                                                             
         MH    R1,=Y(SLN1LEN)                                                   
         B     *+8                                                              
         MH    R1,=Y(SLN2LEN)                                                   
*                                                                               
         CLI   ACTNUM,ACTCHECK                                                  
         BE    VK80                                                             
*                                                                               
         CLI   SCRTYPE,C'N'                                                     
         BNE   *+12                                                             
         LA    R1,SC1SEL1H(R1)                                                  
         B     *+8                                                              
         LA    R1,SC2SEL1H(R1)                                                  
         B     VK90                                                             
*                                                                               
VK80     CLI   SCRTYPE,C'N'                                                     
         BNE   *+12                                                             
         LA    R1,CK1SEL1H(R1)                                                  
         B     *+8                                                              
         LA    R1,CK2SEL1H(R1)                                                  
*                                                                               
VK90     LR    R2,R1               R2 = A(SELECT LINE)                          
         B     VKLOAD                                                           
*                                                                               
VK100    CLI   ACTNUM,ACTCHECK                                                  
         BNE   VK100A                                                           
         LA    R1,CK1SEL0H                                                      
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         LA    R1,CK2SEL0H                                                      
         B     VK100B                                                           
*                                                                               
VK100A   LA    R1,UP1SEL1H                                                      
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         LA    R1,UP2SEL1H                                                      
*                                                                               
VK100B   CR    R2,R1               CURSOR SHOULD BE IN LOWER WINDOW             
         BL    NO                      IF NOT IN UPPER WINDOW                   
*                                                                               
         CLI   ACTNUM,ACTCHECK                                                  
         BNE   VK100D                                                           
         LA    R1,CK1PFLNH                                                      
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         LA    R1,CK2PFLNH                                                      
         B     VK100E                                                           
*                                                                               
VK100D   LA    R1,UP1PFLNH                                                      
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         LA    R1,UP2PFLNH                                                      
*                                                                               
VK100E   CR    R2,R1                                                            
         BNL   NO                                                               
*                                                                               
         LH    R1,2(R2)            GET SEQUENCE # FOR HEADER AND DETAIL         
         SR    R0,R0                                                            
         D     R0,=F'80'                                                        
         STC   R1,BYTE                                                          
*                                                                               
         CLI   ACTNUM,ACTCHECK                                                  
         BNE   VK110                                                            
         LA    R3,CK1SEL0H                                                      
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         LA    R3,CK2SEL0H                                                      
         B     VK110A                                                           
*                                                                               
VK110    LA    R3,UP1SEL1H                                                      
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         LA    R3,UP2SEL1H                                                      
*                                                                               
VK110A   LH    R1,2(R3)                                                         
         SR    R0,R0                                                            
         D     R0,=F'80'                                                        
         LR    R0,R1                                                            
         ZIC   R1,BYTE                                                          
         SR    R1,R0                                                            
         STC   R1,BYTE             SAVE OFFSET OF LINE FROM FIRST               
         MH    R1,=Y(L'PINVMINI)                                                
         LA    R1,LOWERTBL(R1)                                                  
         OC    0(L'PINVMINI,R1),0(R1)                                           
         BZ    NO                                                               
         MVC   SEQUENCE,1(R1)                                                   
*                                                                               
         ZIC   R1,BYTE             GET OFFSET OF LINE FROM FIRST                
*                                                                               
         CLI   SCRTYPE,C'N'                                                     
         BNE   *+12                                                             
         MH    R1,=Y(SLN1LEN)                                                   
         B     *+8                                                              
         MH    R1,=Y(SLN2LEN)                                                   
*                                                                               
         CLI   ACTNUM,ACTCHECK                                                  
         BNE   VK120                                                            
         CLI   SCRTYPE,C'N'                                                     
         BNE   *+12                                                             
         LA    R1,CK1SEL0H(R1)                                                  
         B     *+8                                                              
         LA    R1,CK2SEL0H(R1)                                                  
         B     VK120A                                                           
*                                                                               
VK120    CLI   SCRTYPE,C'N'                                                     
         BNE   *+12                                                             
         LA    R1,UP1SEL1H(R1)                                                  
         B     *+8                                                              
         LA    R1,UP2SEL1H(R1)                                                  
*                                                                               
VK120A   LR    R2,R1               R2 = A(SELECT LINE)                          
*                                                                               
         B     VKLOAD                                                           
*                                                                               
VKLOAD   OI    OVLYFLAG,X'80'      COMMENTS SCREEN IS ON                        
*                                                                               
         CLI   SCRTYPE,C'N'                                                     
         BNE   VKLMAG                                                           
*                                                                               
         GOTO1 CALLOV,DMCB,(X'CB',CHKTAGH)   LOAD THE SCREEN UP                 
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         B     VKDISP                                                           
*                                                                               
VKLMAG   GOTO1 CALLOV,DMCB,(X'CA',CHKTAGH)   LOAD THE SCREEN UP                 
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
VKDISP   DS    0H                  REDISPLAY ENTIRE CORRECRTION                 
         ST    R2,DUB                                                           
*                                                                               
         LA    R2,CM1NETH                                                       
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         LA    R2,CM2NETH                                                       
*                                                                               
         MVC   8(11,R2),JOBREC+80   SHOW WHAT THE OPTIONS COL IS                
*                                                                               
         XC    VALSPACE,VALSPACE                                                
         MVI   VALUIND,0                                                        
         ZAP   VALUNITS,=P'0'                                                   
         ZAP   VALCLMS,=P'0'                                                    
         MVI   VALCOSTY,0          ZERO-OUT SO FIELDS WILL BE SHOWN             
         MVI   VALCOSIN,C' '                                                    
         ZAP   VALCOST,=P'0'                                                    
         MVI   VALCL,0                                                          
         ZAP   VALPRCOS,=P'0'                                                   
*                                                                               
         XC    MINEKEY,MINEKEY     DISPDTL NEEDS MINELEM                        
         MVI   MINEKEY,PIMDTLEQ                                                 
         MVC   MINEKEY+1(L'SEQUENCE),SEQUENCE                                   
         BAS   RE,MINIORD                                                       
*                                                                               
         L     R6,MINELEM                                                       
         USING PIMDTLEL,R6                                                      
*                                                                               
         LA    R2,CM1SEL1H                                                      
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         LA    R2,CM2SEL1H                                                      
*                                                                               
         TM    PIMDSTAT,X'10'      MATCHED                                      
         BZ    VKDSP10                                                          
         MVI   8(R2),C'*'          SHOW THE * IF MATCHED                        
         B     VKDSP20                                                          
*                                                                               
VKDSP10  CLI   PIMBLINE,0                                                       
         BE    VKDSP30                                                          
         MVC   8(L'SLN1SEL,R2),=C'->'   SHOW THE -> IF CORRECTED                
*                                                                               
VKDSP20  CLI   SCRTYPE,C'N'                                                     
         BNE   VKDSP25                                                          
         USING SLN1SELH,R2                                                      
         OI    SLN1SELH+6,X'08'    SHOW IN HIGH INTENSITY                       
         OI    SLN1IDTH+6,X'08'                                                 
         OI    SLN1SIZH+6,X'08'                                                 
         OI    SLN1RTEH+6,X'08'                                                 
         OI    SLN1PRMH+6,X'08'                                                 
         OI    SLN1CTPH+6,X'08'                                                 
         OI    SLN1GRSH+6,X'08'                                                 
         OI    SLN1NETH+6,X'08'                                                 
         OI    SLN1ESTH+6,X'08'                                                 
         B     VKDSP30                                                          
*                                                                               
         USING SCRLIN2D,R2                                                      
VKDSP25  OI    SLN2SELH+6,X'08'    SHOW IN HIGH INTENSITY                       
         OI    SLN2IDTH+6,X'08'                                                 
         OI    SLN2SPCH+6,X'08'                                                 
         OI    SLN2CTPH+6,X'08'                                                 
         OI    SLN2GRSH+6,X'08'                                                 
         OI    SLN2NETH+6,X'08'                                                 
         OI    SLN2ESTH+6,X'08'    PROTECT ESTIMATE                             
*                                                                               
VKDSP30  XC    FULL,FULL           SO DISPDTL WILL SHOW DATE                    
*                                                                               
         XC    DUB,DUB                                                          
*                                  PUT THE LINE                                 
         GOTO1 DISPDTL                                                          
         L     R2,DUB                                                           
*                                                                               
         L     R2,ATWA             MUST SET INDICTOR TO XMIT ALL FIELDS         
         LA    R2,64(R2)               OR SCREEN WILL BE MESSED UP              
         CLI   0(R2),0                                                          
         BE    *+16                FIND END OF TWA                              
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         B     *-16                                                             
         MVC   1(2,R2),=X'0101'    SET INDICATOR TO XMIT ALL FIELDS             
         EJECT                                                                  
***********************************************************************         
* SHOW THE COMMENT                                                              
***********************************************************************         
LRC      DS    0H                                                               
         CLI   SCRTYPE,C'N'                                                     
         BNE   LRC00                                                            
         XC    CM1COM1,CM1COM1     CLEAR,                                       
         OI    CM1COM1H+6,X'80'    TRANSMIT,                                    
         OI    CM1COM1H+4,X'20'    AND VALIDATE THE FIELDS                      
         XC    CM1COM2,CM1COM2                                                  
         OI    CM1COM2H+6,X'80'                                                 
         OI    CM1COM2H+4,X'20'                                                 
         XC    CM1COM3,CM1COM3                                                  
         OI    CM1COM3H+6,X'80'                                                 
         OI    CM1COM3H+4,X'20'                                                 
         OI    CM1COM1H+6,X'40'    CURSOR POSITION ON FIRST LINE                
         B     LRC05                                                            
*                                                                               
LRC00    XC    CM2COM1,CM2COM1     CLEAR,                                       
         OI    CM2COM1H+6,X'80'    TRANSMIT,                                    
         OI    CM2COM1H+4,X'20'    AND VALIDATE THE FIELDS                      
         XC    CM2COM2,CM2COM2                                                  
         OI    CM2COM2H+6,X'80'                                                 
         OI    CM2COM2H+4,X'20'                                                 
         XC    CM2COM3,CM2COM3                                                  
         OI    CM2COM3H+6,X'80'                                                 
         OI    CM2COM3H+4,X'20'                                                 
         OI    CM2COM1H+6,X'40'    CURSOR POSITION ON FIRST LINE                
*                                                                               
LRC05    XC    MINEKEY,MINEKEY     GET COMMENT ELEMENT FOR THE DETAIL           
         MVI   MINEKEY,PIMCOMEQ                                                 
         MVC   MINEKEY+1(L'SEQUENCE),SEQUENCE                                   
         BAS   RE,MINIOHI                                                       
         BNE   LRCX                EXIT IF NONE FOR THE DETAIL                  
*                                                                               
         L     R6,MINELEM                                                       
         USING PIMCOMEL,R6                                                      
         CLI   PIMCOMEL,PIMCOMEQ                                                
         BNE   LRCX                                                             
         CLC   PIMCOMS1(L'SEQUENCE),SEQUENCE                                    
         BNE   LRCX                                                             
*                                                                               
         LA    R2,CM1COM1H                                                      
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         LA    R2,CM2COM1H         R2 = A(FIRST COMMENT LINE)                   
*                                                                               
         LA    R3,PIMCOML1         R3 = A(LENGTH OF 1ST COMMENT LINE)           
         LA    RE,PIMCOMTX         RE = A(FIRST COMMENT LINE'S TEXT)            
*                                                                               
LRC10    CLI   0(R3),0             IF NO TEXT FOR THIS LINE                     
         BE    LRC20               THEN CHECK THE NEXT LINE                     
*                                                                               
         ZIC   R1,0(R3)            GET LENGTH OF THE COMMENT LINE               
*                                                                               
         BCTR  R1,0                OTHERWISE DISPLAY IT ON THE LINE             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),0(RE)                                                    
*                                                                               
         LA    R1,1(R1)            RESTORE LENGTH OF COMMENT                    
         AR    RE,R1               RE = A (NEXT COMMENT LINE'S TEXT)            
LRC20    LA    R3,1(R3)            R3 = A(LENGTH OF NEXT COMMENT LINE)          
         AH    R2,=Y(CM1COM2H-CM1COM1H)  R2 = A(NEXT LINE)                      
*                                                                               
         LA    R0,CM1COM3H         IF WE DISPLAY ALL THE LINES                  
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         LA    R0,CM2COM3H                                                      
*                                                                               
         CR    R2,R0                                                            
         BNH   LRC10                                                            
*                                                                               
LRCX     MVI   GERROR1,5           'RECORD WAS CHANGED' MESSAGE                 
         TM    OVLYFLAG,X'40'      COMMENT LINE CHANGED?                        
         BNZ   *+8                 YES                                          
         MVI   GERROR1,4           NO, 'ENTER DATA' MESSAGE                     
*                                                                               
         LA    R2,CM1COM1H                                                      
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         LA    R2,CM2COM1H                                                      
*                                                                               
         NI    OVLYFLAG,X'FF'-X'60'                                             
         B     INFEXIT             LEAVE PROGRAM, INFORMATIONAL MESSAGE         
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE COMMENT LINES                                                    
***********************************************************************         
VR       CLI   PFKEY,12            CAME BACK FROM COMMENTS SCREEN?              
         BNE   VR10                                                             
         MVI   OVLYFLAG,0          NOT IN COMMENTS SCREEN ANYMORE               
         OI    UBITFLAG,X'40'      REDISPLAY PAGE BEFORE COMMENTS               
         MVI   PFKEY,0             CLEAR THE PFKEY                              
         L     R1,ATIOB                                                         
         MVI   TIOBAID-TIOBD(R1),0                                              
         L     R0,AOFLINE          SET CURSOR TO WHERE WE LEFT IT               
         STH   R0,CURDISP                                                       
*                                                                               
         LA    R3,OPTNTABL         R3 = A(OPTIONS TABLE)                        
*                                                                               
         CLI   ACTNUM,ACTCHECK                                                  
         BNE   VR00A                                                            
         LA    R2,CK1NETH          R2 = WHERE '----NET----' IS                  
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         LA    R2,CK2NETH                                                       
         B     VR05                                                             
*                                                                               
VR00A    CLI   ACTNUM,ACTUPDTE                                                  
         BNE   VR00B                                                            
         LA    R2,UP1NETH                                                       
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         LA    R2,UP2NETH                                                       
         B     VR05                                                             
*                                                                               
VR00B    LA    R2,SC1NETH                                                       
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         LA    R2,SC2NETH                                                       
*                                                                               
VR05     CLI   0(R3),X'FF'                                                      
         BNE   VR05D                                                            
*                                                                               
         LA    R2,CHKOPTNH                                                      
         B     INVLFLD             NO LONGER DIE JUST SEND ERROR                
*******  DC    H'0'                OPTION BETTER BE GOOD                        
*                                                                               
VR05D    ZIC   R1,CHKOPTNH+5                                                    
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                DOESN'T MATCH, GET NEXT ENTRY                
         CLC   0(0,R3),CHKOPTN                                                  
         BE    VR05X                                                            
*                                                                               
         LA    R3,OPTNLEN(R3)                                                   
         B     VR05                                                             
*                                                                               
VR05X    MVC   8(11,R2),2(R3)                                                   
         OI    6(R2),X'80'                                                      
         B     VRX                                                              
*                                                                               
VR10     XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,PIMCOMEQ                                                 
         MVC   MINEKEY+1(L'SEQUENCE),SEQUENCE                                   
         BAS   RE,MINIOHI                                                       
         BNE   VR15                COMMENT DIDN'T EXIST FOR THIS DETAIL         
         L     R6,MINELEM                                                       
         CLI   0(R6),PIMCOMEQ      IF COMMENT ELEMENT                           
         BNE   VR15                                                             
         CLC   2(L'SEQUENCE,R6),SEQUENCE     WITH SAME SEQUENCE                 
         BNE   VR15                                                             
         OI    OVLYFLAG,X'20'      THEN COMMENT EXISTED BEFORE                  
*                                                                               
VR15     L     R6,MINELEM                                                       
         XC    0(L'MELEM,R6),0(R6)                                              
         USING PIMCOMEL,R6                                                      
         MVI   PIMCOMEL,PIMCOMEQ                                                
         MVI   PIMCOMLN,PIMCOMOV                                                
         MVC   PIMCOMS1(L'SEQUENCE),SEQUENCE                                    
*                                                                               
         LA    R2,CM1COM1H         R2 = A(FLDHDR FOR 1ST COMMENT LINE)          
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         LA    R2,CM2COM1H                                                      
*                                                                               
         LA    R3,PIMCOML1         R3 = A(LENGTH OF FIRST COMMENT LINE)         
         LA    RE,PIMCOMTX         RE = A(TEXT FOR FIRST COMMENT LINE)          
*                                                                               
VR20     TM    4(R2),X'20'         LINE CHANGED?                                
         BNZ   *+8                                                              
         OI    OVLYFLAG,X'40'      YES, WE'LL NEED TO CHANGE OR ADD             
*                                                                               
         CLI   5(R2),0             ANY DATA IN THIS COMMENT LINE?               
         BE    VR30                                                             
         MVC   0(L'PIMCOML1,R3),5(R2)   YES, COPY INTO ELEMENT                  
*                                                                               
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),8(R2)                                                    
*                                                                               
         LA    R1,1(R1)                                                         
         AR    RE,R1                                                            
VR30     AH    R2,=Y(CM1COM2H-CM1COM1H)                                         
         LA    R3,1(R3)                                                         
*                                                                               
         LA    R0,CM1COM3H                                                      
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         LA    R0,CM2COM3H                                                      
*                                                                               
         CR    R2,R0                                                            
         BNH   VR20                                                             
*                                                                               
         ZIC   R1,PIMCOML1                                                      
         ZIC   R0,PIMCOML2                                                      
         AR    R1,R0                                                            
         ZIC   R0,PIMCOML3                                                      
         AR    R1,R0                                                            
         ZIC   R0,PIMCOMLN                                                      
         AR    R1,R0                                                            
         STC   R1,PIMCOMLN         NEW LENGTH OF ELEMENT                        
*                                                                               
         TM    OVLYFLAG,X'40'      ANY COMMENTS CHANGED?                        
         BZ    VRX                 NO, EXIT                                     
*                                                                               
         OC    PIMCOML1(3),PIMCOML1   ANY DATA AT ALL?                          
         BNZ   VR40                                                             
*                                                                               
         TM    OVLYFLAG,X'20'      COMMENT EXISTED BEFORE?                      
         BZ    VRX                 NO, NOTHING TO DELETE                        
*                                                                               
         GOTO1 MINIO,DMCB,('MINDEL',(R5))   NONE, DELETE THE ELEMENT            
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    MINEKEY,MINEKEY     TAKE OFF THE BIT IN DETAIL ELEMENT           
         MVI   MINEKEY,PIMDTLEQ                                                 
         MVC   MINEKEY+1(L'SEQUENCE),SEQUENCE                                   
         BAS   RE,MINIORD                                                       
         L     R6,MINELEM                                                       
         USING PIMDTLEL,R6                                                      
         NI    PIMDSTAT,X'FF'-X'04'                                             
         BAS   RE,MINIOWRT                                                      
         B     VRX                                                              
*                                                                               
VR40     TM    OVLYFLAG,X'20'      COMMENT EXISTED BEFORE?                      
         BZ    VR50                NO, NOTHING TO DELETE                        
*                                                                               
         BAS   RE,MINIOWRT                                                      
         B     VRX                                                              
*                                                                               
VR50     BAS   RE,MINIOADD                                                      
*                                                                               
         XC    MINEKEY,MINEKEY     TAKE OFF THE BIT IN DETAIL ELEMENT           
         MVI   MINEKEY,PIMDTLEQ                                                 
         MVC   MINEKEY+1(L'SEQUENCE),SEQUENCE                                   
         BAS   RE,MINIORD                                                       
         L     R6,MINELEM                                                       
         USING PIMDTLEL,R6                                                      
         OI    PIMDSTAT,X'04'                                                   
         BAS   RE,MINIOWRT                                                      
*                                                                               
VRX      TM    OVLYFLAG,X'80'                                                   
         BNZ   LRC                                                              
         B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE READS A MINIO ELEMENT.  MINEKEY MUST BE SET BY CALLER            
***********************************************************************         
MINIORD  NTR1                                                                   
         GOTO1 MINIO,DMCB,('MINRD',(R5))                                        
         CLI   MINERR,0                                                         
         BE    XIT                                                              
         DC    H'0'                DIE ON ANY ERROR                             
         SPACE 2                                                                
***********************************************************************         
* THIS ROUTINE READ HIGH A MINIO ELEMENT.  MINEKEY MUST BE SET BY               
* THE CALLER.                                                                   
***********************************************************************         
MINIOHI  NTR1                                                                   
         GOTO1 MINIO,DMCB,('MINHI',(R5))                                        
         CLI   MINERR,0            RETURN 'YES' IF NO ERRORS                    
         BE    YES                                                              
         CLI   MINERR,MINEEOF      RETURN 'NO' IF END-OF-FILE                   
         BE    NO                                                               
         DC    H'0'                DIE ON ANY OTHER ERROR                       
         SPACE 2                                                                
***********************************************************************         
* THIS ROUTINE READ SEQUENTIAL FOR A MINIO ELEMENT.                             
***********************************************************************         
MINIOSEQ NTR1                                                                   
         GOTO1 MINIO,DMCB,('MINSEQ',(R5))                                       
         CLI   MINERR,0            RETURN 'YES' IF NO ERRORS                    
         BE    YES                                                              
         CLI   MINERR,MINEEOF      RETURN 'NO' IF END-OF-FILE                   
         BE    NO                                                               
         DC    H'0'                DIE ON ANY OTHER ERROR                       
         SPACE 2                                                                
***********************************************************************         
* THIS ROUTINE WRITES OUT A MINIO ELEMENT.  MINELEM MUST BE SET                 
***********************************************************************         
MINIOWRT NTR1                                                                   
         OI    MNIOFLAG,X'80'      REMEMBER TO CLOSE MINIO FILE                 
         GOTO1 MINIO,DMCB,('MINWRT',(R5))                                       
         CLI   MINERR,0                                                         
         BE    XIT                                                              
         DC    H'0'                DIE ON ANY ERROR                             
         SPACE 2                                                                
***********************************************************************         
* THIS ROUTINE ADDS A MINIO ELEMENT.  MINELEM MUST BE SET BY THE CALLER         
***********************************************************************         
MINIOADD NTR1                                                                   
         OI    MNIOFLAG,X'80'      REMEMBER TO CLOSE MINIO FILE                 
         GOTO1 MINIO,DMCB,('MINADD',(R5))                                       
         CLI   MINERR,0                                                         
         BE    XIT                                                              
         CLI   MINERR,MINEDUP      DUPLICATE KEY?                               
         BE    NO                  YES, RETURN A NO                             
         DC    H'0'                DIE ON ANY ERROR                             
         EJECT                                                                  
***********************************************************************         
*                ERROR MESSAGES AND LEFTOVERS                                   
***********************************************************************         
RELO     DS    A                                                                
MISSFLD  MVI   GERROR1,MISSING                                                  
         B     ERREXIT                                                          
*                                                                               
INVLFLD  MVI   GERROR1,INVALID                                                  
         B     ERREXIT                                                          
*                                                                               
ERREXIT  MVI   GMSGTYPE,C'E'                                                    
         GOTO1 MYERR                                                            
*                                                                               
INFEXIT  MVI   GMSGTYPE,C'I'                                                    
         GOTO1 MYERR                                                            
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
***********************************************************************         
* OPTIONS TABLE                                                                 
***********************************************************************         
OPTNTABL DS    0C                                                               
         DC    C'A.',CL11'--ADCODE---'                                          
OPTNNTRY DC    C'B.',CL11'-BILLDATE--'                                          
         DC    C'C.',CL11'----CD-----'                                          
         DC    C'G.',CL11'---GROSS---'                                          
         DC    C'GL',CL11'---GLCD----'                                          
         DC    C'GS',CL11'----GST----'                                          
         DC    C'L.',CL11'--LASTIO---'                                          
         DC    C'N.',CL11'----NET----'                                          
         DC    C'NL',CL11'---NLCD----'                                          
         DC    C'P.',CL11'--PAYDATE--'                                          
         DC    C'T.',CL11'----TAX----'                                          
         DC    C'Z.',CL11'ZONE,EDTN  '                                          
*                                                                               
         DC    X'FF'                                                            
*                                                                               
OPTNLEN  EQU   OPTNNTRY-OPTNTABL                                                
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE PPMATWORKD        (SYSTEM AREAS)                               
         EJECT                                                                  
       ++INCLUDE PPMATWK02D        (PPMAT02'S WORK AREA)                        
         EJECT                                                                  
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* FATIOB                                                                        
* SCREENS                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
       ++INCLUDE FATIOB                                                         
       ++INCLUDE PPMATFFD          (BASE SCREEN FOR SYSTEM)                     
         ORG   CONTAGH                                                          
       ++INCLUDE PPMATFBD          (OUR CHECK SCREEN)                           
         ORG   CHKTAGH                                                          
       ++INCLUDE PPMATEED          (OUR COMMENT SCREEN)                         
         ORG   CHKTAGH                                                          
       ++INCLUDE PPMATEDD          (OUR COMMENT SCREEN)                         
         ORG   CHKTAGH                                                          
       ++INCLUDE PPMATEBD          (OUR COMMENT SCREEN)                         
         ORG   CHKTAGH                                                          
       ++INCLUDE PPMATECD          (OUR COMMENT SCREEN)                         
         ORG   CHKTAGH                                                          
       ++INCLUDE PPMATDBD          (OUR COMMENT SCREEN)                         
         ORG   CHKTAGH                                                          
       ++INCLUDE PPMATDCD          (OUR COMMENT SCREEN)                         
         ORG   CHKTAGH                                                          
       ++INCLUDE PPMATCBD          (OUR COMMENT SCREEN)                         
         ORG   CHKTAGH                                                          
       ++INCLUDE PPMATCAD          (OUR COMMENT SCREEN)                         
         EJECT                                                                  
* DDGENTWA                                                                      
* DDPERVALD                                                                     
* DDMINBLK                                                                      
* AND MORE ...                                                                  
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDMINBLK                                                       
       ++INCLUDE BUYDSECTS                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'035PPMAT03   05/01/02'                                      
         END                                                                    
