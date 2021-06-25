*          DATA SET NESFM20    AT LEVEL 052 AS OF 10/31/05                      
*PHASE T31C20A                                                                  
         TITLE 'T31C20  NETWORK INTEGRATION RECORD MAINTENANCE'                 
T31C20   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T31C20                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         LA    R7,1(RB)            R7-2ND BASE REGISTER                         
         LA    R7,4095(R7)                                                      
         USING T31C20,RB,R7                                                     
         SPACE 3                                                                
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
         CLI   MODE,PRINTREP       PRINT RECORDS                                
         BE    LR                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*     VALIDATE KEY ROUTINE                                                      
* BUILD KEY IN SVKEY / MOVE TO KEY AT EXIT                                      
*                                                                               
         SPACE                                                                  
VK       DS    0H                                                               
         LA    R6,SVKEY                                                         
         USING NIKEY,R6                                                         
         XC    SVKEY,SVKEY                                                      
         MVI   SVKEY,X'0A'                                                      
         SPACE                                                                  
         LA    R2,SFMMEDH             * MEDIA                                   
         GOTO1 VALIMED                                                          
         MVC   NIKAM,BAGYMD                                                     
         SPACE                                                                  
         MVI   NOPTFLG,1                                                        
         MVI   ERROR,MISSING                                                    
         LA    R2,SFMNTWKH            * NETWORK                                 
         GOTO1 VALIFLD                                                          
         BNZ   VK5                                                              
         CLI   ACTNUM,ACTLIST           IN LIST/NTWK IS OPTIONAL                
         BE    VK10                                                             
         B     TRAPERR                                                          
VK5      GOTO1 VALINTWK                                                         
         MVC   NIKNET,QNET                                                      
         SPACE                                                                  
VK10     LA    R2,SFMEFFH            * EFFECTIVE DATE                           
         GOTO1 VALIFLD                                                          
         BNZ   VK12                                                             
         CLI   ACTNUM,ACTLIST            IN LIST EFF DATE IS OPTIONAL           
         BE    VKX                                                              
         B     TRAPERR                                                          
VK12     GOTO1 VALIDAT                                                          
         GOTO1 DATCON,DMCB,(0,QDATE),(5,SAVEDAT)       MMMDD/YY                 
         XC    FULL,FULL                                                        
         GOTO1 DATCON,DMCB,(0,QDATE),(3,FULL+1)        BINARY                   
         L     R1,FULL                                                          
         LCR   R1,R1                      COMPLEMENT EFFECTIVE DATE             
         ST    R1,FULL                                                          
         MVC   NIKEFFDT,FULL+1                                                  
         SPACE                                                                  
VKX      DS    0H                                                               
         LA    R2,SFMMEDH                                                       
         MVC   KEY,SVKEY                                                        
         B     EXIT                                                             
         EJECT                                                                  
* DISPLAY KEY                                                                   
         SPACE                                                                  
DK       DS    0H                                                               
         L     R6,AIO                                                           
         USING NIKEY,R6                                                         
         MVI   SFMMED,C'N'                   * MEDIA                            
         FOUT  SFMMEDH                                                          
         SPACE                                                                  
         MVC   SFMNTWK,NIKNET                * NETWORK                          
         FOUT  SFMNTWKH                                                         
         SPACE                                                                  
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SFMEFF,2(R6)                 * EFFECTIVE DATE STORED             
         FOUT  SFMEFFH                         IN PRINTABLE FORMAT              
         DROP  R6                                                               
         B     EXIT                                                             
         EJECT                                                                  
* VALIDATE RECORD                                                               
VR       DS    0H                                                               
*                                                                               
         CLI   ACTNUM,ACTADD           SINCE DATE ELEM IS                       
         BNE   VR3                     PART OF KEY,SKIP UNLESS ADD              
         LA    R6,ELEM                                                          
         MVI   ELCODE,X'02'            ADD DATE ELEMENT                         
         GOTO1 REMELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   ELEM,X'02'              SET ELEM CODE                            
         MVI   ELEM+1,X'0A'            SET ELEM LENGTH                          
         MVC   ELEM+2(8),SAVEDAT       MOVE IN MMMDD/YY DATE                    
         GOTO1 ADDELEM                                                          
*                                                                               
VR3      LA    R6,ELEM                 ADD MAIN ELEMS                           
         USING NIEL,R6                                                          
         MVI   ELCODE,X'03'                                                     
         GOTO1 REMELEM                                                          
         MVI   MYBYT2,0           SET SCREEN INPUT BYTE TO 0                    
         MVI   MYBYTE,1           SET SEQ CNTR TO 1                             
         MVI   DEFAULT,0                                                        
         LA    R2,SFMDAYH         GET 1ST SCREEN LINE                           
*                                                                               
DOELEM   XC    ELEM,ELEM                                                        
         LA    R5,23(R6)           POINT R5 AT START OF PROG IN ELEM            
         MVI   ELEM,X'03'             SET ELEM CODE                             
         MVI   ELEM+1,X'17'           SET ELEM LENGTH = 23 + PROG CODES         
         MVC   ELEM+2(1),MYBYTE       SET SEQ NUMBER                            
         MVI   ERROR,INVDAY                                                     
         CLI   5(R2),0                    * DAY                                 
         BE    VR4B                NO INPUT = ALL = (SEQ=7)                     
         CLC   =C'ALL',8(R2)                                                    
         BNE   VR4C                                                             
VR4B     MVI   NIELSEQ,8                                                        
         B     VR5                                                              
VR4C     ZIC   R3,5(R2)                                                         
         GOTO1 DAYVAL,DMCB,((R3),8(R2)),BYTE,WORK                               
         CLI   BYTE,0                                                           
         BE    TRAPERR                                                          
         MVI   NIELSEQ,1                                                        
         CLI   BYTE,X'40'          IF IT'S NOT SPECIFIC DAY MON,TUE             
         BNH   *+8                                                              
         MVI   NIELSEQ,4           THEN SET SEQ TO 4                            
         MVC   NIELDAY,BYTE                                                     
         MVI   MYBYT2,1            TURN ON  SCREEN INPUT BYTE                   
*                                                                               
VR5      DS    0H                                                               
         MVI   ERROR,INVALID                                                    
         ZIC   R1,0(R2)                   * DAYPART                             
         AR    R2,R1                                                            
         CLI   5(R2),0                                                          
         BNE   VR5B                                                             
         ZIC   R1,NIELSEQ                                                       
         LA    R1,2(R1)                                                         
         STC   R1,NIELSEQ                                                       
         B     VR10                                                             
VR5B     DS    0H                                                               
         GOTO1 VALIDPT,DMCB                                                     
         MVC   NIELDPT,QDPT        SET DAYPART                                  
         MVC   NIELDPTA,QDPT2                                                   
         MVI   MYBYT2,1            TURN ON  SCREEN INPUT BYTE                   
*                                                                               
VR10     DS    0H                                                               
         MVI   ERROR,INVTIME                                                    
         ZIC   R1,0(R2)            * TIME                                       
         AR    R2,R1                                                            
         CLI   5(R2),0                                                          
         BNE   VR10B                                                            
         ZIC   R1,NIELSEQ                                                       
         LA    R1,1(R1)                                                         
         STC   R1,NIELSEQ                                                       
         B     VR15                                                             
VR10B    ZIC   R3,5(R2)                                                         
         GOTO1 TIMVAL,DMCB,((R3),8(R2)),NIELTIME                                
         BE    TRAPERR                                                          
         MVI   MYBYT2,1            TURN ON  SCREEN INPUT BYTE                   
         CLI   0(R1),X'FF'                                                      
*                                                                               
VR15     DS    0H                   * PROGRAM EXCEPTIONS                        
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         CLI   5(R2),0                                                          
         BE    VR30                                                             
         L     R4,AIO2                 AIO2=SCANNER OUT BLOCK                   
         GOTO1 SCANNER,DMCB,(R2),(R4)                                           
         ZIC   R3,4(R1)                                                         
         LTR   R3,R3              R3=NUM OF PROGS ON ONE LINE                   
         BZ    SCANERR                                                          
VR17     BAS   RE,PROGREC          VALIDATE PROG NAME                           
         MVC   0(6,R5),12(R4)      OK/SET PROG NAME IN ELEM                     
         ZIC   R1,ELEM+1           BUMP ELEM LENGTH                             
         LA    R1,6(R1)                                                         
         STC   R1,ELEM+1                                                        
         LA    R4,32(R4)           BUMP SCAN OUT BLOCK                          
         LA    R5,6(R5)            BUMP ELEM                                    
         BCT   R3,VR17                                                          
         MVI   MYBYT2,1            TURN ON  SCREEN INPUT BYTE                   
         MVI   NIELSEQ,10                                                       
*                                                                               
VR30     DS    0H                                                               
         MVI   ERROR,MISSING       * INTEGRATION RATE                           
         LA    R5,9(R6)                  SET R5 TO ELEM INTEG RATE              
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         CLI   5(R2),0                                                          
         BNE   VR32                                                             
         CLI   MYBYT2,1            HAS ANYTHING ON LINE BEEN SET                
         BE    TRAPERR             IF YES/ERROR - MUST HAVE A RATE              
         CLI   MYBYTE,1            IS IT FIRST LINE                             
         BE    TRAPERR             YES/ERROR-1ST LINE MUST HAVE INPUT           
         B     VR34                   NO/SKIP                                   
VR32     MVI   ERROR,INVALID                                                    
         ZIC   R3,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,8(R2),(R3)                                          
         CLI   0(R1),X'FF'                                                      
         BE    TRAPERR                                                          
         CLC   4(4,R1),=4X'00'                                                  
         BE    TRAPERR                                                          
         MVC   0(4,R5),4(R1)             SET INTG RATE TO ELEM                  
*                                                                               
         CLI   MYBYT2,1            HAS ANYTHING ON LINE BEEN SET                
         BE    *+12                                                             
         MVI   DEFAULT,C'Y'        NO/SO THIS IS DEFAULT RATE                   
         MVI   NIELSEQ,11                                                       
*                                                                               
         GOTO1 ADDELEM                                                          
         ZIC   R1,MYBYTE           ADD TO ELEM SEQ                              
         LA    R1,1(R1)                                                         
         STC   R1,MYBYTE                                                        
VR34     MVI   MYBYT2,0                                                         
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),9             TEST END OF SCREEN                           
         BH    DOELEM                                                           
*                                                                               
VRX      DS    0H                                                               
         CLI   DEFAULT,C'Y'                                                     
         BE    EXIT                                                             
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(31),=C'*** ERROR *** - NO DEFAULT RATE'                  
         LA    R2,SFMDAYH                                                       
         B     ERRX                                                             
*                                                                               
SCANERR  DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(36),=C'*** ERROR IN PROGRAM EXCEPTION FIELD'             
ERRX     GOTO1 ERREX2                                                           
         EJECT                                                                  
********************************************************                        
* READ PROGRAM RECORD TO VALIDATE PROGRAM EXCEPTION NAME                        
*      EXPECTS R4 TO POINT TO PROG CODE                                         
*                                                                               
PROGREC  NTR1                                                                   
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D20'                                                  
         MVC   KEY+2(1),BAGYMD                                                  
         MVC   KEY+3(2),QNETMKT                                                 
         MVC   KEY+5(6),12(R4)      PROG CODE                                   
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=CL8'DMRDHI',=CL8'SPTDIR',KEY,KEY,0                 
         CLC   KEY(11),KEYSAVE                                                  
         BNE   PRGERR                                                           
         MVC   KEY,SVKEY                                                        
         B     EXIT                                                             
*                                                                               
PRGERR   DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(11),=C'*** ERROR -'                                      
         MVC   CONHEAD+12(6),12(R4)                  MOVE IN PROG NAME          
         MVC   CONHEAD+19(22),=C'-HAS NO PROGRAM RECORD'                        
         GOTO1 ERREX2                                                           
         EJECT                                                                  
* DISPLAY RECORD                                                                
         SPACE                                                                  
DR       DS    0H                        *  CLEAR SCREEN                        
         LA    R2,SFMDAYH                                                       
DR2      ZIC   R1,0(R2)                                                         
         SH    R1,=H'8'                                                         
         BCTR  R1,0                                                             
         EX    R1,DROC                                                          
         EX    R1,DRCLC                                                         
         BE    DR5                                                              
         EX    R1,DRXC                                                          
         FOUT  (R2)                                                             
         B     DR5                                                              
DROC     OC    8(0,R2),SPACES                                                   
DRCLC    CLC   8(0,R2),SPACES                                                   
DRXC     XC    8(0,R2),8(R2)                                                    
*                                                                               
DR5      ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),9                                                          
         BH    DR2                                                              
         SPACE                                                                  
*                                                                               
DR7      LA    R2,SFMDAYH                 * DAY                                 
         L     R6,AIO                                                           
         USING NIEL,R6                                                          
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
DR10     CLI   NIELDAY,0                                                        
         BE    DR12                                                             
         GOTO1 UNDAY,DMCB,NIELDAY,8(R2)                                         
         FOUT  (R2)                                                             
DR12     DS    0H                  * DAYPART                                    
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         CLI   NIELDPT,0                                                        
         BE    DR14                                                             
         MVC   8(1,R2),NIELDPT                                                  
         OC    NIELDPTA,NIELDPTA                                                
         BZ    *+10                                                             
         MVC   8(2,R2),NIELDPTA                                                 
         FOUT  (R2)                                                             
DR14     DS    0H                  * TIME                                       
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         OC    NIELTIME,NIELTIME                                                
         BZ    DR16                                                             
         GOTO1 UNTIME,DMCB,NIELTIME,8(R2)                                       
         FOUT  (R2)                                                             
DR16     DS    0H                  * PROGRAM EXCEPTIONS                         
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         ZIC   R5,NIELLEN                TEST ELEM LENGTH                       
         SH    R5,=H'23'              NOT OVER 23 = NO PROG EXCEPTS             
         LTR   R5,R5                                                            
         BZ    DR20                                                             
*                                                                               
         SR    R4,R4              R5 / 6(LENGTH OF PROGS)=NUM OF PROGS          
         D     R4,=F'6'                                                         
         LTR   R4,R4                                                            
         BZ    *+6                                                              
         DC    H'0'               MUST BE AN EVEN DIVIDE/NO REMAINDERS          
         LA    R3,NIELPROG        R3 POINTS TO PROG IN ELEM                     
         LA    R4,8(R2)           R4 POINTS TO SCREEN FIELD AREA                
DR18     MVC   0(6,R4),0(R3)                                                    
         CH    R5,=H'1'           ARE THERE MORE PROGS FOR THE LINE             
         BE    DR18C              NO/SO SKIP SETTING COMMA                      
         CLI   0(R4),C'A'                                                       
         BL    GOTEND                                                           
         LA    R4,1(R4)                                                         
         B     *-12                                                             
GOTEND   MVI   0(R4),C','                                                       
         LA    R4,1(R4)            BUMP SCREEN FIELD                            
         LA    R3,6(R3)            BUMP ELEM                                    
         BCT   R5,DR18                                                          
DR18C    FOUT  (R2)                                                             
*                                                                               
DR20     DS    0H                  * INTEGRATION RATE                           
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         OC    NIELRATE,NIELRATE                                                
         BZ    DR20A                                                            
*        DS    H'0'           LET'S NOT DUMP                                    
         MVC   FULL,NIELRATE                                                    
         EDIT  (B4,FULL),(6,8(R2)),2                                            
         FOUT  (R2)                                                             
*                                                                               
DR20A    ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),9                 TEST END OF SCREEN                       
         BNH   DRX                                                              
         BAS   RE,NEXTEL                                                        
         BE    DR10                                                             
*                                                                               
DRX      DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
* LIST RECORDS *                                                                
         SPACE                                                                  
LR       DS    0H                                                               
         CLI   MODE,PRINTREP                                                    
         BE    LRX                ********8 FOR NOW ********                    
         LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
*        LA    R1,HDRTN                                                         
*        ST    R1,HEADHOOK                                                      
LR02     MVI   NLISTS,X'0F'        SET NUM OF LIST LINES                        
         LA    R3,KEY                                                           
         USING NIKEY,R3                                                         
         SPACE                                                                  
         OC    KEY(20),KEY                                                      
         BNZ   *+10                                                             
         MVC   KEY,SVKEY                                                        
         GOTO1 HIGH                                                             
         B     LR22                                                             
         SPACE                                                                  
LR20     GOTO1 SEQ                                                              
         SPACE                                                                  
LR22     DS    0H                                                               
         CLC   KEY(7),SVKEY               ID/AM                                 
         BNE   LRX                                                              
         CLI   SVKEY+7,0                                                        
         BE    LR22A                                                            
         CLC   KEY+7(4),SVKEY+7            NETWORK                              
         BNE   LRX                                                              
LR22A    CLI   SVKEY+11,0                                                       
         BE    LR22B                                                            
         CLC   KEY+11(3),SVKEY+11          EFFECTIVE DATE                       
         BNE   LR20                                                             
LR22B    DS    0H                                                               
         SPACE                                                                  
         GOTO1 GETREC                                                           
         SPACE                                                                  
         LA    R5,LISTAR                                                        
         XC    LISTAR,LISTAR                                                    
         L     R6,AIO                                                           
         USING NIKEY,R6                                                         
         MVC   5(4,R5),NIKNET                                                   
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   11(8,R5),2(R6)      DATE IN PRINT FORM                           
         GOTO1 LISTMON                                                          
         B     LR20                GOTO READ SEQ                                
*                                                                               
LRX      DS    0H                                                               
         B     EXIT                                                             
*        DROP  R3,R4,R6                                                         
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
HEADING  DS    0H                                                               
         SSPEC H1,3,REQUESTOR                                                   
         SSPEC H1,46,C'NETWORK INTEGRATION RECORDS'                             
         SSPEC H2,46,C'---------------------------'                             
         SSPEC H1,93,AGYNAME                                                    
         SSPEC H2,93,AGYADD                                                     
         SSPEC H3,93,REPORT                                                     
         SSPEC H4,93,RUN                                                        
         SSPEC H5,103,PAGE                                                      
         DC    X'00'                                                            
*                                                                               
         EJECT                                                                  
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
         B     EXIT                                                             
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDFLDIND                                                       
         EJECT                                                                  
       ++INCLUDE NESFMFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NESFMF3D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE NESFMF4D                                                       
         EJECT                                                                  
       ++INCLUDE NESFMWORKD                                                     
         ORG   SYSSPARE                                                         
*                           *******  T31C20 WORK AREA  *******                  
WORKAREA DS    0CL100                                                           
SAVEDAT  DS    CL8                                                              
MYBYTE   DS    CL1                                                              
MYBYT2   DS    CL1                                                              
DEFAULT  DS    CL1                                                              
         EJECT                                                                  
       ++INCLUDE NEGENINTG                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'052NESFM20   10/31/05'                                      
         END                                                                    
