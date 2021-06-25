*          DATA SET NESFM17    AT LEVEL 013 AS OF 11/09/18                      
*PHASE T31C17B,+0                                                               
         TITLE 'NESFM17 -  NADDEF REC'                                          
         PRINT NOGEN                                                            
T31C17   CSECT                                                                  
         NMOD1 0,T31C17                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASUBSYSD                                                      
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         SPACE                                                                  
         MVC   AIO,AIO1                                                         
         L     R7,AIO1                                                          
         USING NNDRECD,R7                                                       
         SPACE 3                                                                
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BNE   VKCHK                                                            
         BAS   RE,VK                                                            
         B     RESET                                                            
VKCHK    CLI   MODE,VALREC         VALIDATE RECORD                              
         BNE   DKCHK                                                            
         BAS   RE,VR                                                            
         B     RESET                                                            
DKCHK    CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BNE   DRCHK                                                            
         BAS   RE,DK                                                            
         B     RESET                                                            
DRCHK    CLI   MODE,DISPREC        DISPLAY RECORD                               
         BNE   LRCHK                                                            
         BAS   RE,DR                                                            
         B     RESET                                                            
LRCHK    CLI   MODE,LISTRECS       LIST RECORDS                                 
         BNE   PRCHK                                                            
         BAS   RE,LR                                                            
         B     RESET                                                            
PRCHK    CLI   MODE,PRINTREP       PRINT RECORDS                                
         BNE   EXIT1                                                            
         BAS   RE,LR                                                            
         B     RESET                                                            
*                                                                               
RESET    MVC   KEY,SVKEY                                                        
         B     EXIT                                                             
*                                                                               
EXIT     GOTO1 VSETSPT                                                          
EXIT1    OI    CONSERVH+6,X'01'                                                 
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
DEMDEF   EQU   23                  DEMDEF RECORD EQUATE                         
COMDEF   EQU   65                  COMDEF RECORD EQUATE                         
*                                                                               
DK       NTR1                                                                   
*                                                                               
         MVC   NADCDE,NNDCODE      MOVE NAD CODE                                
         FOUT  NADCDEH                                                          
         B     EXIT                                                             
         EJECT                                                                  
DR       NTR1                                                                   
*                                                                               
*-- CLEAR SCREEN                                                                
         LA    R2,NADFRSTH+13                                                   
         LA    R6,DMAXQ            MAX NUMBER OF NAD ENTRIES                    
*                                                                               
DR20     MVI   5(R2),0                                                          
         FOUT  (R2),SPACES,1                                                    
         ZIC   RF,0(R2)            NEXT FIELD                                   
         AR    R2,RF                                                            
         MVI   5(R2),0                                                          
         FOUT  (R2),SPACES,11                                                   
         ZIC   RF,0(R2)            NEXT FIELD                                   
         AR    R2,RF                                                            
         ZIC   RF,0(R2)            NEXT FIELD                                   
         AR    R2,RF                                                            
         BCT   R6,DR20                                                          
*                                                                               
         LA    R5,DBLOCKA          SETUP DBLOCKS FOR DEMOCON                    
         USING DBLOCKD,R5                                                       
         LA    R4,NADFRSTH         POINT TO FIRST TWA HEADER                    
         ZIC   RF,0(R4)                                                         
         AR    R4,RF               NEXT FIELD                                   
         L     R6,AIO              POINT TO RECORD IO AREA                      
         USING NNDELDD,R6                                                       
         MVI   ELCODE,NNDELQ       SET ELEMENT CODE                             
         BAS   RE,GETEL            GET THE ELEMENT                              
         BNE   DRXIT               EXIT IF NONE ARE FOUND                       
*                                                                               
         LA    R2,DMAXQ            MAX NUMBER OF NAD ENTRIES                    
*********                                                                       
* DECODE THE DEMO                                                               
         XC    DUB,DUB                                                          
DR30     DS    0H                                                               
         CLI   RECNUM,COMDEF       COMSCORE DEMDEF?                             
         JNE   DR40                                                             
         MVI   8(R4),C'T'          IMPRESSIONS                                  
         ZIC   RF,0(R4)                                                         
         AR    R4,RF                                                            
         MVC   8(8,R4),NNDCDEMO                                                 
         J     DR50                                                             
*                                                                               
DR40     GOTO1 VDEMCON1,DMCB,(1,NNDCAT),('DEMOCON_16',DUB),(R5),DUB+3           
*********                                                                       
         MVI   8(R4),C'V'                                                       
         CLI   DUB+1,C'I'                                                       
         BE    *+10                                                             
         MVC   8(1,R4),DUB+1       GET THE MODIFIER                             
         FOUT  (R4)                                                             
         ZIC   RF,0(R4)                                                         
         AR    R4,RF               NEXT FIELD                                   
*                                                                               
         XC    DBLOCK,DBLOCK       NEEDED BY DEMOCON                            
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELMED,C'N'                                                    
         MVC   DBFILE,=CL3'NAD'                                                 
********DR30     DS    0H                                                       
*                                                                               
         MVI   DUB+1,C'I'          SET MODIFIER TO IMPRESSION                   
**********                                                                      
* ENCODE THE DEMO                                                               
         GOTO1 VDEMCON1,DMCB,(1,DUB),('DEMOCON_17',WORK),(R5),DUB+3             
**********                                                                      
         GOTO1 VDEMCON1,DMCB,(1,WORK),(9,8(R4)),(0,(R5))                        
         FOUT  (R4)                OUTPUT TO SCREEN                             
DR50     ZIC   RF,0(R4)                                                         
         AR    R4,RF               NEXT FIELD                                   
         ZIC   RF,0(R4)                                                         
         AR    R4,RF               NEXT FIELD                                   
         BAS   RE,NEXTEL           GET NEXT ELEMENT                             
         BNE   DRXIT                                                            
         BCT   R2,DR30             CONTROL LOOP                                 
*        MVI   8(R4),C'V'                                                       
*        CLI   NNDMOD,C'I'                                                      
*        BE    *+10                                                             
*        MVC   8(1,R4),NNDMOD      GET THE MODIFIER                             
*        FOUT  (R4)                                                             
*        ZIC   RF,0(R4)                                                         
*        AR    R4,RF               NEXT FIELD                                   
*        BCT   R2,DR30             CONTROL LOOP                                 
DRXIT    B     EXIT                                                             
         DROP  R5,R6                                                            
         EJECT                                                                  
VK       NTR1                      VALIDATE KEY                                 
*                                                                               
         XC    SVKEY,SVKEY                                                      
         MVC   SVKEY(2),=X'0D16'                                                
*                                                                               
         LA    R2,WORK             * MEDIA                                      
         XC    0(8,R2),0(R2)                                                    
         MVI   8(R2),C'N'          SET UP DUMMY HEADER                          
         GOTO1 VALIMED                                                          
         MVC   SVKEY+2(1),BAGYMD                                                
*                                                                               
VK10     DS    0H                                                               
*                                                                               
         MVC   SVKEY+3(6),NADCDE                                                
*                                                                               
         CLI   RECNUM,COMDEF                                                    
         BNE   *+8                                                              
         MVI   SVKEY+9,NNDKRSCQ    RATING SERVICE COMSCORE                      
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
VR       NTR1                                                                   
         BAS   RE,GETTOKEN         GET COMSCORE LICENSE                         
*                                                                               
         MVC   NNDCODE,NADCDE                                                   
*                                                                               
         CLI   RECNUM,COMDEF                                                    
         BNE   *+8                                                              
         MVI   NNDKRS,NNDKRSCQ     RATING SERVICE COMSCORE                      
*                                                                               
         MVI   NNDMAINL,X'01'      ACTIVITY ELEMENT                             
         MVI   NNDELN,X'08'                                                     
         GOTO1 DATCON,DMCB,(5,WORK2),(3,NNDACTD)                                
         CLI   ACTNUM,ACTADD                                                    
         BNE   VR05                IS ACTION ADD?                               
         MVI   NNDACT,C'A'         YES, ACTION=ADD                              
         BE    VR07                                                             
VR05     DS    0H                                                               
*                                                                               
         CLI   ACTNUM,ACTCHA       NO, ARE WE CHANGING?                         
         BNE   VR07                                                             
         MVI   NNDACT,C'C'         YES, ACTION=CHANGE                           
         MVI   ELCODE,NNDELQ       DELETE ALL OLD ELEMENTS FIRST                
         BAS   RE,DELEL                                                         
VR07     DS    0H                                                               
         BAS   RE,EACTIV           ADD EXTENDED ACTIVITY ELEMENT                
*                                                                               
         LA    R2,DMAXQ            NOW WE HAVE TO VALIDATE 39                   
         LA    R4,NADFRSTH           FIELDS WITH NAD CATEGORIES                 
         ZIC   RF,0(R4)                                                         
         AR    R4,RF               POINT TO INPUT FIELD                         
         LA    R5,DBLOCKA          SETUP DBLOCKS FOR DEMOCON                    
         USING DBLOCKD,R5                                                       
*                                                                               
VR10     DS    0H                                                               
         CLI   RECNUM,COMDEF                                                    
         BNE   VR10B                                                            
         LA    RF,NADDE21H         ONLY 20 DEMOS FOR COMDEF                     
         CR    R4,RF               MORE THAN 20 DEMOS?                          
         BL    VR10B               NO - KEEP GOING                              
         CLI   5(R4),0             YES - ANYTHING IN THIS FIELD?                
         BNE   VR10A               YES - THEN ERROR                             
         ZIC   RF,0(R4)            NO - GO TO THE NEXT FIELD                    
         AR    R4,RF                                                            
         ZIC   RF,0(R4)                                                         
         AR    R4,RF                                                            
         ZIC   RF,0(R4)                                                         
         AR    R4,RF                                                            
         B     VR25                                                             
*                                                                               
VR10A    LR    R2,R4                                                            
         B     CDMAXERR                                                         
*                                                                               
VR10B    BAS   RE,VALTYPE          VALIDATE TYPE (MODIFIER)                     
         ZIC   RF,0(R4)                                                         
         AR    R4,RF               POINT TO INPUT FIELD                         
*                                                                               
         CLI   5(R4),0             NO ENTRY, BUMP TO NEXT                       
         BE    VR20                                                             
*                                                                               
         CLI   RECNUM,COMDEF                                                    
         BNE   *+12                                                             
         CLI   8(R4),C'X'                                                       
         BNE   BADDEM                                                           
*                                                                               
         L     R1,AIO2              DBLOCK IS USING AIO2                        
         AHI   R1,1024              DEMOVAL PARAM5 USING AIO2+1024              
         USING P5XD,R1                                                          
         XC    P5XID(P5XDLNQ),P5XID                                             
         MVC   P5XID,=C'P5X '                                                   
         LA    RE,CSLIC                                                         
         ST    RE,P5XLICNS          A(32 BYTE COMSCORE LICENSE)                 
         ST    R1,DMCB+16           EXTENDED PARAM5                             
         OI    DMCB+16,X'80'        SO DEMOVAL KNOWS EXTENDED BLOCK             
         DROP  R1                                                               
*                                                                               
         XC    DBLOCK,DBLOCK       NEEDED BY DEMOVAL                            
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELMED,C'N'                                                    
         MVC   DBFILE,=CL3'NAD'                                                 
         L     R1,ACOMFACS                                                      
         USING COMFACSD,R1                                                      
         MVC   DEMOVAL,CDEMOVAL                                                 
         DROP  R1                                                               
         XC    CSCODE,CSCODE                                                    
         GOTO1 DEMOVAL,DMCB,(1,(R4)),(1,WORK),(0,(R5)),0,,CSCODE                
         CLI   4(R1),0                                                          
         BE    BADDEM              BAD DEMO-CATEGORY                            
*  TVQ CATEGORY NOT ALLOWED                                                     
         CLI   WORK,171                                                         
         BE    BADDEM                                                           
*                                                                               
         CLI   RECNUM,COMDEF       COMSCORE DEMDEF?                             
         JNE   VR12                                                             
         OC    CSCODE,CSCODE       COMSCORE DEMO?                               
         JZ    BADDEM                                                           
         J     VR14                                                             
*                                                                               
VR12     OC    CSCODE,CSCODE       COMSCORE DEMO?                               
         JNZ   BADDEM                                                           
*                                                                               
VR14     LA    R3,ELEM                                                          
         USING NNDELDD,R3                                                       
         XC    ELEM,ELEM           PREPARE ELEMENT TO BE ADDED                  
         MVI   NNDELEM,NNDELQ                                                   
         MVI   NNDLEN,NNDLENQ      ELEMENT LENGTH                               
*                                                                               
         CLI   RECNUM,COMDEF       COMSCORE DEMDEF?                             
         JNE   VR14A                                                            
         MVC   NNDMOD,BYTE         MOVE IN MODIFIER                             
         MVC   NNDCDEMO,CSCODE     SAVE COMSORE DEMO                            
         J     VR15                                                             
*****************************                                                   
VR14A    LA    R5,DBLOCKA          SETUP DBLOCKS FOR DEMOCON                    
         USING DBLOCKD,R5                                                       
         XC    DBLOCK,DBLOCK       NEEDED BY DEMOCON                            
         MVC   DBCOMFCS,ACOMFACS                                                
* DECODE THE DEMO                                                               
         GOTO1 VDEMCON1,DMCB,(1,WORK),('DEMOCON_16',DUB),              X        
               (R5),DUB+3                                                       
*****************************                                                   
         MVC   NNDCAT(3),DUB       STORE COMPRESSED NAD CATEGORY                
         MVI   NNDCAT+1,C'I'       NO MODIFIER ALLOWED                          
         MVC   NNDMOD,BYTE         MOVE IN MODIFIER                             
         CLI   NNDMOD,C'R'         CHECK FOR RATING                             
         BNE   *+14                                                             
         CLC   NNDCAT(3),=XL3'00D901'    CHECK FOR HOMES                        
         BNE   BADDEM                                                           
         CLI   NNDMOD,C'V'         CHECK FOR VPH                                
         BNE   *+12                                                             
         CLI   NNDCAT,1                                                         
         BNH   BADDEM                                                           
* ENCODE THE DEMO                                                               
         GOTO1 VDEMCON1,DMCB,(1,NNDCAT),('DEMOCON_17',NNDCAT),         X        
               (R5),DUB+3                                                       
         DROP  R3                                                               
VR15     BAS   RE,PUTEL                                                         
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
VR20     DS    0H                                                               
*                                                                               
         ZIC   RF,0(R4)                                                         
         AR    R4,RF               NEXT FIELD                                   
         ZIC   RF,0(R4)                                                         
         AR    R4,RF               NEXT FIELD                                   
VR25     BCT   R2,VR10             LOOP                                         
*                                                                               
*        LA    R3,32(R7)           POINT TO FIRST ELEMENT, ADD                  
         LR    R6,R7                                                            
         MVI   ELCODE,NNDELQ                                                    
         BAS   RE,GETEL            GET FIRT DD ELEMENT                          
         BNE   VRXIT                                                            
         LR    R3,R6                                                            
VR30     CLI   0(R3),NNDELQ        CHECK FOR DUPLICATE ENTRIES                  
         BNE   VRXIT                                                            
         CLI   NNDLENQ(R3),X'DD'                                                
         BNE   VRXIT                                                            
*        CLC   3(4,R3),14(R3)                                                   
         CLC   3(NNDLENQ-3,R3),NNDLENQ+3(R3)                                    
         BE    VR40                                                             
         LA    R3,NNDLENQ(R3)      BUMP TO NEXT ELEMENT ENTRY                   
         B     VR30                                                             
VR40     DS    0H                                                               
*                                                                               
         LA    R2,DMAXQ            FIND POSITION OF DUP DEMO                    
         CLI   RECNUM,COMDEF                                                    
         BNE   *+8                                                              
         LA    R2,CDMAXQ           MAX NUMBER OF COMSCORE ENTRIES               
*                                                                               
         LA    R4,NADFRSTH                                                      
         ZIC   RF,0(R4)                                                         
         AR    R4,RF               POINT TO INPUT FIELD                         
*                                                                               
VR50     DS    0H                                                               
         CLI   5(R4),0             NO ENTRY, BUMP TO NEXT                       
         BE    VR60                                                             
         CLC   8(1,R4),4(R3)       SEE IF MODIFIERS MATCH                       
         BNE   VR60                                                             
         MVC   BYTE,8(R4)          SAVE MODIFIER                                
         ZIC   RF,0(R4)                                                         
         AR    R4,RF               POINT TO DEMO FIELD                          
*                                                                               
         OC    CSCODE,CSCODE       COMSCORE DEMO?                               
         JZ    VR55                                                             
         CLC   CSCODE,NNDCDEMO-NNDELEM(R3)                                      
         JNE   VR60                                                             
         J     DUPDEM                                                           
*                                                                               
VR55     XC    DBLOCK,DBLOCK       NEEDED BY DEMOVAL                            
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELMED,C'N'                                                    
         MVC   DBFILE,=CL3'NAD'                                                 
         L     R1,ACOMFACS                                                      
         USING COMFACSD,R1                                                      
         MVC   DEMOVAL,CDEMOVAL                                                 
         DROP  R1                                                               
         GOTO1 DEMOVAL,DMCB,(1,(R4)),(1,WORK),(0,(R5))                          
         CLI   4(R1),0                                                          
         BE    BADDEM              BAD DEMO-CATEGORY                            
         MVC   WORK+1(1),BYTE      PUT MODIFIER IN PLACE                        
         CLC   WORK(3),3(R3)                                                    
         BE    DUPDEM              POINT TO DUPLICATE DEMO                      
         B     VR70                                                             
*                                                                               
VR60     DS    0H                                                               
         ZIC   RF,0(R4)                                                         
         AR    R4,RF               NEXT FIELD                                   
VR70     ZIC   RF,0(R4)                                                         
         AR    R4,RF               NEXT FIELD                                   
         ZIC   RF,0(R4)                                                         
         AR    R4,RF               NEXT FIELD                                   
         BCT   R2,VR50             LOOP                                         
*                                                                               
VRXIT    DS    0H                                                               
         BAS   RE,DR                                                            
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
*  SET DEMO TYPE                                                                
*  R4 = SCREEN HEADER                                                           
VALTYPE  NTR1                      VALIDATE KEY                                 
         LR    R2,R4                                                            
         LA    RE,DTYPTAB                                                       
*                                                                               
         CLI   5(R4),0             NO ENTRY, EXIT                               
         BNE   VALT050                                                          
         ZIC   RF,0(R4)                                                         
         AR    RF,R4                                                            
         CLI   5(RF),0                                                          
         BE    EXIT                                                             
         B     INVERR                                                           
*                                                                               
VALT050  CLI   0(RE),X'FF'                                                      
         BE    INVERR                                                           
         CLC   8(1,R4),0(RE)                                                    
         BE    VALT100                                                          
         LA    RE,1(RE)                                                         
         B     VALT050                                                          
*                                                                               
VALT100  MVC   BYTE,8(R4)                                                       
         CLI   RECNUM,COMDEF       COMSCORE DEMDEF?                             
         JNE   EXIT                                                             
         CLI   BYTE,C'T'                                                        
         BNE   INVERR                                                           
         B     EXIT                                                             
*                                                                               
DTYPTAB  DC    CL3'VRT'                                                         
         DC    XL1'FF'                                                          
         EJECT                                                                  
*                                                                               
* ADD/UPDATE EXTENDED ACTIVITY ELEMENT                                          
EACTIV   NTR1                                                                   
         XC    ELEM,ELEM                                                        
         USING NNDEL0A,ELEM                                                     
         MVI   NND0ACD,NND0ACDQ    ELEMENT CODE                                 
         MVI   NND0ALN,NND0ALNQ    ELEMENT LENGTH                               
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(3,NND0ALDT)  LAST CHANGE DATE                 
         CLI   ACTNUM,ACTADD                                                    
         BNE   *+10                                                             
         MVC   NND0ACDT,NND0ALDT   CREATION DATE                                
*                                                                               
         TIME  TU                                                               
         STCM  R0,15,NND0ATIM      TIME STAMP                                   
*                                                                               
         LR    R6,R7                                                            
         MVI   ELCODE,NND0ACDQ                                                  
         BAS   RE,GETEL                                                         
         BNE   *+16                                                             
         MVC   NND0ACDT,NND0ACDT-NNDEL0A(R6)  COPY CREATION DATE                
         MVC   NND0ACPD,NND0ACPD-NNDEL0A(R6)  COPY CREATION PID                 
*                                                                               
         L     RF,ACOMFACS         INITIALIZE SECRET BLOCK                      
         L     RF,CSECRET-COMFACSD(RF)                                          
         LH    R5,=Y(SVSECRET-T31CFFD)                                          
         AR    R5,RA                                                            
         GOTO1 (RF),DMCB,('SECPINIT',(R5)),0                                    
         BNE   EACT20              ERRORS WITH SECRET                           
         USING SECD,R5                                                          
         MVC   NND0ALPD,SECPID     LAST CHANGE PID                              
         CLI   ACTNUM,ACTADD                                                    
         BNE   *+10                                                             
         MVC   NND0ACPD,NND0ALPD   CREATION PID                                 
         DROP  R5                                                               
*                                                                               
EACT20   BAS   RE,DELEL            DELETE OLD EXTENDED ACTIVITY ELEM            
         BAS   RE,PUTEL            ADD NEW EXTENDED ACTIVITY ELEM               
         B     EXIT                                                             
*                                                                               
*                                                                               
LR       NTR1                                                                   
*                                                                               
         MVI   NLISTS,X'0F'                                                     
         OC    KEY(13),KEY         IS KEY ALL NULLS?                            
         BNZ   *+10                NO, DO A READ HIGH                           
         MVC   KEY,SVKEY           YES, MOVE IN SAVED KEY                       
         GOTO1 VSETSPT                                                          
         GOTO1 HIGH                                                             
         B     LR20                                                             
*                                                                               
LR10     GOTO1 VSETSPT                                                          
         GOTO1 SEQ                                                              
*                                                                               
LR20     DS    0H                                                               
         CLC   KEY(3),SVKEY                ID/AM                                
         BNE   LRX                                                              
*                                                                               
         CLI   RECNUM,COMDEF       COMSCORE DEMDEF?                             
         JE    LR25                                                             
         CLI   KEY+9,NNDKRSCQ      RATING SERVICE COMSCORE                      
         JE    LR10                                                             
         J     LR30                                                             
*                                                                               
LR25     CLI   RECNUM,COMDEF       COMSCORE DEMDEF?                             
         JNE   *+12                                                             
         CLI   KEY+9,NNDKRSCQ      RATING SERVICE COMSCORE                      
         JNE   LR10                                                             
*                                                                               
*                                                                               
LR30     DS    0H                                                               
         GOTO1 GETREC                                                           
*                                                                               
         CLI   MODE,PRINTREP                                                    
         BE    PR                                                               
*                                                                               
         LA    R5,LISTAR           PREPARE A LIST LINE                          
         USING LLINED,R5                                                        
         XC    LISTAR,LISTAR                                                    
         MVC   LRCODE,NNDCODE                                                   
*                                                                               
LR40     GOTO1 LISTMON                                                          
         B     LR10                GOTO READ SEQ                                
*                                                                               
LRX      B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
PR       DS    0H                  PRINT THE LINE                               
*                                                                               
         MVI   NFILE,C'T'          STATION FILE                                 
         LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
         LA    R1,HDRTN                                                         
         ST    R1,HEADHOOK                                                      
*                                                                               
         LA    R4,P+10                                                          
         USING PLINED,R4                                                        
*                                                                               
         MVC   PRCODE,NNDCODE                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     LR10                                                             
         DROP  R4                                                               
         EJECT                                                                  
* SUB-ROUTINES FOR ELEMENT MAINTENANCE                                          
*                                                                               
DELEL    LR    R0,RE                                                            
         GOTO1 HELLO,DMCB,(C'D',=C'SPTFILE'),(ELCODE,0(R7)),0                   
         LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 2                                                                
PUTEL    LR    R0,RE                                                            
         GOTO1 HELLO,DMCB,(C'P',=C'SPTFILE'),0(R7),ELEM                         
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
HEADING  DS    0H                                                               
         SSPEC H1,3,REQUESTOR                                                   
         SSPEC H1,42,C'NETWORK NAD RECORDS'                                     
         SSPEC H2,42,C'-------------------'                                     
         SSPEC H1,93,AGYNAME                                                    
         SSPEC H2,93,AGYADD                                                     
         SSPEC H3,93,REPORT                                                     
         SSPEC H4,93,RUN                                                        
         SSPEC H5,103,PAGE                                                      
         DC    X'00'                                                            
*                                                                               
HDRTN    NTR1                                                                   
*                                                                               
         LA    R2,H8+10                                                         
         USING PLINED,R2                                                        
         MVC   PRCODE(8),=C'NAD CODE'                                           
         MVC   PRCODE+132(13),=13C'-'                                           
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
* GET COMSCORE USER TOKEN                                                       
***********************************************************************         
GETTOKEN NTR1                                                                   
         XC    CSLIC,CSLIC         CLEAR THE COMSCORE USER TOKEN                
*                                                                               
         TM    USRIDFLG,USRRNTKQ   DID WE FIND THE TOKEN EARLIER?               
         JZ    GTOKENX             NO WE DIDN'T, THEN NO NEED                   
         L     RA,ATWA                                                          
         USING T31CFFD,RA          BASE SCREEN FOR SYSTEM + THIS PROG           
         MVC   HALF,TWAAGY         SAVE OFF THE AGENCY ALPHA                    
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CT5KEY,R4                                                        
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,TWAAGY                                                  
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD '),=C'CTFILE ',KEY,AIO3                
         L     R4,AIO3                                                          
         LA    R6,CT5DATA          POINT TO THE FIRST ELEMENT IN REC            
         USING CTSEAD,R6                                                        
         CLI   0(R6),CTSEAELQ      ALREADY ON SECURITY ALPHA ELEM?              
         JE    GTOKEN10            YES                                          
         MVI   ELCODE,CTSEAELQ     NO, FIND SECURITY ALPHA ELEM                 
         BAS   RE,NEXTEL                                                        
         JNE   *+10                                                             
GTOKEN10 MVC   HALF,CTSEAAID       GET SECURITY AGENCY ALPHA AS WELL            
         DROP  R6                                                               
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING TOKRECD,R4          BUILD TOKEN RECORD KEY                       
         MVI   TOKKMIN,TOKKMINQ                                                 
         MVI   TOKKTYP,TOKKRTRK                                                 
         MVC   TOKKSAGY,HALF       SECURITY AGENCY ALPHA                        
         MVC   TOKKAAGY,TWAAGY     AND AGENCY ALPHA                             
         MVI   TOKKSYS,X'03'       NET SYSTEM                                   
         MVC   KEYSAVE,KEY                                                      
                                                                                
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI '),=C'GENDIR ',KEY,KEY                 
         CLC   KEY(L'TOKKEY),KEYSAVE                                            
         JNE   GTOKENX                                                          
                                                                                
         L     R4,AIO3                                                          
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'GENFIL ',KEY+36,(R4),WORK             
                                                                                
         AHI   R4,TOKFIRST                                                      
         USING RTAUTHD,R4                                                       
         CLI   0(R4),RTAUTELQ      X'0A' ELEMENT                                
         JNE   GTOKENX                                                          
         OC    RTAUTID(L'RTAUTID+L'RTAUTSEC),RTAUTID                            
         JZ    GTOKENX                                                          
         OI    USRIDFLG,USRRNTKQ   HAS ACCESS TO RENTRAK DEMOS                  
         LR    RE,R9                                                            
         MVC   CSLIC,RTAUTID       SAVE USER TOKEN FOR DEMOVAL                  
*                                                                               
GTOKENX  DS    0H                                                               
         J     EXIT                                                             
         DROP  R4                                                               
*                                                                               
INVERR   MVI   ERROR,INVALID                                                    
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
*                                                                               
BADDEM   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'DEMOMSG),DEMOMSG                                       
         LR    R2,R4               POINT TO OFFENDING FIELD                     
         GOTO1 ERREX2                                                           
*                                                                               
CDMAXERR XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'CDMAXMSG),CDMAXMSG                                     
         LR    R2,R4               POINT TO OFFENDING FIELD                     
         GOTO1 ERREX2                                                           
*                                                                               
DEMOMSG  DC    C'INVALID DEMOGRAPHIC CATEGORY'                                  
CDMAXMSG DC    C'ONLY 20 DEMOS ALLOWED'                                         
*                                                                               
DUPDEM   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'DUPMSG),DUPMSG                                         
         LR    R2,R4               POINT TO OFFENDING FIELD                     
         GOTO1 ERREX2                                                           
*                                                                               
DUPMSG   DC    C'DUPLICATE DEMOGRAPHIC CATEGORY'                                
*                                                                               
         GETEL (R6),DATADISP,ELCODE                                             
*                                                                               
DMAXQ    EQU   39                  MAX NUMBER OF DEMOS NIELSEN                  
CDMAXQ   EQU   20                  MAX NUMBER OF DEMOS COMSCORE                 
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
PLINED   DSECT                                                                  
PRCODE   DS    CL6                                                              
         SPACE 2                                                                
LLINED   DSECT                                                                  
LRCODE   DS    CL6                                                              
         EJECT                                                                  
       ++INCLUDE DEDEMEQUS2                                                     
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE SPGENNAD                                                       
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE FASECRETD                                                      
*                                                                               
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDFLDIND                                                       
       ++INCLUDE GEGENTOK                                                       
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DEDEMOVALD                                                     
         EJECT                                                                  
       ++INCLUDE NESFMFFD                                                       
         ORG   CONHEADH-64+BASETWA2                                             
*SECOND SAVE STORAGE AREA                                                       
SVAREA   DS    0H                                                               
SVSECRET DS    CL1024                                                           
SVOFFBLK DS    CL100               OFICCER BLOCK                                
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE NESFMD8D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE NESFMD9D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
*                                                                               
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
         EJECT                                                                  
       ++INCLUDE NEGENUSER                                                      
         EJECT                                                                  
       ++INCLUDE NESFMWORKD                                                     
         ORG   SYSSPARE                                                         
*                           *******  T31C17 WORK AREA  *******                  
WORK2    DS    CL64                                                             
DBLOCKA  DS    CL256                                                            
DEMOVAL  DS    A                                                                
CSCODE   DS    CL8                                                              
CSLIC    DS    CL32                COMSCORE LICENSE                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013NESFM17   11/09/18'                                      
         END                                                                    
