*          DATA SET NERES08    AT LEVEL 004 AS OF 05/01/02                      
*PHASE T32108A,*                                                                
*INCLUDE NETUNBK                                                                
*INCLUDE GETBROAD                                                               
T32108   TITLE '-   CORRECTED PROGRAMS LISTING'                                 
T32108   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**MSGF**,RA,RR=R2                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R2,RELO             SAVE PROGRAM RELOCATION FACTOR               
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VKEY                                                             
         CLI   MODE,VALREC         *** VALREC ** = DISPLAY RECORD               
         BE    DSP                                                              
         CLI   MODE,LISTRECS       *** VALREC ** = DISPLAY RECORD               
         BE    VKEY                                                             
         B     XIT                                                              
         EJECT                                                                  
*********************************************************************           
*VKEY -  VALIDATE BOOK                                                          
*********************************************************************           
VKEY     DS    0H                                                               
         XC    KEY,KEY                                                          
         XC    BK,BK               SET TO DEFAULT = READ HI ON BOOK             
         CLI   MSGBKH+5,0                                                       
         BE    VKEYX                                                            
*                                                                               
VKEY10   ZIC   R1,MSGBKH+5                                                      
         BCTR  R1,0                                                             
         EXCLC R1,MSGBK,=C'LATEST'                                              
         BE    VKEYX                                                            
*                                                                               
VKEY20   DS    0H                                                               
         LA    R2,MSGBKH                                                        
         GOTO1 ANY                                                              
         GOTO1 DATVAL,DMCB,(0,WORK),DUB                                         
         CLI   DMCB+3,0                                                         
         BNE   VKEY30                                                           
VKEYBD   MVC   CONHEAD(L'INVDAT),INVDAT     BAD DATE IN KEY                     
         MVI   ERROR,X'FE'                                                      
         GOTO1 VERRXIT                                                          
         B     VKEYX                                                            
*                                                                               
VKEY30   GOTO1 NETWEEK,DMCB,DUB,GETDAY,ADDAY                                    
         MVC   BK(1),4(R1)         YEAR                                         
         MVC   BK+1(1),8(R1)       WEEK                                         
         XC    BK,=X'FFFF'         BOOK IN REV ORDER ON FILE                    
*                                                                               
VKEYX    DS    0H                                                               
         CLI   MODE,LISTRECS                                                    
         BE    DSP                                                              
         B     XIT                                                              
*********************************************************************           
*DSP  -  DISPLAY MESSAGE FILE RECORD                                            
*********************************************************************           
DSP      DS    0H                                                               
*                                                                               
         LA    R2,MSGLN1H          CLEAR ALL MSG LINES                          
         LA    R0,MSGLNXH                                                       
DSP05    XC    8(L'MSGLN1,R2),8(R2)                                             
         OI    6(R2),X'80'                                                      
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               BUMP TO NEXT LINE                            
         CR    R2,R0                                                            
         BNH   DSP05                                                            
*                                                                               
         XC    KEE,KEE                                                          
         LA    R6,KEE                                                           
         USING PRKEY,R6                                                         
         MVC   PRCODE(8),=C'PNNMSG1D'  READ MESSAGE FILE RECD                   
         MVC   PRBOOK,BK           BOOK REQUESTED FROM MSG                      
         OC    PRBOOK,PRBOOK                                                    
         BZ    DSP10                                                            
         BAS   RE,RDPAV            READ SPECF BOOK ON PAVFIL                    
         BE    DSP20               RECD FOUND                                   
         XC    PRBOOK,PRBOOK       ELSE READ HI FOR LATEST BOOK                 
*                                                                               
DSP10    BAS   RE,HIPAV            READ LATEST BOOK                             
         BNE   DSPX                NO MSG RECDS FOUND                           
*                                                                               
DSP20    LA    R6,IO               PT TO RECORD                                 
         MVC   BK,PRBOOK           ACTUAL BOOK OF RECD READ IN                  
         XC    BK,=X'FFFF'                                                      
         L     RF,=V(GETBROAD)                                                  
         A     RF,RELO                                                          
         ST    RF,GETBRD                                                        
         L     RF,=V(NETUNBK)                                                   
         A     RF,RELO                                                          
         GOTO1 (RF),DMCB,(C'W',BK),DATE,GETDAY,ADDAY,GETBRD                     
         GOTO1 DATCON,DMCB,(0,DATE),(8,MSGBK)                                   
         OI    MSGBKH+6,X'80'      TRANSMIT FIELD                               
*                                                                               
         LA    R5,MSGLN1H          1ST DATA LINE                                
         LA    R6,PRFRSTEL         DISP TO 1ST ELEMENT                          
         CLI   0(R6),0             EOR?                                         
         BE    DSPX                NOTHING IN RECD                              
         CLI   0(R6),X'01'         MKT ELEMENT?                                 
         BNE   DSP30                                                            
         USING MARELEM,R6          PROCESS SAVED DATES IN 'MKT' ELEM            
         MVC   MSGCDT,SPACES                                                    
         GOTO1 DATCON,DMCB,(2,MARDATE),(8,MSGCDT)                               
*                                                                               
         DS    0H                                                               
         LA    R4,MSGCDT+10        BUMP PAST DATE TO DISPLAY TIME               
         GOTO1 HEXOUT,DMCB,MARTYPE,WORK,2                                       
         PACK  DUB,WORK(2)                                                      
         CVB   R1,DUB                                                           
         LA    R1,8(R1)            ADD 8 HOURS                                  
         STC   R1,MYWORK                                                        
         MVI   6(R4),C'M'                                                       
         MVI   5(R4),C'A'                                                       
         CLI   MYWORK,12                                                        
         BL    DSP25                                                            
         BE    *+12                                                             
         SH    R1,=H'12'           ADJUST OUT OF MILITARY TIME                  
         STC   R1,MYWORK                                                        
         MVI   5(R4),C'P'                                                       
*                                                                               
DSP25    MVI   2(R4),C':'                                                       
         MVC   3(2,R4),WORK+2      MINUTES                                      
         EDIT  (1,MYWORK),(2,0(R4))                                             
         OI    MSGCDTH+6,X'80'                                                  
         B     DSP50               BUMP TO NEXT ELEM                            
*                                                                               
DSP30    CLI   0(R6),X'06'         MSG LINE?                                    
         BNE   DSPX                NO, EOF                                      
         USING MSGELEM,R6                                                       
         MVC   8(L'MSGLN1,R5),MSGLINE                                           
         OI    6(R5),X'80'         TRANSMIT                                     
         ZIC   R0,0(R5)            BUMP NEXT SCREEN LOACATION                   
         AR    R5,R0                                                            
         LA    R1,MSGLNX                                                        
         CR    R5,R1               PAST END OF SCREEN?                          
         BH    DSPX                YES, DON'T DISPLAY ANY MORE ELEMS            
*                                                                               
DSP50    ZIC   R0,1(R6)            BUMP TO NEXT ELEM                            
         AR    R6,R0                                                            
         B     DSP30               NEXT ELEM (NOT GONNA BE MKT)                 
*                                                                               
DSPX     DS    0H                                                               
         OI    MSGBKH+6,X'40'      POSN CURSOR ON BK FIELD                      
         B     XIT                                                              
         EJECT                                                                  
*********************************************************************           
*              FILE I/O ROUTINES                                                
*********************************************************************           
*                                                                               
HIPAV    NTR1                                                                   
         MVC   COMMAND,DMRDHI                                                   
         MVC   KEESAVE,KEE                                                      
         B     IO20                                                             
RDPAV    NTR1                                                                   
         MVC   COMMAND,DMREAD                                                   
         MVC   KEESAVE,KEE                                                      
         B     IO20                                                             
*                                                                               
SQ       NTR1                                                                   
         MVC   COMMAND,DMRSEQ                                                   
         MVC   KEESAVE,KEE                                                      
*                                                                               
IO20     EQU   *                                                                
         GOTO1 DATAMGR,DMCB,(1,COMMAND),PAVDIR,KEE,IO                           
         CLI   8(R1),0             TEST EOF OR NOT FOUND                        
         BNE   IOEXIT                                                           
         LA    RE,IO                                                            
         USING PRKEY,RE                                                         
         MVC   NESTATUS,PRKSTAT                                                 
         CLC   IO(8),KEESAVE       DID WE READ A MSG FILE RECD?                 
         BNE   IOEXIT              NO                                           
         MVC   KEE,IO              READ PAVFIL                                  
         OC    KEE+19(4),KEY+19    TEST IF INDEX D/A PRESENT                    
         BZ    IOEXIT                                                           
         XC    IO(L'KEE),IO                                                     
         MVC   IO(18),KEE                                                       
         LA    RE,IO                                                            
         MVC   PRRSTAT,NESTATUS                                                 
         DROP  RE                                                               
         MVC   COMMAND,DMRDHI                                                   
         GOTO1 DATAMGR,DMCB,(1,COMMAND),PAVFIL,KEE+19,IO                        
         CLI   8(R1),0             SET CC                                       
         BNE   IOEXIT                                                           
         CLC   IO(12),KEE                                                       
*                                                                               
IOEXIT   XIT1                                                                   
*                                                                               
         SPACE 3                                                                
XIT      XIT1                                                                   
         SPACE 1                                                                
MYEND    MVI   ERROR,X'FE'         USING MY OWN ERROR MSG                       
         SPACE 1                                                                
ERREND   GOTO1 VERRXIT                                                          
         SPACE 1                                                                
BUMP     ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BR    RE                                                               
         SPACE 1                                                                
RELO     DS    A                                                                
         SPACE 1                                                                
PATCH    DS    0H                  PATCH AREA                                   
         DC    XL32'00'                                                         
INVDAT   DC    C'** ERROR ** INVALID DATE   '                                   
         EJECT                                                                  
*              LITERAL POOL                                                     
         SPACE 3                                                                
         LTORG                                                                  
*                                                                               
*              CONSTANTS                                                        
*                                                                               
PAVDIR   DC    C'NTIDIR  '                                                      
PAVFIL   DC    C'NTIFIL  '                                                      
DMRDHI   DC    C'DMRDHI  '                                                      
DMREAD   DC    C'DMREAD  '                                                      
DMRSEQ   DC    C'DMRSEQ  '                                                      
DMWRT    DC    C'DMWRT   '                                                      
DMOPEN   DC    C'DMOPEN  '                                                      
*                                                                               
EFFS     DC    XL50'FF'                                                         
NESTATUS DS    C                                                                
*                                                                               
         EJECT                                                                  
*              DEDEMFILE                                                        
*        PRINT OFF                                                              
       ++INCLUDE DEDEMFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
*              NERESALL                                                         
*        PRINT OFF                                                              
       ++INCLUDE NERESALL1                                                      
         PRINT ON                                                               
         SPACE 1                                                                
*              DSECT TO COVER SCREEN                                            
         SPACE 1                                                                
         ORG   CONTAGH                                                          
       ++INCLUDE NERESF8D                                                       
         EJECT                                                                  
*              LOCAL WORKING STORAGE                                            
         SPACE 3                                                                
SYSD     DSECT                                                                  
         ORG   OVWORK              LOCAL WORKING STORAGE                        
DATE     DS    CL6                                                              
BK       DS    XL2                                                              
MYWORK   DS    CL30                                                             
KEE      DS    CL24                                                             
KEESAVE  DS    CL24                                                             
*                                                                               
GETBRD   DS    A                                                                
DUMMY    DS    CL2                                                              
         DS    CL(L'OVWORK-(*-OVWORK)) SPARE                                    
*                                                                               
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004NERES08   05/01/02'                                      
         END                                                                    
