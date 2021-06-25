*          DATA SET NEPUP14    AT LEVEL 049 AS OF 05/01/02                      
*PHASE T32214A,*                                                                
         TITLE 'T32214 - QSET MAINTENANCE'                                      
T32214   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T32214**,RA                                                    
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING SYSD,R9             SYSTEM SPECIFIC WORK                         
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7                                                   
         SPACE 1                                                                
*                                                                               
         CLI   MODE,VALKEY                                                      
         BE    VKEY                                                             
         CLI   MODE,VALREC                                                      
         BE    VREC                                                             
*        CLI   MODE,SETFILE                                                     
*        BE    SFIL                                                             
         CLI   MODE,DISPKEY                                                     
         BE    DKEY                                                             
         CLI   MODE,DISPREC                                                     
         BE    DREC                                                             
         CLI   MODE,LISTRECS                                                    
         BE    LR                                                               
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE KEY FIELDS                                              
         SPACE 3                                                                
VKEY     LA    R2,PUPCLIH          CLIENT                                       
         GOTO1 VVALCLT                                                          
         MVC   PUPCLIN,CLTNAME                                                  
         OI    PUPCLINH+6,X'80'                                                 
         SPACE 1                                                                
         LA    R2,PUPYEARH         YEAR                                         
         CLI   ACTNUM,ACTLIST                                                   
         BNE   *+12                                                             
         CLI   5(R2),0                                                          
         BE    VKEY20                                                           
*                                                                               
         CLI   5(R2),2                                                          
         BNE   INVYEAR                                                          
         SPACE 1                                                                
VKEY20   LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         USING NQURECD,R4                                                       
         MVI   NQUKTYPE,X'25'                                                   
         MVC   NQUKAM,BINAGYMD                                                  
         MVC   NQUKCLT,CLTCOMP                                                  
         MVC   NQUYEAR,PUPYEAR                                                  
         MVC   SVKEY,KEY                                                        
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE RECORD                                                  
         SPACE 3                                                                
VREC     L     R4,AIO                                                           
         USING NQURECD,R4                                                       
*                                                                               
         MVI   NQUAEL,X'01'                                                     
         MVI   NQUALEN,50                                                       
*                                                                               
         LA    R2,PUPQ4SDH         QUARTER 4 START CHECK                        
         CLI   5(R2),0                                                          
         BE    INVINPT                                                          
         GOTO1 DATVAL,DMCB,(0,PUPQ4SD),WORK                                     
         OC    0(4,R1),0(R1)                                                    
         BZ    INVINPT                                                          
         GOTO1 GETDAY,DMCB,WORK,FULL                                            
         CLC   FULL(3),=CL3'MON'                                                
         BNE   MONERR                                                           
         GOTO1 DATCON,DMCB,(0,WORK),(2,NQUA4ST)                                 
*                                                                               
         LA    R2,PUPQ4DTH         QUARTER 4 END CHECK                          
         CLI   5(R2),0                                                          
         BE    INVINPT                                                          
         GOTO1 DATVAL,DMCB,(0,PUPQ4DT),DUB                                      
         OC    0(4,R1),0(R1)                                                    
         BZ    INVINPT                                                          
         GOTO1 GETDAY,DMCB,DUB,FULL                                             
         CLC   FULL(3),=CL3'SUN'                                                
         BNE   SUNERR                                                           
         GOTO1 DATCON,DMCB,(0,DUB),(2,NQUA4END)                                 
*                                                                               
         LA    R2,PUPQ1DTH         QUARTER 1 END CHECK                          
         CLI   5(R2),0                                                          
         BE    INVINPT                                                          
         GOTO1 DATVAL,DMCB,(0,PUPQ1DT),DUB                                      
         OC    0(4,R1),0(R1)                                                    
         BZ    INVINPT                                                          
         GOTO1 GETDAY,DMCB,DUB,FULL                                             
         CLC   FULL(3),=CL3'SUN'                                                
         BNE   SUNERR                                                           
         GOTO1 DATCON,DMCB,(0,DUB),(2,NQUA1END)                                 
*                                                                               
         LA    R2,PUPQ2DTH         QUARTER 2 END CHECK                          
         CLI   5(R2),0                                                          
         BE    INVINPT                                                          
         GOTO1 DATVAL,DMCB,(0,PUPQ2DT),DUB                                      
         OC    0(4,R1),0(R1)                                                    
         BZ    INVINPT                                                          
         GOTO1 GETDAY,DMCB,DUB,FULL                                             
         CLC   FULL(3),=CL3'SUN'                                                
         BNE   SUNERR                                                           
         GOTO1 DATCON,DMCB,(0,DUB),(2,NQUA2END)                                 
*                                                                               
         LA    R2,PUPQ3DTH         QUARTER 3 END CHECK                          
         CLI   5(R2),0                                                          
         BE    INVINPT                                                          
         GOTO1 DATVAL,DMCB,(0,PUPQ3DT),WORK+6                                   
         OC    0(4,R1),0(R1)                                                    
         BZ    INVINPT                                                          
         GOTO1 GETDAY,DMCB,WORK+6,FULL                                          
         CLC   FULL(3),=CL3'SUN'                                                
         BNE   SUNERR                                                           
         GOTO1 DATCON,DMCB,(0,WORK+6),(2,NQUA3END)                              
*                                                                               
*--FIRST QUARTER GREATER THEN FOURTH                                            
         LA    R2,PUPQ4DTH                                                      
         CLC   NQUA4END,NQUA1END                                                
         BH    RANGERR                                                          
*                                                                               
*--SECOND QUARTER GREATER THEN FIRST                                            
         LA    R2,PUPQ1DTH                                                      
         CLC   NQUA1END,NQUA2END                                                
         BH    RANGERR                                                          
*                                                                               
*--THIRD QUARTER GREATER THEN SECOND                                            
         LA    R2,PUPQ2DTH                                                      
         CLC   NQUA2END,NQUA3END                                                
         BH    RANGERR                                                          
*                                                                               
*--RANGE FROM FOURTH START TO THIRD END MUST BE LESS THEN ONE YEAR              
         LA    R2,PUPQ4SDH                                                      
*                                                                               
         CLC   WORK(2),WORK+6      COMPARE YEARS                                
         BE    VR300               SAME YEARS - ALWAYS OK                       
         PACK  DUB,WORK(2)                                                      
         CVB   R4,DUB                                                           
         PACK  DUB,WORK+6(2)                                                    
         CVB   R5,DUB                                                           
         SR    R5,R4                                                            
         CH    R5,=H'1'                                                         
         BNE   YEARERR             CAN'T EXCEED ONE YEAR                        
         CLC   WORK+2(2),WORK+8    CHK MTHS                                     
         BL    YEARERR             INVALID                                      
         BH    VR300               OK                                           
         CLC   WORK+4(2),WORK+10   SAME MTH - CHK DAY                           
         BH    VR300               STR DAY MUST BE HIGH                         
         B     YEARERR             ELSE INVALID                                 
*                                                                               
VR300    B     DREC                DISPLAY THE RECORD                           
         EJECT                                                                  
*              DISPLAY KEY                                                      
         SPACE 3                                                                
DKEY     L     R4,AIO                                                           
         MVC   GENCKEY,KEY                                                      
         USING NQURECD,R4                                                       
         GOTO1 CLUNPK,DMCB,NQUKCLT,PUPCLI                                       
         LA    R2,PUPCLIH                                                       
         MVI   5(R2),2                                                          
         CLI   PUPCLI+2,X'41'                                                   
         BL    *+8                                                              
         MVI   5(R2),3                                                          
         MVC   AIO,AIO2                                                         
         GOTO1 VVALCLT                                                          
         MVC   AIO,AIO1                                                         
         OI    PUPCLIH+6,X'80'                                                  
         MVC   PUPCLIN,CLTNAME                                                  
         OI    PUPCLINH+6,X'80'                                                 
*                                                                               
         MVC   PUPYEAR,NQUYEAR                                                  
         OI    PUPYEARH+6,X'80'                                                 
*                                                                               
         MVC   KEY,GENCKEY                                                      
         B     XIT                                                              
         EJECT                                                                  
*              DISPLAY RECORD                                                   
         SPACE 3                                                                
DREC     L     R4,AIO                                                           
         USING NQURECD,R4                                                       
*                                                                               
         MVC   PREVKEY,KEY                                                      
         MVI   PREVFLAG,C'Y'                                                    
*                                                                               
         XC    PUPQ4SD,PUPQ4SD                                                  
         XC    PUPQ4DT,PUPQ4DT                                                  
         XC    PUPQ1DT,PUPQ1DT                                                  
         XC    PUPQ2DT,PUPQ2DT                                                  
         XC    PUPQ3DT,PUPQ3DT                                                  
*                                                                               
         OI    PUPQ4SDH+6,X'80'                                                 
         OI    PUPQ4DTH+6,X'80'                                                 
         OI    PUPQ1DTH+6,X'80'                                                 
         OI    PUPQ2DTH+6,X'80'                                                 
         OI    PUPQ3DTH+6,X'80'                                                 
*                                                                               
         PRINT GEN                                                              
         GOTO1 DATCON,DMCB,(2,NQUA4ST),(5,PUPQ4SD)                              
         GOTO1 DATCON,DMCB,(2,NQUA4END),(5,PUPQ4DT)                             
         GOTO1 DATCON,DMCB,(2,NQUA1END),(5,PUPQ1DT)                             
         GOTO1 DATCON,DMCB,(2,NQUA2END),(5,PUPQ2DT)                             
         GOTO1 DATCON,DMCB,(2,NQUA3END),(5,PUPQ3DT)                             
         PRINT NOGEN                                                            
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* LIST RECORDS *                                                                
***********************************************************************         
LR       DS    0H                                                               
         LA    R5,LISTAR                                                        
         USING PLINED,R5                                                        
*                                                                               
LR02     CLI   PREVFLAG,C'Y'                                                    
         BE    LR03                                                             
         OC    KEY,KEY                                                          
         BNZ   LR05                                                             
         MVC   KEY(2),SVKEY                                                     
         OC    PUPCLI,PUPCLI                                                    
         BZ    LR05                                                             
         CLI   PUPCLIH+5,2                                                      
         BL    LR02A                                                            
         OC    PUPCLI,SPACES                                                    
         GOTO1 CLPACK,DMCB,PUPCLI,KEY+2                                         
         B     LR05                                                             
LR02A    XC    FULL,FULL                                                        
         MVC   FULL(3),=3C'A'                                                   
         ZIC   R1,PUPCLIH+5                                                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FULL(0),PUPCLI                                                   
         GOTO1 CLPACK,DMCB,FULL,KEY+2                                           
*                                                                               
         CLI   PUPYEARH+5,0                                                     
         BE    LR05                                                             
         MVC   NQUYEAR,PUPYEAR                                                  
         B     LR05                                                             
*                                                                               
LR03     MVC   KEY,PREVKEY                                                      
         MVI   PREVFLAG,C'N'                                                    
*                                                                               
LR05     GOTO1 HIGH                                                             
         B     LR10                                                             
*                                                                               
LRSEQ    GOTO1 SEQ                                                              
*                                                                               
LR10     CLC   KEY(4),KEYSAVE                                                   
         BNE   LRX                                                              
*                                                                               
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         USING NQURECD,R4                                                       
         GOTO1 CLUNPK,DMCB,NQUKCLT,PCLT     * CLIENT                            
         GOTO1 DATCON,DMCB,(2,NQUA4ST),(5,PQ4S)                                 
         GOTO1 DATCON,DMCB,(2,NQUA4END),(5,PQ4E)                                
         GOTO1 DATCON,DMCB,(2,NQUA1END),(5,PQ1E)                                
         GOTO1 DATCON,DMCB,(2,NQUA2END),(5,PQ2E)                                
         GOTO1 DATCON,DMCB,(2,NQUA3END),(5,PQ3E)                                
*                                                                               
         GOTO1 LISTMON                                                          
         B     LRSEQ               GOTO READ SEQ                                
*                                                                               
LRX      B     XIT                                                              
         DROP  R4,R5                                                            
         EJECT                                                                  
*                                                                               
*      IN HOUSE WRITE ROUTINES                                                  
MYFILADD NTR1                                                                   
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'SPTFILE ',KEY+14,AIO,MYDMWRK           
         BAS   RE,DMCHECK                                                       
         B     YES                                                              
*                                                                               
MYFILWRT NTR1                                                                   
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'SPTFILE ',KEY+14,AIO,MYDMWRK           
         BAS   RE,DMCHECK                                                       
         B     YES                                                              
*                                                                               
DMCHECK  CLI   8(R1),0                                                          
         BER   RE                                                               
         TM    8(R1),X'90'                                                      
         BM    NO                                                               
         DC    H'0'                                                             
         SPACE 1                                                                
YES      SR    R1,R1                                                            
         B     *+8                                                              
NO       LA    R1,1                                                             
         LTR   R1,R1                                                            
         SPACE 1                                                                
B        XIT                                                                    
         EJECT                                                                  
*              ERROR EXITS                                                      
         SPACE 3                                                                
INVINPT  MVC   CONHEAD(L'INVERR),INVERR                                         
         B     MYEND                                                            
         SPACE 1                                                                
INVYEAR  MVC   CONHEAD(L'YEARINV),YEARINV                                       
         B     MYEND                                                            
         SPACE 1                                                                
RANGERR  MVC   CONHEAD(L'HIGHERR),HIGHERR                                       
         B     MYEND                                                            
         SPACE 1                                                                
YEARERR  MVC   CONHEAD(L'YEARBK),YEARBK                                         
         B     MYCURSOR                                                         
         SPACE 1                                                                
BADERR   MVC   CONHEAD(L'BADCK),BADCK                                           
         B     MYCURSOR                                                         
         SPACE 1                                                                
SUNERR   MVC   CONHEAD(L'SUNDERR),SUNDERR                                       
         B     MYCURSOR                                                         
         SPACE 1                                                                
MONERR   MVC   CONHEAD(L'MONDERR),MONDERR                                       
         B     MYCURSOR                                                         
         SPACE 1                                                                
INVERR   DC    C'** ERROR ** INVALID INPUT'                                     
YEARINV  DC    C'** ERROR ** YEAR MUST BE 2 CHARACTER NUMERIC'                  
HIGHERR  DC    C'** ERROR ** QUARTER DATE GREATER THEN NEXT QUARTER'            
YEARBK   DC    C'** ERROR ** TOTAL QUARTERS GREATER THEN 1 YEAR'                
BADCK    DC    C'** ERROR ** DO NOT RETRY CONTACT DDS'                          
SUNDERR  DC    C'** ERROR ** MUST BE SUNDAY DATE'                               
MONDERR  DC    C'** ERROR ** MUST BE MONDAY DATE'                               
         EJECT                                                                  
*              SUPPORTING SUBROUTINES                                           
         SPACE 3                                                                
BUMP     ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         BR    RE                                                               
         SPACE 3                                                                
         GETEL (R6),DATADISP,ELCODE                                             
         SPACE 1                                                                
MYCURSOR MVI   ERROR,X'FE'         USING MY OWN ERROR MSG                       
         GOTO1 VCURSERR            AND POSITIONING CURSOR                       
         SPACE 1                                                                
MYEND    MVI   ERROR,X'FE'         USING MY OWN ERROR MSG                       
         SPACE 1                                                                
ERREND   GOTO1 VERRXIT                                                          
         SPACE 1                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*              LTORG FOR THIS PHASE                                             
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE NEPUPALLN                                                      
         EJECT                                                                  
       ++INCLUDE NEPUPE4D                                                       
         EJECT                                                                  
       ++INCLUDE NEPUPE5D                                                       
         EJECT                                                                  
GENCKEY  DS    CL32                                                             
SVKEY    DS    CL13                                                             
PREVKEY  DS    CL48                                                             
PREVFLAG DS    CL1                                                              
LASTPER  DS    CL1                 SAVE PERIOD                                  
*                                                                               
MYDMWRK  DS    12D                                                              
*                                                                               
CLTREC   DSECT                                                                  
       ++INCLUDE NEGENPQUA                                                      
*                                                                               
PLINED   DSECT                                                                  
PCLT     DS    CL3                                                              
         DS    CL4                                                              
PQ4S     DS    CL8                                                              
         DS    CL4                                                              
PQ4E     DS    CL8                                                              
         DS    CL4                                                              
PQ1E     DS    CL8                                                              
         DS    CL4                                                              
PQ2E     DS    CL8                                                              
         DS    CL4                                                              
PQ3E     DS    CL8                                                              
PLENGTH  EQU   *-PCLT                                                           
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'049NEPUP14   05/01/02'                                      
         END                                                                    
