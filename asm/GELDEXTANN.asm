*          DATA SET GELDEXTANN AT LEVEL 013 AS OF 11/11/99                      
*PHASE GELDXANN,*                                                               
*INCLUDE HEXOUT                                                                 
*INCLUDE HELEN                                                                  
*INCLUDE HELLO                                                                  
         TITLE 'GELDEXTANN - GENDIR FILE FIX FOR SYSTEM RECORDS'                
* PARAMETER LIST                                                                
*                                                                               
* P1=A(RECORD)  PASS FIRST BYTE X'00'= INITIALISE                               
*                               X'01'= RECORD IN CORE                           
*                               X'FF'= END OF FILE                              
*               RETURN VALUE    X'00'= KEEP RECORD                              
*                               X'FF'= PURGE RECORD                             
*                               X'FF'/C'EOJ'=PURGE & CAUSE EOJ                  
* P2=A(TAPEOUT) PASS FIRST BYTE X'80'= TAPE INPUT                               
*                               X'40'= TAPE OUTPUT                              
*                               X'20'= RECORD IS I/S FILE RECORD                
* P3=A(PARAM CARD)                                                              
* P4=A(FILE DEFN)                                                               
* P5=A(PRINTER)                                                                 
* P6=A(CPRINT)                                                                  
*                                                                               
         PRINT NOGEN                                                            
DMLDEXT  CSECT                                                                  
         NMOD1 (WORKX-WORKD),GELDEXT                                            
         USING WORKD,RC                                                         
         EJECT                                                                  
* CONTROL FLOW LOGIC                                                            
*                                                                               
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         SPACE 2                                                                
         CLI   PLIST,X'00'                                                      
         BE    DMXINIT             INITIALISE                                   
         CLI   PLIST,X'01'                                                      
         BE    DMXREC              PROCESS                                      
         CLI   PLIST,X'FF'                                                      
         BE    DMXEOF              END-OF-FILE                                  
         B     DMXIT                                                            
         SPACE 2                                                                
DMXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         B     DMXIT                                                            
         SPACE 2                                                                
DMXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         B     DMXIT                                                            
         SPACE 2                                                                
DMXPGEOF L     R1,APARM            PURGE AND CAUSE INPUT EOF EXIT               
         MVI   0(R1),X'FF'                                                      
         MVC   1(3,R1),=C'EOF'                                                  
         B     DMXIT                                                            
         SPACE 2                                                                
DMXIT    XMOD1 1                                                                
         EJECT                                                                  
* INITIALISE LOGIC                                                              
*                                                                               
DMXINIT  DS    0H                                                               
*                                                                               
         B     DMXIT                                                            
*                                                                               
* PROCESS RECORD LOGIC                                                          
*                                                                               
DMXREC   DS    0H                                                               
         L     R2,AREC                                                          
         USING SA0REC,R2                                                        
         TM    SA0STAT,X'80'       TEST ALREADY DELETED                         
         BO    DMXPURGE            YES                                          
*                                                                               
         CLI   SA0KTYP,SA0KTYPQ    C'0' TYPE                                    
         BNE   DMXKEEP                                                          
*                                                                               
         LA    R3,SA0DATA          FIRST ELEM                                   
DMX10    CLI   0(R3),0                                                          
         BE    DMXKEEP                                                          
         CLI   0(R3),X'21'         SYSTEM ELEMENT                               
         BE    DMX20                                                            
DMX10NXT ZIC   R1,1(R3)                                                         
         AR    R3,R1                                                            
         B     DMX10                                                            
*                                                                               
         USING SASYSD,R3                                                        
DMX20    CLI   SASYSNUM,X'02'      SPOT SYSTEM                                  
         BNE   DMX10NXT                                                         
*                                                                               
         MVI   OVERRIDE,0                                                       
         XC    SVFILE,SVFILE                                                    
         XC    SVSFM,SVSFM                                                      
         ZIC   R5,SASYSLN                                                       
         AHI   R5,-SASYSLNQ        SUBTRACT FIXED PART                          
         LA    R4,SASYSPGM         PROGRAM LIST                                 
DMX22    LTR   R5,R5               R5 = REMAINING LEN OF ELEM                   
         BZ    DMX30                                                            
         CLI   0(R4),X'17'         FIND SFM SETTING AND SAVE                    
         BE    DMX25                                                            
         CLI   0(R4),X'19'         FIND FILE SETTING AND SAVE                   
         BE    DMX28                                                            
DMX23    LA    R4,3(R4)                                                         
         SHI   R5,-3                                                            
         B     DMX22                                                            
*                                                                               
DMX25    MVC   SVSFM,1(R4)         YES SFM SETTING                              
         OI    OVERRIDE,OVERSFM                                                 
         OC    SVSFM,SVSFM         IS SFM=NO                                    
         BZ    DMXKEEP             THEN DON'T CHANGE - DONE                     
         B     DMX23                                                            
*                                                                               
DMX28    MVC   SVFILE,1(R4)        YES FILE SETTING                             
         OI    OVERRIDE,OVERFILE                                                
         B     DMX23                                                            
*                                                                               
*                                                                               
DMX30    TM    OVERRIDE,OVERFILE   FILE OVERRIDE?                               
         BZ    DMX50               NO                                           
*                                                                               
         OC    OVERRIDE,OVERSFM    SFM OVERRIDE?                                
         BZ    DMX40               NO                                           
*                                                                               
*                                                                               
* BOTH FILE AND SFM HAVE OVERRIDES - REPLACE SFM WITH FILE SETTING              
*                                                                               
         CLC   SVFILE,SVSFM        IF THEY ARE ALREADY THE SAME                 
         BE    DMXKEEP             THEN DO NOTHING                              
*                                                                               
         BAS   RE,PRTELEM                                                       
         LA    R4,SASYSPGM         PROGRAM LIST                                 
DMX32    CLI   0(R4),X'17'         FIND SFM SETTING AND REPLACE                 
         BE    DMX34                                                            
         LA    R4,3(R4)                                                         
         B     DMX32                                                            
DMX34    MVC   1(2,R4),SVFILE      REPLACE WITH FILE OVERRIDE                   
         AP    RECCHG,=P'1'                                                     
         BAS   RE,PRTELEM                                                       
         CP    RECCHG,=P'50'                                                    
         BH    DMXKEEP                                                          
         GOTO1 VPRINTER                                                         
         B     DMXKEEP                                                          
*                                                                               
*                                                                               
* FILE OVERRIDE BUT NO SFM OVERRIDE                                             
*                                                                               
DMX40    OC    SASYSALL,SASYSALL   IS DEFAULT SETTING = NO                      
         BZ    DMXKEEP             THEN DON'T CHANGE - DONE                     
*                                                                               
         BAS   RE,PRTELEM                                                       
*                                                                               
         ZIC   R1,1(R3)            R3 POINT TO BEGINNING OF ELEM                
         AHI   R1,-1                                                            
         EX    R1,*+4                                                           
         MVC   ELEM(0),0(R3)       SAVE ELEM                                    
         MVI   0(R3),X'FF'         SET TO DELETE ELEM                           
         GOTO1 =V(HELLO),DMCB,(C'D',=C'GENFIL'),(X'FF',0(R2)),0,0               
*                                                                               
         BAS   RE,BLDSFM           BLD SFM SETTING IN ELEM AND ADD              
*                                                                               
         GOTO1 =V(HELLO),DMCB,(C'P',=C'GENFIL'),0(R2),ELEM,0                    
*                                                                               
         AP    RECCHG,=P'1'                                                     
         BAS   RE,PRTELEM                                                       
         CP    RECCHG,=P'50'                                                    
         BH    DMXKEEP                                                          
         GOTO1 VPRINTER                                                         
         B     DMXKEEP                                                          
*                                                                               
*                                                                               
* NO FILE OVERRIDE                                                              
*                                                                               
DMX50    TM    OVERRIDE,OVERSFM    SFM OVERRIDE                                 
         BZ    DMXKEEP             NO SFM OVERRIDE = DONE                       
*                                                                               
*                                                                               
* NO FILE OVERRIDE BUT SFM OVERRIDE                                             
*                                                                               
DMX60    OC    SVSFM,SVSFM         IS SFM OVERRIDE = NO                         
         BZ    DMXKEEP             THEN DONE                                    
*                                                                               
         BAS   RE,PRTELEM                                                       
*                                                                               
         ZIC   R1,1(R3)            R3 POINT TO BEGINNING OF ELEM                
         AHI   R1,-1                                                            
         EX    R1,*+4                                                           
         MVC   ELEM(0),0(R3)       SAVE ELEM                                    
         MVI   0(R3),X'FF'         SET TO DELETE ELEM                           
         GOTO1 =V(HELLO),DMCB,(C'D',=C'GENFIL'),(X'FF',0(R2)),0,0               
*                                                                               
         BAS   RE,DELSFM           DELETE SFM SETTING IN ELEM AND ADD           
*                                                                               
         GOTO1 =V(HELLO),DMCB,(C'P',=C'GENFIL'),0(R2),ELEM,0                    
*                                                                               
         AP    RECCHG,=P'1'                                                     
         BAS   RE,PRTELEM                                                       
         CP    RECCHG,=P'50'                                                    
         BH    DMXKEEP                                                          
         GOTO1 VPRINTER                                                         
         B     DMXKEEP                                                          
         DROP  R3                                                               
*                                                                               
BLDSFM   NTR1                      BLD SFM INTO ELEM                            
         USING SASYSD,R6                                                        
         LA    R6,ELEM                                                          
         ZIC   R5,SASYSLN                                                       
         AHI   R5,-SASYSLNQ        SUBTRACT FIXED PART                          
         LA    R4,SASYSPGM         PROGRAM LIST                                 
BSFM10   LTR   R5,R5               R5 = REMAINING LEN OF ELEM                   
         BZ    BSFM20              ADD TO END                                   
         CLI   0(R4),X'17'                                                      
         BH    BSFM20              ADD HERE                                     
         LA    R4,3(R4)                                                         
         SHI   R5,-3                                                            
         B     BSFM10                                                           
*                                                                               
BSFM20   MVC   WORK(100),0(R4)     SAVE END OF ELEM                             
         MVI   0(R4),X'17'         SFM                                          
         MVC   1(2,R4),SVFILE      WITH FILE SETTINGS                           
         MVC   3(100,R4),WORK      REPLACE END OF ELEM                          
         ZIC   R1,SASYSLN                                                       
         AHI   R1,3                INCREMENT LEN                                
         STC   R1,SASYSLN                                                       
         XIT1                                                                   
*                                                                               
DELSFM   NTR1                      BLD SFM INTO ELEM                            
         USING SASYSD,R6                                                        
         LA    R6,ELEM                                                          
         ZIC   R5,SASYSLN                                                       
         AHI   R5,-SASYSLNQ        SUBTRACT FIXED PART                          
         LA    R4,SASYSPGM         PROGRAM LIST                                 
DSFM10   LTR   R5,R5               R5 = REMAINING LEN OF ELEM                   
         BNZ   *+6                                                              
         DC    H'0'                HAS TO BE THERE                              
         CLI   0(R4),X'17'                                                      
         BE    DSFM20              ADD HERE                                     
         LA    R4,3(R4)                                                         
         SHI   R5,-3                                                            
         B     DSFM10                                                           
*                                                                               
DSFM20   MVC   WORK(100),3(R4)     MOVE END OF ELEM                             
         MVC   0(100,R4),WORK      REPLACE END OF ELEM                          
         ZIC   R1,SASYSLN                                                       
         AHI   R1,-3               DECREMENT LEN                                
         STC   R1,SASYSLN                                                       
         XIT1                                                                   
         DROP  R6                                                               
*                                                                               
PRTELEM  NTR1                                                                   
         CP    RECCHG,=P'50'                                                    
         BH    PRTELX                                                           
         ZIC   R2,1(R3)            LEN                                          
         GOTO1 =V(HEXOUT),DMCB,0(R3),P,(R2),=C'TOG'                             
         GOTO1 VPRINTER                                                         
PRTELX   XIT1                                                                   
*                                                                               
         EJECT                                                                  
* END-OF-FILE LOGIC                                                             
*                                                                               
DMXEOF   DS    0H                                                               
         MVI   P,C' '                                                           
         MVC   P+1(L'P-1),P                                                     
         MVC   P+10(12),=C'CHANGED RECS'                                        
         EDIT  (P4,RECCHG),(8,P),ZERO=NOBLANK                                   
         GOTO1 VPRINTER                                                         
*                                                                               
         B     DMXIT                                                            
         EJECT                                                                  
         LTORG                                                                  
         SPACE 2                                                                
RECCHG   DC    PL4'0'                                                           
         EJECT                                                                  
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
APARM    DS    A                                                                
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
*                                                                               
OVERRIDE DS    XL1                                                              
OVERSFM  EQU   X'80'                                                            
OVERFILE EQU   X'40'                                                            
SVFILE   DS    XL2                                                              
SVSFM    DS    XL2                                                              
ELEM     DS    XL250                                                            
WORK     DS    CL128                                                            
WORKX    DS    0C                                                               
         EJECT                                                                  
*DMLDDEFN                                                                       
       ++INCLUDE DMLDDEFN                                                       
         EJECT                                                                  
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
         SPACE 2                                                                
*GEGENSCR                                                                       
       ++INCLUDE GEGENSCR                                                       
*SEACSFILE                                                                      
       ++INCLUDE SEACSFILE                                                      
         SPACE 2                                                                
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013GELDEXTANN11/11/99'                                      
         END                                                                    
