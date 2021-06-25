*          DATA SET CTCONANN   AT LEVEL 063 AS OF 11/19/99                      
*PHASE CONANN,*                                                                 
***********************************************************************         
*                                                                     *         
* MODULE CALLED AS AN EXTERN TO CONCRETE TO CHANGE X'21' ELEMS FOR    *         
* SYSTEM SECURITY RECORDS                                             *         
* COPY SPOT FILE SETTINGS TO SPOT SFM - ONLY IF SFM IS NOT =N         *         
* FOR ALL LISTED ID'S, AND SAVE OFF OLD EL FOR LATER RESTORE          *         
*                                                                     *         
***********************************************************************         
         TITLE 'CTCONANN - CHANGE SYS AUTH (X''21'') ELEMS'                     
CONANN   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*CONANN                                                        
*                                                                               
         LR    RC,R1               GET W/S POINTER                              
         USING CONWORKD,RC                                                      
*                                                                               
         MVI   DATADISP+1,28                                                    
*                                                                               
MAIN     DS    0H                                                               
*                                                                               
         L     R2,AIOAREA          POINT TO RECORD                              
         LA    R2,4(R2)            POINT TO FIRST BYTE OF RECORD                
*                                                                               
         L     R8,VCPRINT                                                       
         USING DPRINT,R8                                                        
*                                                                               
         CLI   0(R2),X'FF'         TEST FILE TRAILER                            
         BNE   M10                 YES - PRINT TOTALS                           
         MVI   P,C' '                                                           
         MVC   P+1(L'P-1),P                                                     
         MVC   P+10(12),=C'CHANGED RECS'                                        
         EDIT  (P4,RECCHG),(8,P),ZERO=NOBLANK                                   
         GOTO1 VPRINTER                                                         
         XIT1                                                                   
*                                                                               
M10      DS    0H                                                               
*                                                                               
         USING SA0REC,R2                                                        
         TM    SA0STAT,X'80'       TEST ALREADY DELETED                         
         BO    EXIT                YES                                          
*                                                                               
         CLI   SA0KTYP,SA0KTYPQ    C'0' TYPE                                    
         BNE   EXIT                                                             
*                                                                               
         CLC   SA0LEN,=H'1900'                                                  
         BH    PRNERR                                                           
*                                                                               
         LA    R3,SA0DATA          FIRST ELEM                                   
DMX10    CLI   0(R3),0                                                          
         BE    EXIT                                                             
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
         AHI   R5,-16              SUBTRACT FIXED PART                          
         LA    R4,SASYSPGM         PROGRAM LIST                                 
DMX22    CH    R5,=H'0'            R5 = REMAINING LEN OF ELEM                   
         BNH   DMX30                                                            
         CLI   0(R4),X'17'         FIND SFM SETTING AND SAVE                    
         BE    DMX25                                                            
         CLI   0(R4),X'19'         FIND FILE SETTING AND SAVE                   
         BE    DMX28                                                            
DMX23    LA    R4,3(R4)                                                         
         AHI   R5,-3                                                            
         B     DMX22                                                            
*                                                                               
DMX25    MVC   SVSFM,1(R4)         YES SFM SETTING                              
         OI    OVERRIDE,OVERSFM                                                 
         OC    SVSFM,SVSFM         IS SFM=NO                                    
         BZ    EXIT                THEN DON'T CHANGE - DONE                     
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
         TM    OVERRIDE,OVERSFM    SFM OVERRIDE?                                
         BZ    DMX40               NO                                           
*                                                                               
*                                                                               
* BOTH FILE AND SFM HAVE OVERRIDES - REPLACE SFM WITH FILE SETTING              
*                                                                               
         CLC   SVFILE,SVSFM        IF THEY ARE ALREADY THE SAME                 
         BE    EXIT                THEN DO NOTHING                              
*                                                                               
         CP    RECBOTH,=P'10'                                                   
         BH    DMX31                                                            
         MVC   P(14),=C'BOTH OVERRIDES'                                         
         GOTO1 VHEXOUT,DMCB,SVFILE,P+20,2,=C'TOG'                               
         GOTO1 VHEXOUT,DMCB,SVSFM,P+25,2,=C'TOG'                                
         GOTO1 VPRINTER                                                         
*                                                                               
         BAS   RE,PRTKEY                                                        
         BAS   RE,PRTELEM                                                       
*                                                                               
DMX31    LA    R4,SASYSPGM         PROGRAM LIST                                 
DMX32    CLI   0(R4),X'17'         FIND SFM SETTING AND REPLACE                 
         BE    DMX34                                                            
         LA    R4,3(R4)                                                         
         B     DMX32                                                            
DMX34    MVC   1(2,R4),SVFILE      REPLACE WITH FILE OVERRIDE                   
*                                                                               
         AP    RECCHG,=P'1'                                                     
         AP    RECBOTH,=P'1'                                                    
*                                                                               
         CP    RECBOTH,=P'11'                                                   
         BH    EXIT                                                             
         BAS   RE,PRTELEM                                                       
         GOTO1 VPRINTER                                                         
         B     EXIT                                                             
*                                                                               
*                                                                               
* FILE OVERRIDE BUT NO SFM OVERRIDE                                             
*                                                                               
DMX40    OC    SASYSALL,SASYSALL   IS DEFAULT SETTING = NO                      
         BZ    EXIT                THEN DON'T CHANGE - DONE                     
*                                                                               
         CP    RECFIL,=P'10'                                                    
         BH    DMX41                                                            
         MVC   P(14),=C'FILE OVERRIDE '                                         
         GOTO1 VHEXOUT,DMCB,SVFILE,P+20,2,=C'TOG'                               
         GOTO1 VHEXOUT,DMCB,SVSFM,P+25,2,=C'TOG'                                
         GOTO1 VPRINTER                                                         
         BAS   RE,PRTKEY                                                        
         BAS   RE,PRTELEM                                                       
*                                                                               
DMX41    ZIC   R1,1(R3)            R3 POINT TO BEGINNING OF ELEM                
         AHI   R1,-1                                                            
         EX    R1,*+4                                                           
         MVC   ELEM(0),0(R3)       SAVE ELEM                                    
         MVI   0(R3),X'FF'         SET TO DELETE ELEM                           
*                                                                               
         GOTO1 VHELLO,DMCB,(C'D',=C'CTFBIG'),(X'FF',(R2)),0,0                   
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CP    RECFIL,=P'10'                                                    
         BH    DMX42                                                            
         MVC   P(12),=C'ELEM DELETED'                                           
         GOTO1 VPRINTER                                                         
*                                                                               
DMX42    BAS   RE,BLDSFM           BLD SFM SETTING IN ELEM AND ADD              
*                                                                               
         GOTO1 VHELLO,DMCB,(C'P',=C'CTFBIG'),(R2),ELEM,0                        
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CP    RECFIL,=P'10'                                                    
         BH    DMX44                                                            
         MVC   P(10),=C'ELEM ADDED'                                             
         GOTO1 VPRINTER                                                         
*                                                                               
DMX44    SR    R1,R1                                                            
         ICM   R1,3,SA0LEN                                                      
         AHI   R1,3                                                             
         L     R3,AIOAREA                                                       
         STH   R1,0(R3)                                                         
*                                                                               
* MAKE SURE REC LT 2000 BYTES                                                   
         CHI   R1,2000                                                          
         BL    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         AP    RECCHG,=P'1'                                                     
         AP    RECFIL,=P'1'                                                     
*                                                                               
         CP    RECFIL,=P'11'                                                    
         BH    EXIT                                                             
         GOTO1 VPRINTER                                                         
         B     EXIT                                                             
*                                                                               
*                                                                               
* NO FILE OVERRIDE                                                              
*                                                                               
DMX50    TM    OVERRIDE,OVERSFM    SFM OVERRIDE                                 
         BZ    EXIT                NO SFM OVERRIDE = DONE                       
*                                                                               
*                                                                               
* NO FILE OVERRIDE BUT SFM OVERRIDE                                             
*                                                                               
DMX60    OC    SVSFM,SVSFM         IS SFM OVERRIDE = NO                         
         BZ    EXIT                THEN DONE                                    
*                                                                               
         CP    RECSFM,=P'10'                                                    
         BH    DMX61                                                            
         MVC   P(14),=C'SFM  OVERRIDE '                                         
         GOTO1 VHEXOUT,DMCB,SVFILE,P+20,2,=C'TOG'                               
         GOTO1 VHEXOUT,DMCB,SVSFM,P+25,2,=C'TOG'                                
         GOTO1 VPRINTER                                                         
         BAS   RE,PRTKEY                                                        
         BAS   RE,PRTELEM                                                       
*                                                                               
DMX61    ZIC   R1,1(R3)            R3 POINT TO BEGINNING OF ELEM                
         AHI   R1,-1                                                            
         EX    R1,*+4                                                           
         MVC   ELEM(0),0(R3)       SAVE ELEM                                    
         MVI   0(R3),X'FF'         SET TO DELETE ELEM                           
         GOTO1 VHELLO,DMCB,(C'D',=C'CTFBIG'),(X'FF',(R2)),0,0                   
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BAS   RE,DELSFM           DELETE SFM SETTING IN ELEM AND ADD           
*                                                                               
         GOTO1 VHELLO,DMCB,(C'P',=C'CTFBIG'),(R2),ELEM,0                        
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,SA0LEN                                                      
         AHI   R1,-3                                                            
         L     R3,AIOAREA                                                       
         STH   R1,0(R3)                                                         
*                                                                               
* MAKE SURE REC LT 2000 BYTES                                                   
         CHI   R1,2000                                                          
         BL    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         AP    RECCHG,=P'1'                                                     
         AP    RECSFM,=P'1'                                                     
*                                                                               
         CP    RECSFM,=P'11'                                                    
         BH    EXIT                                                             
         GOTO1 VPRINTER                                                         
         B     EXIT                                                             
         DROP  R3                                                               
*                                                                               
BLDSFM   NTR1                      BLD SFM INTO ELEM                            
         USING SASYSD,R6                                                        
         LA    R6,ELEM                                                          
         ZIC   R5,SASYSLN                                                       
         AHI   R5,-16              SUBTRACT FIXED PART                          
         LA    R4,SASYSPGM         PROGRAM LIST                                 
BSFM10   CH    R5,=H'0'            R5 = REMAINING LEN OF ELEM                   
         BNH   BSFM20              ADD TO END                                   
         CLI   0(R4),X'17'                                                      
         BH    BSFM20              ADD HERE                                     
         LA    R4,3(R4)                                                         
         AHI   R5,-3                                                            
         B     BSFM10                                                           
*                                                                               
BSFM20   MVC   WORK2(100),0(R4)    SAVE END OF ELEM                             
         MVI   0(R4),X'17'         SFM                                          
         MVC   1(2,R4),SVFILE      WITH FILE SETTINGS                           
         MVC   3(100,R4),WORK2     REPLACE END OF ELEM                          
         ZIC   R1,SASYSLN                                                       
         AHI   R1,3                INCREMENT LEN                                
         STC   R1,SASYSLN                                                       
         CP    RECFIL,=P'10'                                                    
         BH    BSFMX                                                            
         MVC   P(7),=C'ADD SFM'                                                 
         GOTO1 VPRINTER                                                         
         LA    R3,ELEM                                                          
         BAS   RE,PRTELEM                                                       
BSFMX    XIT1                                                                   
*                                                                               
DELSFM   NTR1                      BLD SFM INTO ELEM                            
         USING SASYSD,R6                                                        
         LA    R6,ELEM                                                          
         ZIC   R5,SASYSLN                                                       
         AHI   R5,-16              SUBTRACT FIXED PART                          
         LA    R4,SASYSPGM         PROGRAM LIST                                 
DSFM10   LTR   R5,R5               R5 = REMAINING LEN OF ELEM                   
         BNZ   *+6                                                              
         DC    H'0'                HAS TO BE THERE                              
         CLI   0(R4),X'17'                                                      
         BE    DSFM20              ADD HERE                                     
         LA    R4,3(R4)                                                         
         AHI   R5,-3                                                            
         B     DSFM10                                                           
*                                                                               
DSFM20   MVC   WORK2(100),3(R4)    MOVE END OF ELEM                             
         MVC   0(100,R4),WORK2     REPLACE END OF ELEM                          
         ZIC   R1,SASYSLN                                                       
         AHI   R1,-3               DECREMENT LEN                                
         STC   R1,SASYSLN                                                       
         CP    RECSFM,=P'10'                                                    
         BH    DSFMX                                                            
         MVC   P(7),=C'DEL SFM'                                                 
         GOTO1 VPRINTER                                                         
         LA    R3,ELEM                                                          
         BAS   RE,PRTELEM                                                       
DSFMX    XIT1                                                                   
         DROP  R6                                                               
*                                                                               
PRTELEM  NTR1                                                                   
*        CP    RECCHG,=P'50'                                                    
*        BH    PRTELX                                                           
         ZIC   R4,1(R3)            LEN                                          
         GOTO1 VHEXOUT,DMCB,0(R3),P,(R4),=C'TOG'                                
         GOTO1 VPRINTER                                                         
PRTELX   XIT1                                                                   
*                                                                               
PRTKEY   NTR1                                                                   
*        CP    RECCHG,=P'50'                                                    
*        BH    PRTKEYX                                                          
         GOTO1 VHEXOUT,DMCB,0(R2),P,30,=C'TOG'                                  
         GOTO1 VPRINTER                                                         
         MVC   P(30),0(R2)                                                      
         GOTO1 VPRINTER                                                         
PRTKEYX  XIT1                                                                   
*                                                                               
         EJECT                                                                  
*                                                                               
*                                                                               
PRNERR   DS    0H                                                               
         MVC   P(14),=C'LENGTH ERROR: '                                         
         GOTO1 VHEXOUT,DMCB,0(R2),P+14,25,=C'TOG'                               
         GOTO1 VPRINTER                                                         
         B     EXIT                                                             
         DROP  R8                                                               
*                                                                               
ENDSYS   AHI   R7,1                R7=1ST PROGRAM                               
NS2      CLI   0(R7),0             END OF SYSTEM?                               
         BER   RE                                                               
         AHI   R7,3                                                             
         B     NS2                                                              
                                                                                
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
DATADISP DS    H                                                                
ELCODE   DS    C                                                                
RECCHG   DC    PL4'0'              CHANGED RECS                                 
RECFIL   DC    PL4'0'              CHANGED RECS                                 
RECSFM   DC    PL4'0'              CHANGED RECS                                 
RECBOTH  DC    PL4'0'              CHANGED RECS                                 
*                                                                               
OVERRIDE DS    XL1                                                              
OVERSFM  EQU   X'80'                                                            
OVERFILE EQU   X'40'                                                            
SVFILE   DS    XL2                                                              
SVSFM    DS    XL2                                                              
ELEM     DS    XL250                                                            
WORK2    DS    CL128                                                            
WORKX    DS    0C                                                               
*                                                                               
SYSPRG   DS    0H                                                               
         DC    X'06'               ACC SYSTEM                                   
         DC    X'030264'            FILE=0264                                   
         DC    X'0B0464'            PRD=0464                                    
         DC    X'190000'            INT=N                                       
         DC    X'00'               END SYSTEM ACC                               
*                                                                               
         DC    X'02'               SPOT SYSTEM                                  
         DC    X'130000'            PAY                                         
         DC    X'00'               END SYSTEM SPOT                              
*                                                                               
         DC    X'04'               PRINT SYSTEM                                 
         DC    X'030000'            PAY                                         
         DC    X'00'               END SYSTEM PRINT                             
*                                                                               
         DC    X'03'               NET SYSTEM                                   
         DC    X'130000'            PAY                                         
         DC    X'00'               END SYSTEM NET                               
*                                                                               
         DC    X'FF'               EOFT                                         
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE CTCONWORKD                                                     
       ++INCLUDE DDDPRINT                                                       
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE SEACSFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'063CTCONANN  11/19/99'                                      
         END                                                                    
