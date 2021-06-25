*          DATA SET CTCONAN2   AT LEVEL 075 AS OF 02/08/00                      
*PHASE CONANN,*                                                                 
***********************************************************************         
*                                                                     *         
* MODULE CALLED AS AN EXTERN TO CONCRETE                                        
* SYSTEM SECURITY RECORDS                                             *         
* FIND SYSTEMS RECORDS THAT HAVE FILE=N AND SFM/=N                    *         
*                                                                     *         
***********************************************************************         
         TITLE 'CTCONANN - FIND SYS AUTH (X''21'') ELEMS'                       
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
         OC    3(20,R2),3(R2)      SKIP DUP RECORDS                             
         BNZ   EXIT                                                             
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
         MVC   SVSFM,SASYSALL      DEFAULT SETTING                              
         MVC   SVFILE,SASYSALL     DEFAULT SETTING                              
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
         B     DMX23                                                            
*                                                                               
DMX28    MVC   SVFILE,1(R4)        YES FILE SETTING                             
         OI    OVERRIDE,OVERFILE                                                
         B     DMX23                                                            
*                                                                               
*                                                                               
DMX30    CLC   SVSFM,SVFILE        SETTINGS ARE THE SAME                        
         BE    EXIT                THEN DON'T CARE                              
*                                                                               
         OC    SVSFM,SVSFM                                                      
         BZ    EXIT                ALSO DON'T CARE IF SFM=NO                    
*                                                                               
         OC    SVFILE,SVFILE                                                    
         BNZ   EXIT                ONLY WANT FILE=NO                            
*                                                                               
         AP    RECCHG,=P'1'                                                     
*                                                                               
         CLC   SA0KAGY,SVAGY                                                    
         BE    DMX38                                                            
*        BE    DMX38A                                                           
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         MVC   SVAGY,SA0KAGY                                                    
*                                                                               
*MX38A   GOTO1 VHEXOUT,DMCB,(R2),P+70,30,=C'TOG'                                
*        MVC   P+40(26),0(R2)                                                   
*                                                                               
DMX38    LA    R3,SA0DATA          FIRST ELEM                                   
DMX39    CLI   0(R3),0                                                          
         BE    EXIT                                                             
         CLI   0(R3),X'C3'         PID NAME ELEM                                
         BE    DMX40                                                            
         ZIC   R1,1(R3)                                                         
         AR    R3,R1                                                            
         B     DMX39                                                            
*                                                                               
DMX40    MVC   P+5(8),2(R3)                                                     
*                                                                               
         MVC   P+1(2),SA0KAGY                                                   
*                                                                               
         MVC   P+15(5),=C'FILE='                                                
         MVI   P+20,C'Y'                                                        
         CLC   SVFILE,=X'000F'                                                  
         BE    DMX42                                                            
         MVI   P+20,C'N'                                                        
         CLC   SVFILE,=X'0000'                                                  
         BE    DMX42                                                            
         GOTO1 VHEXOUT,DMCB,SVFILE,P+20,2,=C'TOG'                               
*                                                                               
DMX42    MVC   P+27(4),=C'SFM='                                                 
         MVI   P+31,C'Y'                                                        
         CLC   SVSFM,=X'000F'                                                   
         BE    DMX44                                                            
         MVI   P+31,C'N'                                                        
         CLC   SVSFM,=X'0000'                                                   
         BE    DMX44                                                            
         GOTO1 VHEXOUT,DMCB,SVSFM,P+31,2,=C'TOG'                                
DMX44    GOTO1 VPRINTER                                                         
         B     EXIT                                                             
         DROP  R3,R2                                                            
*                                                                               
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
SVAGY    DS    CL2                                                              
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
**PAN#1  DC    CL21'075CTCONAN2  02/08/00'                                      
         END                                                                    
