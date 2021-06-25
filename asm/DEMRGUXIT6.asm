*          DATA SET DEMRGUXIT6 AT LEVEL 002 AS OF 07/08/13                      
*PROCESS USING(WARN(15))                                                        
*PHASE DEMRGU6A                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE SORTER                                                                 
***********************************************************************         
*                                                                     *         
* NOTE FROM DEIS (JUL/2013): IT APPEARS THAT THIS CONVERSION HASN'T   *         
* BEEN RUN FOR A FEW YEARS. IF IT EVER NEEDS TO BE RESURRECTED,       *         
* PLEASE NOTE THE FOLLOWING:                                          *         
*  1. WE SHOULD CHANGE THE PRODUCTION JCL TO COMMENT OUT THE          *         
*     //SYMNOUT DD * STATEMENT. IT'S GENERATING TONS OF REDUNDANT     *         
*     OUTPUT.                                                         *         
*  2. THIS PROGRAM APPEARS TO TAKE A VERY LONG TIME TO RUN, PERHAPS   *         
*     BECAUSE IT IS CALLING DDSORTER FROM WITHIN A DFSORT EXIT. WE    *         
*     MIGHT CONSIDER CHANGING THIS EXIT SO THAT IT MODIFIES THE       *         
*     OUTPUT RECORDS IN SUCH A WAY THAT A SUBSEQUENT ICETOOL OPERATOR *         
*     CAN GIVE US THE SAME RESULTS AS WE'RE GETTING NOW. TBD.         *         
*                                                                     *         
***********************************************************************         
***********************************************************************         
*                                                                     *         
* THIS IS A DFSORT USER EXIT PROGRAM. IT IS INVOKED VIA MEMBER        *         
* DEMRGICED, WHICH CONTAINS CONTROL CARDS TO ICETOOL/DFSORT.          *         
* SEE IBM'S DFSORT: APPLICATION PROGRAMMING GUIDE FOR DETAILS.        *         
*                                                                     *         
* NOTE: CALLS TO EXTERNAL SUBROUTINES *MUST* BE MADE VIA              *         
*       CALL_DDS_SUBRTN, BECAUSE THIS EXIT'S USE OF REGISTERS IS      *         
*       NON-DDS-CONFORMING.                                           *         
*                                                                     *         
* THIS MODULE READS THE OUTPUT OF A PREVIOUS SORT, AND MERGES         *         
* TOGETHER RECORDS OF THE SAME MAJOR/MINOR KEY.                       *         
*                                                                     *         
***********************************************************************         
DEMRGU6  CSECT                                                                  
*                                                                               
         ENTRY E35                 MUST BE "E35" (FOR DFSORT)                   
*                                                                               
         PRINT NOGEN                                                            
         REQUS                                                                  
*                                                                               
         USING E35,RC              RC = PROGRAM BASE REGISTER                   
E35      STM   RE,RC,12(RD)        SAVE ALL REGS EXCEPT RD                      
         LA    RC,0(RF)            SET PROGRAM BASE REGISTER                    
         ST    RD,SAVE15+4         SAVE BACKWARD POINTER                        
         LA    RE,SAVE15           SET FORWARD POINTER...                       
         ST    RE,8(RD)            ...IN SAVE AREA                              
         LR    RD,RE               SET OUR SAVE AREA                            
         LR    R2,R1               SAVE PARAMETER LIST POINTER                  
*                                                                               
         LA    R5,OUT_REC          BUILD OUTPUT RECORD HERE                     
*                                                                               
         L     R3,0(R2)            LOAD A(RECORD)                               
         LTR   R3,R3               EOF?                                         
         BZ    EOF                 YES: DO NOT RETURN                           
*                                                                               
         LA    R7,SECT_TAB                                                      
*                                                                               
         MVC   REC_ORIGIN,4(R3)    FILE/TAPE OR SIGNAL                          
         CLI   NEW_REC,C'Y'                                                     
         BE    BUFFREC                                                          
*                                                                               
         CLC   =C'*END OF MAJOR/MINOR KEY*',25(R3)                              
         BE    KEEPREC             PUT OUT THE RECORD                           
*        BNE   MERGEREC            ADD-ON TO BUFFER                             
         BNE   BR01                                                             
************************************************************                    
*        BUFFER THE RECORDS HERE. (STARTING FROM X'23' ELEM                     
* R3: POINTS TO THE INCOMING RECORD                                             
* R4: POINTS TO THE BUFFER                                                      
* R6: POINTS TO BEGINNING OF THE SECTION                                        
* R7: POINTS TO THE SECTION ADDRESS/LENGTH POINTER TABLE                        
************************************************************                    
BUFFREC  DS    0H                                                               
*                                                                               
         SR    R5,R5               BACKUP THE RECORD                            
         ICM   R5,3,0(R3)                                                       
         SHI   R5,1                                                             
         STCM  R5,3,OUT_RDW                                                     
         LA    R4,OUT_REC                                                       
         LA    R3,5(R3)            POINT PAST RDW+REC ORIGIN                    
         SHI   R5,4                                                             
         LR    RF,R4                                                            
         LR    RE,R3                                                            
         MVCL  R4,RE                                                            
*                                                                               
         CLC   OUT_RDW(2),=H'27'                                                
         BE    BR30                                                             
*                                                                               
         LA    RE,ELEM_BUFFER                                                   
         LA    RF,3000                                                          
         XCEF                                                                   
         LA    R5,ELEM_BUFFER                                                   
         LA    R6,ELEM_BUFFER                                                   
         B     BR05                                                             
*                                                                               
BR01     DS    0H                                                               
         CLI   0(R7),X'FF'         ADD-ON TO BUFFER                             
         BE    *+12                                                             
         LA    R7,L'SECT_TAB(R7)                                                
         B     BR01                                                             
         ICM   R5,15,1(R7)                                                      
         LR    R6,R5                                                            
         LA    R3,5(R3)                                                         
*                                                                               
BR05     MVI   ELCODE,X'23'        STORE THE MARKET BREAK                       
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SEC_ORIGIN,2(R3)                                                 
         B     BR20                                                             
*                                                                               
BR10     CLI   0(R3),X'23'         NEW SECTION? OR END OF RECORD?               
         BH    BR20                                                             
         LR    RE,R5                                                            
         SR    RE,R6                                                            
         ST    R6,0(R7)            PREVIOUS SECTION ADDRESS                     
         STCM  RE,3,4(R7)          PREVIOUS SECTION LENGTH                      
         MVC   6(1,R7),SEC_ORIGIN                                               
         MVC   7(1,R7),REC_ORIGIN                                               
         CLI   0(R3),X'23'                                                      
         BNE   *+10                                                             
         MVC   SEC_ORIGIN,2(R3)                                                 
         LA    R7,L'SECT_TAB(R7)                                                
         MVI   0(R7),X'FF'                                                      
         LR    R6,R5                                                            
         CLI   0(R3),0                                                          
         BE    BR25                                                             
*                                                                               
BR20     ZIC   RE,1(R3)            STORE ALL THE ELEMENTS TRAILING              
         SHI   RE,1                SECTION LEAD ELEMENT.                        
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),0(R3)                                                    
         LA    R5,1(RE,R5)                                                      
         LA    R3,1(RE,R3)                                                      
         B     BR10                                                             
*                                  END OF RECORD                                
BR25     STCM  R5,15,1(R7)         STORE ADDRESS OF NEXT AREA                   
         CLI   NEW_REC,C'N'                                                     
         BE    MERGEREC                                                         
BR30     MVI   NEW_REC,C'N'                                                     
         B     DELREC                                                           
         EJECT                                                                  
*                                                                               
************************************************************                    
* MERGE RECORDS.                                                                
* MAINLY GO THROUGH THE SECTION LEAD ELEMENT POINTER TABLE                      
* ZERO OUT THE ONES TO BE REPLACED.                                             
*                                                                               
* R7: POINTS TO THE SECTION LEAD ELEMENT POINTER TABLE                          
************************************************************                    
MERGEREC DS    0H                                                               
*                                  CLEAR RECORD FROM X'23' ELEM ON              
         GOTOR ,DMCB,SORTCRD,RECCRD                                             
         L     RF,=V(SORTER)                                                    
         BRAS  RE,CALL_DDS_SUBRTN                                               
*                                                                               
         LA    R4,2000             CLEAR RECORD BEYOND X'23'                    
         LA    R3,OUT_REC                                                       
         LR    R5,R3                                                            
         MVI   ELCODE,X'23'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    RE,R3               ADDRESS OF X'23'                             
         SR    RE,R5               DISPLACEMENT OF X'23'                        
         AHI   RE,4                                                             
         STCM  RE,3,OUT_RDW                                                     
         SHI   RE,4                LENGTH OF RECORD TO CLEAR                    
         SR    R4,RE                                                            
         LR    RF,R4                                                            
         LR    RE,R3                                                            
         XCEF                                                                   
*                                                                               
         LA    R7,SECT_TAB                                                      
MR10     CLI   0(R7),X'FF'                                                      
         BE    MR15                                                             
         GOTOR ,DMCB,=C'PUT',0(R7)                                              
         L     RF,=V(SORTER)                                                    
         BRAS  RE,CALL_DDS_SUBRTN                                               
         LA    R7,SLTABLEN(R7)                                                  
         B     MR10                                                             
*                                                                               
MR15     GOTOR ,DMCB,=C'GET'                                                    
         L     RF,=V(SORTER)                                                    
         BRAS  RE,CALL_DDS_SUBRTN                                               
         OC    DMCB+4(4),DMCB+4                                                 
         BZ    MR30                                                             
*                                                                               
MR20     L     R3,DMCB+4                                                        
         USING SLTABD,R3                                                        
         CLC   SLSEC,PREV_SEC     SECTION ALREADY LOADED VIA CONVERSION         
         BE    MR15                                                             
         MVC   PREV_SEC,SLSEC                                                   
*                                                                               
         L     R4,SLADD           ADD SECTION TO RECORD                         
MR25     GOTOR ,DMCB,(C'P',=C'DEMFIL'),OUT_REC,0(R4),=C'ADD=END'                
         L     RF,=V(HELLO)                                                     
         BRAS  RE,CALL_DDS_SUBRTN                                               
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RE,DMCB+12                                                       
         AHI   RE,4                                                             
         STCM  RE,3,OUT_RDW                                                     
         CLI   0(R4),X'5E'                                                      
         BE    MR15                                                             
         ZIC   RE,1(R4)                                                         
         AR    R4,RE                                                            
         B     MR25                                                             
*                                                                               
MR30     DS    0H                  PUT OUT THE FINALIZED RECORD.                
         GOTO1 =V(SORTER),DMCB,=C'END'                                          
         B     DELREC                                                           
*                                                                               
KEEPREC  LA    R1,OUT_RDW                                                       
         MVI   NEW_REC,C'Y'                                                     
         DS    0H                                                               
         SR    RF,RF               SET RC=0: KEEP RECORD                        
         B     GOBACK                                                           
*                                                                               
DELREC   DS    0H                                                               
         LHI   RF,4                SET RC=4: DELETE RECORD                      
         B     GOBACK                                                           
*                                                                               
EOF      DS    0H                                                               
         LHI   RF,8                SET RC=8:EOF                                 
*                                                                               
GOBACK   L     RD,4(,RD)                                                        
         L     RE,12(,RD)                                                       
         LM    R2,RC,28(RD)        RESTORE REGS                                 
         BR    RE                  RETURN                                       
         SPACE 3                                                                
         LTORG                                                                  
         SPACE 2                                                                
         GETEL R3,23,ELCODE                                                     
         EJECT                                                                  
*----------------------------------------------------------------------         
*                                                                               
* CALL A DDS SUBROUTINE. THIS REQUIRES ESTABLISHING A NEW RD CHAIN,             
* BECAUSE THIS PROGRAM IS A DFSORT EXIT, AND DOES NOT CONFORM TO DDS            
* STANDARD REGISTER USAGE.                                                      
*                                                                               
* INPUT REGISTERS ARE STANDARD:                                                 
*   R1 = A(PARAMETER LIST)                                                      
*   RE = RETURN ADDRESS                                                         
*   RF = A(ROUTINE TO CALL)                                                     
*                                                                               
*----------------------------------------------------------------------         
*                                                                               
CALL_DDS_SUBRTN DS 0H                                                           
         STM   RE,RC,GPRSAVE       SAVE CALLER'S REGISTERS                      
         LR    R0,RD               SAVE CALLER'S RD LOCALLY                     
         DROP  RC                                                               
*                                                                               
         BASR  RB,0                                                             
         AHI   RB,-2                                                            
         USING *-6,RB              MAKE THIS ROUTINE ADDRESSABLE                
         L     RD,=V(REGSAVE)      DDS WORKING STORAGE                          
*                                                                               
         BASR  RE,RF               CALL EXTERNAL SUBROUTINE                     
*                                                                               
         LR    RD,R0               RESTORE CALLER'S RD                          
         LM    RE,RC,GPRSAVE       RESTORE CALLER'S REGISTERS                   
         BSM   0,RE                EXIT                                         
         DROP  RB                                                               
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
         ORG   DEMRGU6+(((*-DEMRGU6)/256)+1)*256  FOR I-CACHE PIPELINE          
         SPACE 2                                                                
SAVE15   DS    18F                 SAVE DFSORT'S REGISTERS                      
GPRSAVE  DS    15F                 INTERNAL CALLER'S SAVED RE-RC                
DMCB     DS    6F                  PARAMETERS TO CALL_DDS_SUBRTN                
         SPACE 3                                                                
ELCODE   DS    X                                                                
*                                                                               
SEC_ORIGIN DS  X                   SECTION CODE                                 
REC_ORIGIN DS  C                   RECORD FROM TAPE OR FILE                     
NEW_REC  DC    C'Y'                RECORD RELEASED, START FRESH                 
PREV_SEC DS    X                   PREVIOUS MARKET BREAK                        
SECT_TAB DS    7XL8                ADDRESS+LENGTH+SRC OF EACH SECTION           
         DC    X'FF'                                                            
*                                                                               
SORTCRD  DC    CL80'SORT FIELDS=(7,2,A),FORMAT=BI '                             
RECCRD   DC    CL80'RECORD TYPE=F,LENGTH=8'                                     
*                                                                               
OUT_RDW  DS    F                   MERGED OUTPUT RECORD                         
OUT_REC  DS    2000X                                                            
ELEM_BUFFER DS 3000X                                                            
         EJECT                                                                  
SLTABD   DSECT                                                                  
SLADD    DS    A                                                                
SLLENG   DS    AL2                                                              
SLSEC    DS    XL1                 MARKET BREAK                                 
SLREC    DS    XL1                 FILE/CONVERSION                              
SLTABLEN EQU   *-SLTABD                                                         
*                                                                               
         EJECT                                                                  
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002DEMRGUXIT607/08/13'                                      
         END                                                                    
