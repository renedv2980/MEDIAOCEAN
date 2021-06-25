*          DATA SET DEMRGUXIT2 AT LEVEL 004 AS OF 02/28/14                      
*PROCESS USING(WARN(15))                                                        
*PHASE DEMRGU2A                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE BINSRCH2                                                               
*INCLUDE HEXOUT                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
***********************************************************************         
*                                                                     *         
* THIS IS A DFSORT USER EXIT PROGRAM. IT IS INVOKED VIA MEMBER        *         
* DEMRGICEG, WHICH CONTAINS CONTROL CARDS TO ICETOOL/DFSORT.          *         
* SEE IBM'S DFSORT: APPLICATION PROGRAMMING GUIDE FOR DETAILS.        *         
*                                                                     *         
* NOTE: CALLS TO EXTERNAL SUBROUTINES *MUST* BE MADE VIA              *         
*       CALL_DDS_SUBRTN, BECAUSE THIS EXIT'S USE OF REGISTERS IS      *         
*       NON-DDS-CONFORMING.                                           *         
*                                                                     *         
* THIS MODULE READS THE OUTPUT OF A PREVIOUS SORT, AND MERGES         *         
* TOGETHER ALL OF THE REPLACEMENT RANGE MINOR KEY DATA FOR EACH MAJOR *         
* KEY INTO A SINGLE RECORD FOR THAT KEY. A SYSPRINT REPORT IS         *         
* PRODUCED SHOWING THE REPLACEMENT RANGES FOUND (AS A REALITY CHECK). *         
*                                                                     *         
***********************************************************************         
DEMRGU2  CSECT                                                                  
*                                                                               
         ENTRY E35                 MUST BE "E35" (FOR DFSORT)                   
*                                                                               
         PRINT NOGEN                                                            
         REQUS                                                                  
*                                                                               
         USING E35,RC              RC = PROGRAM BASE REGISTER                   
E35      STM   RE,RC,12(RD)        SAVE ALL REGS EXCEPT RD                      
         LA    RC,0(RF)            SET PROGRAM BASE REGISTER                    
         STMH  GR0,GRF,DFSORT_HIGH_HALVES                                       
         ST    RD,SAVE15+4         SAVE BACKWARD POINTER                        
         LA    RE,SAVE15           SET FORWARD POINTER...                       
         ST    RE,8(RD)            ...IN SAVE AREA                              
         LR    RD,RE               SET OUR SAVE AREA                            
         LR    R2,R1               SAVE PARAMETER LIST POINTER                  
*                                                                               
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
*                                                                               
         BC    0,MAIN10            *** SELF-MODIFYING CODE ***                  
         MVI   *-3,X'F0'           *** ONLY DO THIS ONCE   ***                  
*                                                                               
         L     RF,=V(PRNTER)       A(SYSPRINT DCB)                              
         MVC   DCBDDNAM-IHADCB(8,RF),=C'REPTRACE'  DDNAME=REPTRACE              
*                                                                               
         MVC   P(26),=C'*** REPLACEMENT RANGES ***'                             
         L     RF,=V(PRINTER)                                                   
         BRAS  RE,CALL_DDS_SUBRTN                                               
*                                                                               
MAIN10   DS    0H                                                               
         LA    R5,OUT_REC          BUILD OUTPUT RECORD HERE                     
         USING SMAJKEY,R5                                                       
*                                                                               
         L     R3,0(R2)            LOAD A(RECORD)                               
         LTR   R3,R3               EOF?                                         
         BZ    EOF                 YES: DO NOT RETURN                           
*                                                                               
         LA    R3,4(R3)            POINT PAST RDW                               
*                                                                               
         CLC   =C'*END OF MAJOR KEY*',18(R3)                                    
         BE    PROCKEY             NO MORE FOR THIS MAJOR KEY                   
*                                                                               
         MVI   ELCODE,MRGRCDEQ     SPECIAL "REPLACEMENT DATA" ELEMENT           
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING MRGRELEM,R3                                                      
         CLC   MRGREYEC,=C'*MERGE REPLACEMENT ELEMENT*'                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R4,DUB                                                           
         USING REPTABLD,R4                                                      
         L     R7,0(R2)            A(RECORD)                                    
         MVC   MINORKEY,18+4(R7)   MINOR KEY (L'RDW =4)                         
         MVC   LOW_HIGH,MRGRTYPE   LOW/HIGH INDICATOR                           
         GOTOR ,DMCB,(X'01',DUB),SREPTABL,NUMVALS,REPTABLQ,            +        
               (0,REPTABLQ),L'SREPTABL/REPTABLQ                                 
         L     RF,=V(BINSRCH)                                                   
         BRAS  RE,CALL_DDS_SUBRTN                                               
         OC    0(4,R1),0(R1)       TABLE FULL?                                  
         BNZ   *+6                                                              
         DC    H'0'                RANGES DON'T ALL FIT IN ONE RECORD !         
         MVC   NUMVALS,DMCB+8      CURRENT NUMBER OF VALUES IN TABLE            
         DROP  R3,R4                                                            
*                                                                               
         B     DELREC              THROW THIS INTERMEDIATE RECORD AWAY          
         EJECT                                                                  
PROCKEY  DS    0H                                                               
*                                                                               
* PRINT THE REPLACEMENT KEY TABLE, IF PRESENT, AND CONFIRM ITS                  
* INTEGRITY.                                                                    
*                                                                               
         ICM   R6,15,NUMVALS       ANY DATA TO REPLACE?                         
         BZ    EOF                 NO: EOF (DO NOT RETURN)                      
*                                                                               
         TML   R6,X'0001'          # OF RECORDS IS DIVISIBLE BY TWO?            
         BZ    *+6                 YES                                          
         DC    H'0'                ODD NUMBER OF SIGNAL RECORDS                 
*                                                                               
         LA    R4,SREPTABL         REPLACEMENT KEY TABLE                        
         USING REPTABLD,R4                                                      
         MVI   TOGGLE,X'00'        TOGGLE FLIPS BETWEEN X'00' AND X'FF'         
*                                   WHICH ARE THE ONLY VALID VALUES FOR         
*                                   FIELD LOW_HIGH                              
*                                                                               
NEXTMINK DS    0H                                                               
         L     R7,0(R2)            A(RECORD)                                    
         GOTOR ,DMCB,4(R7),PMAJKEYX,18,=C'TOG'                                  
         L     RF,=V(HEXOUT)                                                    
         BRAS  RE,CALL_DDS_SUBRTN                                               
         OC    DMCB+16(4),DMCB+16  PRINT HEX MAJOR KEY                          
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   PMAJKEYA,0+4(R7)    PRINT ALPHA MAJOR KEY (L'RDW = 4)            
         MVC   PRANGTYP,=C'LOW:  ' ASSUME THE MINOR KEY IS LOW                  
         CLI   LOW_HIGH,MRGRLOW                                                 
         BE    *+10                                                             
         MVC   PRANGTYP,=C'HIGH: ' NO: IT'S HIGH                                
*                                                                               
         GOTOR ,DMCB,MINORKEY,PMINKEYX,L'MINORKEY,=C'TOG'                       
         L     RF,=V(HEXOUT)                                                    
         BRAS  RE,CALL_DDS_SUBRTN                                               
         OC    DMCB+16(4),DMCB+16  PRINT HEX MINOR KEY                          
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,=V(PRINTER)                                                   
         BRAS  RE,CALL_DDS_SUBRTN                                               
*                                                                               
         CLC   LOW_HIGH,TOGGLE     SEQUENCE *MUST* BE:                          
         BE    *+6                  LOW,HIGH,LOW,HIGH,ETC...                    
         DC    H'0'                LOW MINOR KEY EXPECTED                       
*                                                                               
         LA    R4,REPTABLQ(R4)     BUMP TO NEXT TABLE ENTRY                     
         XI    TOGGLE,X'FF'        FLIP BETWEEN X'00' AND X'FF'                 
*                                                                               
         BCT   R6,NEXTMINK         ANY MORE ENTRIES?                            
         DROP  R4                                                               
*                                                                               
         MVI   P,0                 NO: SKIP A LINE                              
         L     RF,=V(PRINTER)                                                   
         BRAS  RE,CALL_DDS_SUBRTN                                               
*                                                                               
         L     R7,0(R2)            A(RECORD)                                    
         MVC   SMAJKEY,0+4(R7)     MAJOR KEY (L'RDW = 4)                        
         XC    SMINKEY,SMINKEY                                                  
         XC    SRECLEN,SRECLEN                                                  
         MVI   SSTATUS,0                                                        
         MVC   SEYECAT,=C'*REPLACEMENT RANGES*'                                 
         MVC   SNUMENTS,NUMVALS    NUMBER OF TABLE ENTRIES                      
         LHI   RF,SREPTABL-SSIGNAL OVERHEAD LENGTH OF RECORD                    
         L     R1,NUMVALS          NUMBER OF TABLE ENTRIES...                   
         MHI   R1,REPTABLQ         ...TIMES L'ENTRY...                          
         LA    RF,4(R1,RF)         ...PLUS OVERHEAD LENGTH + L'RDW...           
         STCM  RF,3,OUT_RDW        ...EQUALS RECORD LENGTH                      
*                                                                               
         XC    NUMVALS,NUMVALS     GET SET FOR NEXT MAJOR KEY                   
*                                                                               
         DROP  R5                                                               
*                                                                               
KEEPREC  DS    0H                                                               
         SGR   GRF,GRF             SET RC=0: KEEP RECORD                        
         SGR   GR1,GR1                                                          
         LA    R1,OUT_RDW          SET RECORD POINTER                           
         B     GOBACK                                                           
*                                                                               
DELREC   DS    0H                                                               
         LGHI  GRF,4               SET RC=4: DELETE RECORD                      
         B     GOBACK                                                           
*                                                                               
EOF      DS    0H                                                               
         GOTOR ,DMCB,=C'CLOSE'     CLOSE SYSPRINT                               
         L     RF,=V(PRINT)                                                     
         BRAS  RE,CALL_DDS_SUBRTN                                               
*                                                                               
         LGHI  GRF,8               SET RC=8:EOF                                 
*                                                                               
GOBACK   DS    0H                                                               
         LMH   GR0,GR0,DFSORT_HIGH_HALVES                                       
         LMH   GR2,GRE,DFSORT_HIGH_HALVES+8                                     
         L     RD,4(,RD)                                                        
         L     RE,12(,RD)                                                       
         LM    R2,RC,28(RD)        RESTORE REGS                                 
         BSM   0,RE                RETURN                                       
         SPACE 3                                                                
         LTORG                                                                  
         SPACE 3                                                                
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
         ORG   DEMRGU2+(((*-DEMRGU2)/256)+1)*256  FOR I-CACHE PIPELINE          
         SPACE 2                                                                
SAVE15   DS    18F                 SAVE DFSORT'S REGISTERS                      
GPRSAVE  DS    15F                 INTERNAL CALLER'S SAVED RE-RC                
DFSORT_HIGH_HALVES DS 16F                                                       
DMCB     DS    6F                  PARAMETERS TO CALL_DDS_SUBRTN                
         SPACE 3                                                                
ELCODE   DS    X                                                                
TOGGLE   DS    X                   FLIPS BETWEEN X'00' AND X'FF'                
*                                                                               
NUMVALS  DC    A(0)                CURRENT NUMBER OF VALUES IN TABLE            
*                                                                               
DUB      DS    D                                                                
*                                                                               
OUT_RDW  DS    F                   MERGED OUTPUT RECORD                         
OUT_REC  DS    2000X                                                            
         EJECT                                                                  
       ++INCLUDE DEMRGUXITD                                                     
         EJECT                                                                  
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
         ORG   P                                                                
PMAJKEYX DS    CL(18*2)            MAJOR KEY (HEXOUT FORMAT)                    
         DS    CL2                                                              
PMAJKEYA DS    CL18                MAJOR KEY (ALPHA)                            
         DS    CL2                                                              
PRANGTYP DS    CL6                 'LOW:  ' OR 'HIGH: '                         
PMINKEYX DS    CL(2*2)             MINOR KEY (HEXOUT FORMAT)                    
         ORG                                                                    
         SPACE 3                                                                
         DCBD  DSORG=PS,DEVD=DA                                                 
         SPACE 3                                                                
* DEDEMFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE DEDEMFILE                                                      
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004DEMRGUXIT202/28/14'                                      
         END                                                                    
