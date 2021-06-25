*          DATA SET DEMRGUXITJ AT LEVEL 006 AS OF 11/29/17                      
*PROCESS USING(WARN(15))                                                        
*PHASE DEMRGUJA                                                                 
***********************************************************************         
*                                                                     *         
* THIS IS A DFSORT USER EXIT PROGRAM. IT IS INVOKED VIA MEMBER        *         
* DEMRGICEG, WHICH CONTAINS CONTROL CARDS TO ICETOOL/DFSORT.          *         
* SEE IBM'S DFSORT: APPLICATION PROGRAMMING GUIDE FOR DETAILS.        *         
*                                                                     *         
* THIS MODULE IS A COMPONENT OF THE GENERALIZED DEMO MERGE UTILITY.   *         
* IT READS A DATASET WHICH HAS BEEN PRODUCED BY A FULL OUTER JOIN.    *         
* FILE1 (F1) IS THE CONVERSION OUTPUT. FILE2 (F2) IS THE CURRENT      *         
* DEMOFILE OUTPUT. DFSORT'S REFORMAT OPERATOR BUILDS THE RECORDS      *         
* THAT COME INTO THIS EXIT AFTER THE JOIN. EACH RECORD LOOKS LIKE     *         
* ONE OF THESE:                                                       *         
*                                                                     *         
*   <RDW><F1-RDW><F1-RECORD>                    (CONVERSION ONLY)     *         
*   <RDW><F2-RDW><F2-RECORD>                    (DEMOFILE ONLY)       *         
*   <RDW><F1-RDW><F1-RECORD><F2-RDW><F2-RECORD> (PRESENT ON BOTH)     *         
*                                                                     *         
* CONSIDER THE BRILLIANCE OF THIS STRATEGY:                           *         
*   1. WE ALWAYS WANT TO KEEP ALL UNPAIRED RECORDS.                   *         
*   2. IN THE CASE OF PAIRED RECORDS, WE ALWAYS WANT TO KEEP THE F1   *         
*       RECORD (I.E., THE ONE THAT CAME FROM THE CONVERSION, AS       *         
*       OPPOSED TO THE DEMOFILE).                                     *         
* THEREFORE, GIVEN THE WAY THE RECORDS ARE FORMATTED, WE CAN ALWAYS   *         
* UNCONDITIONALLY MODIFY THE OUTPUT RECORD TO POINT JUST PAST THE     *         
* ORIGINAL RDW. THIS ALWAYS YIELDS THE CORRECT OUTPUT RECORD. QED.    *         
*                                                                     *         
* FEB/2016: LOGIC WAS ADDED TO DISCARD DUPLICATE PASSIVE KEYS. THIS   *         
*           IS A PREREQUISITE FOR VSAM.                               *         
*                                                                     *         
***********************************************************************         
DEMRGUJ  CSECT                                                                  
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
*                                                                               
         L     R3,0(R1)            LOAD A(RECORD)                               
         LTR   R3,R3               EOF?                                         
         BZ    EOF                 YES: DO NOT RETURN                           
*                                                                               
         USING RECORD,R3                                                        
         SGR   GR1,GR1             A(RETURNED RECORD)                           
         LA    R1,RDW              ALWAYS KEEP THE FIRST "RECORD"               
         DROP  R3                                                               
         USING RDW,R1                                                           
         NI    STATUS,X'FF'-X'3F'  RESET LOGICAL FILE NUMBER BITS               
         CLC   DIRREC(L'MAJORKEY+L'MINORKEY),PREVKEY                            
         JL    *+2                 NOT IN MAJOR/MINOR KEY SEQUENCE ?!?          
         CLC   =AL2(DIRRECLQ+L'RDW),RDW  IS THIS A PASSIVE?                     
         BNE   KEEPREC             NO: KEEP IT                                  
         CLC   PREVKEY,DIRREC      COMPARE ENTIRE DIRECTORY RECORD              
         BE    DELREC              SAME AS PREVIOUS: DISCARD IT                 
*                                                                               
KEEPREC  DS    0H                                                               
         MVC   PREVKEY,DIRREC      SAVE THE ENTIRE DIRECTORY PORTION            
         DROP  R1                                                               
*                                                                               
         SGR   GRF,GRF             SET RC=0: KEEP RECORD                        
         B     GOBACK                                                           
*                                                                               
DELREC   DS    0H                                                               
         LGHI  GRF,4               SET RC=4:  DELETE RECORD                     
         B     GOBACK                                                           
*                                                                               
EOF      DS    0H                                                               
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
         SPACE 2                                                                
         ORG   DEMRGUJ+(((*-DEMRGUJ)/256)+1)*256  FOR I-CACHE PIPELINE          
         SPACE 2                                                                
SAVE15   DS    18F                 SAVE DFSORT'S REGISTERS                      
DFSORT_HIGH_HALVES DS 16F                                                       
PREVKEY  DC    XL(DIRRECLQ)'00'    LAST RECORD (KEY PORTION)                    
         SPACE 3                                                                
RECORD   DSECT                                                                  
         DS    F                   RDW OF RECORD FROM DFSORT                    
RDW      DS    XL4                 RDW OF RECORD TO BE KEPT                     
DIRREC   DS    0X                                                               
MAJORKEY DS    XL18                                                             
MINORKEY DS    XL2                                                              
RECLEN   DS    XL2                                                              
STATUS   DS    X                                                                
DIRRECLQ EQU   *-DIRREC                                                         
         DS    0X                  REMAINDER OF RECORD TO BE KEPT               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006DEMRGUXITJ11/29/17'                                      
         END                                                                    
