*          DATA SET DELMXP1    AT LEVEL 005 AS OF 02/28/14                      
*PROCESS USING(WARN(15))                                                        
*PHASE DELMXP1A                                                                 
***********************************************************************         
* DFSORT USER EXIT FOR THE LOCAL MONTHLY PROGRAM AVERAGE.             *         
*                                                                     *         
* IT ASSIGNS A PROGRAM ID TO TO ALL THE TELECASTS OF A PROGRAM        *         
* THAT HAVE CONTIGUOUS OR OVERLAPPING QHS ACROSS ALL DAYS AND WEEKS.  *         
*                                                                     *         
* THIS IS A DFSORT USER EXIT PROGRAM. IT IS INVOKED VIA MEMBER        *         
* DELMICETPA, WHICH CONTAINS CONTROL CARDS TO ICETOOL/DFSORT.         *         
* SEE IBM'S DFSORT: APPLICATION PROGRAMMING GUIDE FOR DETAILS.        *         
*                                                                     *         
* NOTE: CALLS TO EXTERNAL SUBROUTINES *MUST* BE MADE VIA              *         
*       CALL_DDS_SUBRTN, BECAUSE THIS EXIT'S USE OF REGISTERS IS      *         
*       NON-DDS-CONFORMING.                                           *         
***********************************************************************         
ICEPAX0  CSECT                                                                  
*                                                                               
         ENTRY E35                 MUST BE "E35" (DFSORT OUTPUT EXIT)           
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
**********************************************************************          
* ASSIGN THE PROGRAM ID.                                                        
*                                                                               
* INPUT:  QUARTER HOUR RECORDS THAT HOLD BOTH THE TIME PERIOD AND               
* -----   PROGRAM INFORMATION. RECORDS ARE SORTED BY                            
*         PROGRAM NAME/QUARTER HOUR.                                            
*                                                                               
* OUTPUT: KEEP THE WORK RECORDS AND FILL IN THE PROGRAM ID SLOT.                
* ------                                                                        
*                                                                               
* RULES:  TELECASTS WITH THE SAME PROGRAM NAME AND CONTIGUOUS OR                
* ------  OVERLAPPING QUARTER HOURS GET THE SAME PROGRAM ID.                    
*         RESET THE PROGRAM ID WHEN THE PROGRAM NAME CHANGES.                   
*         A PROGRAM THAT CROSSES DAYS WILL BE SPLIT AT 5 AM AND                 
*         ASSIGNED A SEPARATE ID STARTING WITH 5AM.                             
*                                                                               
**********************************************************************          
*                                                                               
         L     R3,0(R2)            LOAD A(RECORD)                               
*                                                                               
         LTR   R3,R3               EOF?                                         
         BNZ   MAIN20              NO                                           
         B     EOF                 YES. WE ARE DONE.                            
*                                                                               
         USING LMDSECT,R3                                                       
*                                                                               
MAIN20   DS    0X                                                               
         CLC   W_KDTYPE,PREV_KDTYPE     CHANGE IN DATA TYPE,                    
         BNE   NEWPRGNM                  NEW PROGRAM                            
         CLC   W_PNRPNAME,PREV_PNRPNAME CHANGE IN PROGRAM NAME,                 
         BNE   NEWPRGNM                  NEW PROGRAM                            
*                                                                               
         ZIC   R1,W_KQHR           CHECK FOR OVERLAPPING OR CONTIGUOUS          
         ZIC   R0,PREV_KQHR        QUARTER HOURS.                               
         SR    R1,R0                                                            
         LTR   R1,R1               OVERLAPPING QHS (SAME QH AS PREV)?           
         BZ    SAMEID              YES. USE THE SAME PROGRAM ID.                
         CHI   R1,1                CONTIGUOUS QHS (QH GREATER BY ONE)?          
         BNE   NEWID               NO. NEED NEW PROGRAM ID.                     
         CLI   W_KQHR,0            SPLIT AT 5:00 AM (QUARTER HOUR ZERO)         
         BE    NEWID               NEED NEW PROGRAM ID.                         
         B     SAMEID              CONTIGUOUS QHS. SAME PROG ID                 
*                                                                               
*                                                                               
***********************************************************************         
* NEW PROGRAM NAME. RESTART THE PROGRAM ID.                                     
***********************************************************************         
NEWPRGNM DS    0X                                                               
         MVC   PROG_ID,=A(0)                                                    
*                                                                               
***********************************************************************         
* SAME PROGRAM NAME, BUT QUARTER HOURS ARE NOT OVERLAPPING OR                   
* CONTIGUOUS. NEED NEW PROGRAM ID.                                              
***********************************************************************         
NEWID    L     R1,PROG_ID                                                       
         AHI   R1,1                                                             
         ST    R1,PROG_ID                                                       
*                                                                               
***********************************************************************         
* SAME PROGRAM NAME, AND QUARTER HOURS ARE OVERLAPPING OR CONTIGUOUS.           
* USE THE SAME PROGRAM ID.                                                      
***********************************************************************         
SAMEID   DS      0X                                                             
         MVC   W_PNRPGID,PROG_ID   ADD THE ASSIGNED PROGRAM ID TO RECD          
         MVC   PREV_KDTYPE,W_KDTYPE       SAVE PREVIOUS VALUES                  
         MVC   PREV_PNRPNAME,W_PNRPNAME                                         
         MVC   PREV_KQHR,W_KQHR                                                 
*                                                                               
         B     KEEPREC             AND KEEP THE RECORD                          
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* DFSORT ACTIONS                                                                
***********************************************************************         
KEEPREC  DS    0H                                                               
         SGR   GRF,GRF             SET RC=0: KEEP RECORD                        
         SGR   GR1,GR1                                                          
         LR    R1,R3               SET RECORD POINTER                           
         B     GOBACK                                                           
*                                                                               
DELREC   DS    0H                                                               
         LGHI  GRF,4               SET RC=4: DELETE RECORD                      
         B     GOBACK                                                           
*                                                                               
ADDREC   DS    0H                                                               
         LGHI  GRF,12              SET RC=12: INSERT RECORD                     
         SGR   GR1,GR1                                                          
         LA    R1,OUTREC           SET RECORD POINTER                           
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
*                                                                               
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
         ORG   ICEPAX0+(((*-ICEPAX0)/256)+1)*256  FOR I-CACHE PIPELINE          
         SPACE 2                                                                
SAVE15   DS    18F                 SAVE DFSORT'S REGISTERS                      
DFSORT_HIGH_HALVES DS 16F                                                       
*                                                                               
PREV_KDTYPE DC XL(L'W_KDTYPE)'00'                                               
PREV_PNRPNAME DS CL(L'W_PNRPNAME)                                               
PREV_KQHR DS XL(L'W_KQHR)                                                       
*                                                                               
PROG_ID  DC    A(0)                                                             
*                                                                               
         DS    0L                                                               
OUTREC   DS    CL(W_LMRECLQ)  OUTPUT RECORD                                     
*                                                                               
         EJECT                                                                  
* INPUT RECORDS DSECT                                                           
       ++INCLUDE DELMDSECT                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005DELMXP1   02/28/14'                                      
         END                                                                    
