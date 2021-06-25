*          DATA SET DELNK00    AT LEVEL 010 AS OF 05/12/10                      
*$PANAPT01S$ *******  FIRST LINE TO DELETE  *****************                   
*                                                           *                   
*  ATTN: THE LEVEL STAMPS ARE *LEGITIMATELY* OUT-OF-SYNC!   *                   
*        DELETE THIS COMMENT BLOCK ON THE NEXT PROMOTION!   *                   
*                                                           *                   
*  THIS SOURCE MODULE IS AT A HIGHER LEVEL NUMBER THAN ITS  *                   
*  CORRESPONDING LOAD OR OBJECT MODULE, BECAUSE THE MEMBER  *                   
*  WAS PROMOTED USING THE SRCE LIBCODE VIA MR# 035556.      *                   
*                                                           *                   
*  THIS COMMENT BLOCK WAS INSERTED *PROGRAMMATICALLY* BY    *                   
*  PANAPT, TO EXPLAIN WHY THE LEVEL STAMPS ARE OUT-OF-SYNC. *                   
*  BEFORE THIS MEMBER CAN BE PROMOTED AGAIN, THIS ENTIRE    *                   
*  COMMENT BLOCK *MUST* BE MANUALLY DELETED.                *                   
*                                                           *                   
*$PANAPT01X$ *******  LAST LINE TO DELETE  ******************                   
*PHASE TF2F00A,*                                                                
DELNK00  TITLE '- DEMO SYSTEM DDLINK INTERFACE'                                 
DELNK00  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKL,**LK00**,RR=RE,CLEAR=YES                                   
         LR    R9,RC                                                            
         USING WORKD,R9            R9=A(GLOBAL W/S)                             
         LA    R8,WORKD                                                         
         AHI   R8,4096                                                          
         USING WORKD+4096,R8                                                    
                                                                                
         ST    RB,BASEADDR                                                      
         ST    RE,BASERELO                                                      
         ST    RD,BASERD                                                        
         ST    R1,AFAPARM                                                       
                                                                                
         MVC   ATIOB,0(R1)                                                      
         MVC   ATWA,4(R1)                                                       
         MVC   ATIA,12(R1)                                                      
         MVC   ACOMFACS,16(R1)                                                  
                                                                                
         L     RA,ATWA                                                          
         USING TWAD,RA                                                          
                                                                                
         L     RF,ACOMFACS                                                      
         L     RF,CCALLOV-COMFACSD(RF)                                          
         LA    R1,DMCB                                                          
         GOTOR (RF),(R1),(1,0),0,0                                              
         MVC   AROUT1,0(R1)        SET A(ROUTINE OVERLAY 1)                     
         GOTOR (RF),(R1),(2,0),0,0                                              
         MVC   AROUT2,0(R1)        SET A(ROUTINE OVERLAY 2)                     
                                                                                
         GOTOR (#WRKINI,AWRKINI)   INITIALIZE WORKING STORAGE                   
                                                                                
***********************************************************************         
* INITIALIZE CONTROL BLOCKS AND CALL DDLINK                           *         
***********************************************************************         
                                                                                
INIT06   LA    R2,TWAD                                                          
         AHI   R2,FABLK-TWAD                                                    
         ST    R2,AFABLK                                                        
         USING FALINKD,R2                                                       
         USING RUNPARMD,DDRUNP     RUNPARMD EMULATION                           
         USING RUNFACSD,DDRUNF     RUNFACS EMULATION                            
         USING LP_D,DDLINKC        DDLINK CONTROL BLOCK                         
         USING WRKIOD,DDWRKIOC     WRKIO CONTROL BLOCK                          
                                                                                
         OI    LNKSERVH+(FHAT-FHD),FHATMO                                       
         OI    LNKSERVH+(FHOI-FHD),FHOITR                                       
         OI    LNKINPH+(FHOI-FHD),FHOICU+FHOITR                                 
                                                                                
         LA    R0,LNKINPH          SET A(FIRST SCREEN POSITION)                 
         ST    R0,FALABLD                                                       
         MVC   FALTBLD,VTWABLD     A(TWABLD)                                    
         MVC   FALASWCH,VSWITCH    A(SWITCH)                                    
         XC    FAMSGBLK,FAMSGBLK                                                
         LA    R0,FAMSGBLK         A(MESSAGE BLOCK)                             
         ST    R0,FALAMSG                                                       
         LA    R0,FACON            A(CONTROL FIELD BUFFER)                      
         ST    R0,FALACON                                                       
         L     R0,ATWA                                                          
         AHI   R0,SVFALINK-TWAD    A(FALINK SAVED STORAGE)                      
         ST    R0,FALASVE                                                       
         LHI   R0,FALATMS                                                       
         STCM  R0,15,FALAPGS                                                    
***********************************************************************         
* INITIALIZE ONLINE RUNNER VALUES (RUNFACSD)                          *         
***********************************************************************         
                                                                                
         LA    R0,TWAD             PASS A(4K SAVE AREA)                         
         AHI   R0,SVSERVER-TWAD                                                 
         STCM  R0,15,RSVRSAVE                                                   
         LA    R0,DDWRKIOC                                                      
         ST    R0,RWRKBLK1         SET A(WRKIO CONTROL BLOCK)                   
         MVC   RCOMFACS,ACOMFACS                                                
         MVC   RWRKIO,VWRKIO                                                    
* RDDLINK HAS BEEN REMOVED BY JNEW - RELINK WILL CAUSE AN ASSEMBLY              
* ERROR. IT SHOULD BE SAFE TO REMOVE THE FOLLOWING MVC INSTRUCTION              
* ON THE NEXT RELINK, BUT THE CODE SHOULD BE TESTED.                            
         MVC   RDDLINK,VDDLINK                                                  
                                                                                
         MVC   RRUNIT,VRUNIT                                                    
         LA    R0,RUNFACSD                                                      
         STCM  R0,7,RUNPARUN       SET A(RUNFACS) IN RUNPARMS                   
                                                                                
***********************************************************************         
* INITIALIZE DDLINK CONTROL BLOCK VALUES (LP_D)                       *         
***********************************************************************         
                                                                                
         MVI   LP_FLAG,0           PATCHABLE FLAG BYTE                          
         MVC   LP_USRID,TWAUSRID   SET USER-ID                                  
         MVC   LP_ACCS,TWAACCS     SET LIMIT ACCESS BYTES                       
         MVC   LP_AUIR1,AROUT1     SET A(INDEX ROUTINES 1)                      
         MVC   LP_AUIR2,AROUT2     SET A(INDEX ROUTINES 2)                      
         LA    R0,TWAD                                                          
         ST    R0,LP_ATWA          SET A(TWA)                                   
         AHI   R0,SVDDLINK-TWAD                                                 
         ST    R0,LP_ASAVE         SET A(DDLINK SAVE AREA)                      
         LHI   R0,DDNDX-DELNK00                                                 
         A     R0,BASEADDR                                                      
         STCM  R0,15,LP_ANDX                                                    
         MVC   LP_ATIOB,ATIOB      SET A(TIOB)                                  
         MVC   LP_ACOM,ACOMFACS    SET A(COMFACS)                               
         LA    R0,WORKD                                                         
         AHI   R0,LINKWORK-WORKD                                                
         STCM  R0,15,LP_AWORK                                                   
         LA    R0,RUNPARMD                                                      
         ST    R0,LP_ARUNP         SET A(DUMMY RUNPARMS)                        
         MVC   LP_AFBLK,AFABLK                                                  
         MVC   LP_AFALK,VFALINK    SET A(FALINK)                                
         LA    R0,WORKD                                                         
         ST    R0,LP_ABLK1         ALWAYS PASS A(W/S) AS BLOCK 1                
                                                                                
         LR    R1,RD               ACQUIRE W/S FOR LP_AUWMP                     
         AHI   R1,14000                                                         
         L     RE,4(RD)                                                         
         ST    RE,4(R1)                                                         
         ST    R1,8(RE)                                                         
         LR    R2,RD                                                            
         LR    RD,R1                                                            
                                                                                
         LR    R0,R2                                                            
         LHI   R1,14000                                                         
         STH   R1,LP_WMPL        TELL DDLINK L'EXTENDED BUFFER                  
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         ST    R2,LP_AUWMP                                                      
                                                                                
***********************************************************************         
* INITIALIZE WRKIO CONTROL BLOCK VALUES (WRKIOD)                      *         
***********************************************************************         
                                                                                
         MVC   WRKIACOM,ACOMFACS                                                
         MVC   WRKIAREC,AIO4       IO2 USED FOR WORKER RECORD AREA              
         MVC   WRKIABUF,ATIA       PASS A(TIA) AS WORKER BUFFER                 
                                                                                
         GOTOR VDDLINK,LP_D        PASS CONTROL TO DDLINK                       
                                                                                
EXITY    CR    RE,RE                                                            
EXIT     XIT1  ,                                                                
         DROP  R2                                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
DDNDX    DS    0H                  DDLINK MASTER MAP INDEX                      
                                                                                
M#INIT   EQU   1                   INITIAL DOWNLOAD                             
O#INIT   EQU   X'10'                                                            
         DC    AL2(M#INIT),AL1(O#INIT)                                          
         DC    AL1(LP_RNOWQ,0)                                                  
         DC    AL1(08),AL2(RE#INIT)                                             
                                                                                
M#STAT   EQU   2                   STATION LIST                                 
O#STAT   EQU   X'10'                                                            
         DC    AL2(M#STAT),AL1(O#STAT)                                          
         DC    AL1(LP_RNOWQ,0)                                                  
         DC    AL1(08),AL2(RE#STA)                                              
                                                                                
M#MRKT   EQU   3                   MARKET LIST                                  
O#MRKT   EQU   X'10'                                                            
         DC    AL2(M#MRKT),AL1(O#MRKT)                                          
         DC    AL1(LP_RNOWQ,0)                                                  
         DC    AL1(08),AL2(RE#MKTL)                                             
                                                                                
M#VALSTA EQU   4                   VALIDATE STATION SYNTAX  LIST                
O#VALSTA EQU   X'10'                                                            
         DC    AL2(M#VALSTA),AL1(O#VALSTA)                                      
         DC    AL1(LP_RNOWQ,0)                                                  
         DC    AL1(08),AL2(RE#VSTAT)                                            
                                                                                
M#VALSBK EQU   5                   VALIDATE BOOK FOR STATIONS                   
O#VALSBK EQU   X'10'                                                            
         DC    AL2(M#VALSBK),AL1(O#VALSBK)                                      
         DC    AL1(LP_RNOWQ,0)                                                  
         DC    AL1(08),AL2(RE#VBOOK)                                            
                                                                                
M#VALDTM EQU   6                   VALIDATE DAYTIME EXPRESSION                  
O#VALDTM EQU   X'10'                                                            
         DC    AL2(M#VALDTM),AL1(O#VALDTM)                                      
         DC    AL1(LP_RNOWQ,0)                                                  
         DC    AL1(08),AL2(RE#VDYTM)                                            
                                                                                
M#VALDEM EQU   7                   VALIDATE DEMO EXPRESSION                     
O#VALDEM EQU   X'10'                                                            
         DC    AL2(M#VALDEM),AL1(O#VALDEM)                                      
         DC    AL1(LP_RNOWQ,0)                                                  
         DC    AL1(08),AL2(RE#VDEMO)                                            
                                                                                
M#SPILL  EQU   8                   SPILL LIST                                   
O#SPILL  EQU   X'10'                                                            
         DC    AL2(M#SPILL),AL1(O#SPILL)                                        
         DC    AL1(LP_RNOWQ,0)                                                  
         DC    AL1(08),AL2(RE#SPILL)                                            
                                                                                
M#BOOKL  EQU   9                   BOOKS LIST                                   
O#BOOKL  EQU   X'10'                                                            
         DC    AL2(M#BOOKL),AL1(O#BOOKL)                                        
         DC    AL1(LP_RNOWQ,0)                                                  
         DC    AL1(08),AL2(RE#BOOKS)                                            
                                                                                
M#DEMOL  EQU   10                  DEMOS LIST                                   
O#DEMOL  EQU   X'10'                                                            
         DC    AL2(M#DEMOL),AL1(O#DEMOL)                                        
         DC    AL1(LP_RNOWQ,0)                                                  
         DC    AL1(08),AL2(RE#DEMOS)                                            
                                                                                
M#VUPGD  EQU   13                  VALIDATE UPGRADES                            
O#VUPGD  EQU   X'10'                                                            
         DC    AL2(M#VUPGD),AL1(O#VUPGD)                                        
         DC    AL1(LP_RNOWQ,0)                                                  
         DC    AL1(08),AL2(RE#VUPGD)                                            
                                                                                
M#PGDET  EQU   20                  PROGRAM DETAILS                              
O#PGDET  EQU   X'11'                                                            
         DC    AL2(M#PGDET),AL1(O#PGDET)                                        
         DC    AL1(LP_RNOWQ+LP_RNWKQ+LP_RCONQ+LP_RDSCQ,0)                       
****     DC    AL1(LP_RNOWQ,0)                                                  
         DC    AL1(08),AL2(RE#COMDL)                                            
                                                                                
M#PRGHD  EQU   21                  PROGRAM HEADERS FOR PAV                      
O#PRGHD  EQU   X'11'                                                            
         DC    AL2(M#PRGHD),AL1(O#PRGHD)                                        
         DC    AL1(LP_RNOWQ,0)                                                  
         DC    AL1(08),AL2(RE#PAVBO)                                            
                                                                                
M#INVHD  EQU   22                  PROGRAM HEADERS FOR INV                      
O#INVHD  EQU   X'11'                                                            
         DC    AL2(M#INVHD),AL1(O#INVHD)                                        
         DC    AL1(LP_RNOWQ,0)                                                  
         DC    AL1(08),AL2(RE#INVBO)                                            
                                                                                
M#PRECAL EQU   23                  DETAILS RECALC                               
O#PRECAL EQU   X'11'                                                            
         DC    AL2(M#PRECAL),AL1(O#PRECAL)                                      
         DC    AL1(LP_RNOWQ,0)                                                  
         DC    AL1(08),AL2(RE#RECAL)                                            
                                                                                
M#CANLK  EQU   24                  CANADIAN SPOT DESKTOP LOOKUP                 
O#CANLK  EQU   X'11'                                                            
         DC    AL2(M#CANLK),AL1(O#CANLK)                                        
         DC    AL1(LP_RNOWQ+LP_RNWKQ+LP_RCONQ+LP_RDSCQ,0)                       
         DC    AL1(08),AL2(RE#COMDL)                                            
                                                                                
***      LKMMI D,I#DECAND,M#CANLK,M#CANLK,RUNNER=Y                              
                                                                                
DDNDXX   DC    AL2(0)                                                           
                                                                                
         PRINT OFF                                                              
       ++INCLUDE DELNKWRK                                                       
       ++INCLUDE GEDDEQUS                                                       
       ++INCLUDE REDDEQUS                                                       
         PRINT ON                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010DELNK00   05/12/10'                                      
         END                                                                    
