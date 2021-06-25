*          DATA SET REVIEW04   AT LEVEL 002 AS OF 08/31/00                      
*          DATA SET REVIEW04   AT LEVEL 001 AS OF 10/30/96                      
*PHASE T81704A                                                                  
REVIEW04 TITLE 'VIEWER GLOBAL DATA OBJECTS'                                     
REVIEW04 CSECT                                                                  
         PRINT NOGEN                                                            
         SPACE 1                                                                
***********************************************************************         
* BRANCH INDEX HELD IN HIGH ORDER BYTE OF RF                          *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
ROUT     NMOD1 RTWORKL,REVIEW04,R6,R7,RR=R3,CLEAR=YES                           
         USING RTWORKD,RC                                                       
         ST    R1,RTPARMA                                                       
         MVC   RTPARMS,0(R1)                                                    
         ST    R3,RTRELO                                                        
         USING TWAD,RA                                                          
         USING WORKD,R9                                                         
         L     R8,AGWORK                                                        
         USING GWORKD,R8                                                        
         LR    R2,RF                                                            
         GOTO1 VDICTAT,RTPARM,C'LU  ',DCLIST,RTLISTU                            
         GOTO1 (RF),(R1),C'LL  ',,RTLISTL                                       
*                                                                               
         B     OBJECT                                                           
*                                                                               
ROUTS    DS    0XL4                                                             
         B     OBJECT                                                           
         DC    12XL4'00'                                                        
*                                                                               
EXITL    MVI   RTBYTE1,0           SET CC LOW                                   
         B     EXITCC                                                           
EXITH    MVI   RTBYTE1,2           SET CC HIGH                                  
         B     EXITCC                                                           
EXITOK   MVI   RTBYTE1,1           SET CC EQUAL                                 
EXITCC   CLI   RTBYTE1,1                                                        
*                                                                               
EXIT     L     R1,RTPARMA          RETURN PARAMS TO CALLER                      
         MVC   0(L'RTPARMS,R1),RTPARMS                                          
         XIT1  ,                   EXIT WITH CC SET                             
         EJECT                                                                  
***********************************************************************         
* OBJECT - PROVIDES INTERFACE TO OBJECTS                              *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT                                             *         
* P2 HOLDS EQUATED VERB                                               *         
***********************************************************************         
         SPACE 1                                                                
OBJECT   LA    R1,RTPARMS                                                       
         TM    0(R1),GCBOVER       CAN WE OVERRIDE THIS CALL?                   
         BZ    OO02                                                             
         L     RF,AOLY                                                          
         TM    0(R1),GCBPS         FILTERING?                                   
         BZ    *+8                                                              
         L     RF,APSOLY           PREVIOUS SESSION OVERLAY REQUIRED            
         BASR  RE,RF                                                            
         BNH   EXIT                OVERRIDDEN AT A LOWER LEVEL                  
*                                                                               
OO02     LA    R5,TABLEOO          OBJECTS KNOWN AT THIS LEVEL                  
         USING OBJTABD,R5                                                       
         L     RE,0(R1)                                                         
*                                                                               
OO04     CLI   OBJVERB,EOT         E.O.T.                                       
         BE    EXITH               NOT KNOWN AT THIS LEVEL                      
         CLM   RE,1,OBJVERB        R1 HOLDS EQUATED VERB                        
         BE    OO06                MATCHED                                      
         LA    R5,OBJTABL(R5)                                                   
         B     OO04                ITERATE KNOWN OBJECTS                        
*                                                                               
OO06     ICM   RF,15,OBJADR                                                     
         A     RF,RTRELO                                                        
         BR    RF                  INVOKE OBJECT                                
         DROP  R5                                                               
*                                                                               
TABLEOO  DC    AL1(ODATA),AL1(0,0,0),AL4(DATA)                                  
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT                                                         *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT IDENTIFIER                                  *         
* P2 HOLDS EQUATED DATA IDENTIFIER OR ZERO                            *         
* P3 BYTE  0   HOLDS GLOBAL ACTION IF P2 IS ZERO                      *         
* P3 BYTES 1-3 HOLD EQUATED VERB                                      *         
* P4 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* P5 HOLDS A(FIELD TABLE ENTRY) OR ZERO IF P2 IS ZERO                 *         
***********************************************************************         
         SPACE 1                                                                
DATA     L     RE,4(R1)            RE HOLDS DATA IDENTIFIER                     
         LTR   RE,RE               NO GLOBAL OVERRIDES (SO FAR)                 
         BZ    EXITH                                                            
         LA    RF,KNOWTAB          TABLE OF KNOWN OBJECTS                       
         USING KNOWTABD,RF                                                      
*                                                                               
DATA02   CLC   KNOWID,=AL2(EOT)    E.O.T                                        
         BE    EXITH               NOT KNOWN AT THIS LEVEL                      
         CLM   RE,3,KNOWID         MATCH ON DATA TYPE                           
         BE    DATA04                                                           
         LA    RF,KNOWLQ(RF)       ITERATE THE TABLE                            
         B     DATA02                                                           
*                                                                               
DATA04   ICM   RF,15,KNOWADD       A(KNOWN OBJECT)                              
         A     RF,RTRELO           RELOCATE IT                                  
         BASR  RE,RF                                                            
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* TABLE OF KNOWN RECORD OBJECTS                                       *         
***********************************************************************         
         SPACE 1                                                                
KNOWTAB  DC    AL2(EOT)                                                         
*                                                                               
KNOWTABD DSECT                                                                  
KNOWID   DS    XL2                 IDENTIFIER                                   
KNOWADD  DS    AL4                 A(OBJECT)                                    
KNOWLQ   EQU   *-KNOWTABD                                                       
*                                                                               
REVIEW04 CSECT                                                                  
         LTORG                                                                  
DCLIST   DS    0D                                                               
DCLISTX  DC    X'00'                                                            
         DROP  RB,R6,R7                                                         
         EJECT                                                                  
***********************************************************************         
* MACRO BRANCH TO DATA OBJECT                                         *         
***********************************************************************         
         SPACE 1                                                                
         MACRO                                                                  
&NTRDO   NTRDO  &VBTAB,&ID,&LB                                                  
         AIF   (T'&ID NE 'O').L1                                                
         DC    CL8' '                                                           
         AGO  .L2                                                               
.L1      ANOP                                                                   
         DC    CL8'&ID'                                                         
.L2      ANOP                                                                   
         DS    0H                                                               
         USING *,RB                                                             
&NTRDO   NTR1  BASE=(RF)                                                        
         LM    R1,R3,8(R1)         R1 HOLDS VERB                                
         USING FDRELD,R3           R3 HOLDS A(FIELD TABLE ENTRY)                
         LA    R5,&VBTAB           TABLE OF KNOWN VERBS                         
         USING OBJTABD,R5                                                       
*                                                                               
&LB.03   CLI   OBJVERB,EOT         E.O.T.                                       
         BE    &LB.H                                                            
         CLM   R1,1,OBJVERB        R1 HOLDS EQUATED VERB                        
         BE    *+12                MATCHED                                      
         LA    R5,OBJTABL(R5)                                                   
         B     &LB.03              BUMP & LOOP                                  
*                                                                               
         ICM   RF,15,OBJADR        ROUTINE TO HANDLE THE VERB                   
         A     RF,RTRELO                                                        
         BR    RF                                                               
*                                                                               
&LB.XL   MVI   RTPARMS,DFLTL       EXIT LOW FOR FILTER                          
         B     &LB.E                                                            
&LB.XE   MVI   RTPARMS,DFLTE       EXIT EQUAL FOR FILTER                        
         B     &LB.E                                                            
&LB.XH   MVI   RTPARMS,DFLTH       EXIT HIGH FOR FILTER                         
         B     &LB.E                                                            
&LB.XX   MVI   RTPARMS,DFLTX       EXIT DEFINATELY NOT VALID                    
         B     &LB.E                                                            
*                                                                               
&LB.L    MVI   RTBYTE1,0           SET CC LOW                                   
         B     &LB.CC                                                           
&LB.H    MVI   RTBYTE1,2           SET CC HIGH                                  
         B     *+8                                                              
&LB.E    MVI   RTBYTE1,1           SET CC EQUAL                                 
&LB.CC   CLI   RTBYTE1,1                                                        
*                                                                               
*                                                                               
&LB.X    XIT1                                                                   
         MEND                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE                                                     *         
***********************************************************************         
         SPACE 1                                                                
RTWORKD  DSECT                                                                  
RTRELO   DS    A                                                                
RTPARMA  DS    A                   A(INCOMING PARAMETER LIST)                   
RTPARMO  DS    F                                                                
RTPARMS  DS    0XL24               SAVED PARAMETERS                             
RTPARMS1 DS    A                                                                
RTPARMS2 DS    A                                                                
RTPARMS3 DS    A                                                                
RTPARMS4 DS    A                                                                
RTPARMS5 DS    A                                                                
RTPARMS6 DS    A                                                                
RTPARM   DS    XL24                * PARAMETERS 1-6 *                           
RTBYTE1  DS    X                                                                
RTWORK   DS    XL80                                                             
*                                                                               
RTLISTU  DS    0D                  DICTIONARY EQUATES USED                      
*                                                                               
RTLISTL  DS    0D                  DICTIONARY EQUATES USED                      
RTWORKL  EQU   *-RTWORKD                                                        
*                                                                               
REVIEW04 CSECT                                                                  
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
FF       EQU   X'FF'                                                            
         EJECT                                                                  
* REVIEWWRK                                                                     
         PRINT OFF                                                              
       ++INCLUDE REVIEWWRK                                                      
         PRINT ON                                                               
* DDSCANBLKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002REVIEW04  08/31/00'                                      
         END                                                                    
