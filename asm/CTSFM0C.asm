*          DATA SET CTSFM0C    AT LEVEL 007 AS OF 05/01/02                      
*PHASE TA0A0CA                                                                  
***********************************************************************         
*                                                                     *         
*  TITLE: TA0A0C - MAINTENANCE OF LOAD RECORDS                        *         
*                                                                     *         
*  COMMENTS: MAINTAINS LOAD RECORDS.                                  *         
*                                                                     *         
*  CALLED FROM: SFM CONTROLLER (TA0A00), WHICH CALLS                  *         
*               DDGENCON (T00A30) WHICH CALLS THIS.                   *         
*                                                                     *         
*  CALLS TO:    DATAMGR                                               *         
*                                                                     *         
*  INPUTS: SCREENS CTSFMFC (TA0AFB) -- MAINTENANCE                    *         
*                                                                     *         
*  OUTPUTS: UPDATED LOAD RECORDS                                      *         
*                                                                     *         
*  LOCALS: REGISTER USAGE                                             *         
*          R0 - WORK                                                  *         
*          R1 - WORK                                                  *         
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CURSOR    *         
*          R3 - WORK                                                  *         
*          R4 - WORK                                                  *         
*          R5 - WORK                                                  *         
*          R6 - USED FOR GETEL ELEMENT DSECT POINTER                  *         
*          R7 - WORK                                                  *         
*          R8 - SPOOLD                                                *         
*          R9 - SYSD                                                  *         
*          RA - TWA                                                   *         
*          RB - FIRST BASE                                            *         
*          RC - GEND                                                  *         
*          RD - SYSTEM                                                *         
*          RE - SYSTEM                                                *         
*          RF - SYSTEM                                                *         
*                                                                     *         
***********************************************************************         
         TITLE 'TA0A0C MAINTENANCE OF LOAD RECORDS'                             
TA0A0C   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*TA0A0C*,R7,RR=R3                                              
         L     RC,0(R1)            STANDARD CODING                              
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN + OUR SCREEN                     
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY?                                
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD?                             
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY?                                 
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD?                              
         BE    DR                                                               
*                                                                               
EXIT0C   XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE KEY                                                    *         
***********************************************************************         
*                                                                               
* VALIDATE THE SYSTEM                                                           
*                                                                               
VK       DS    0H                                                               
         LA    R2,SFMSYSTH         POINT TO SYSTEM FIELD FIRST                  
         CLI   5(R2),0             NO SYSTEM?                                   
         BE    MISSFLD             MISSING SYSTEM                               
*                                                                               
* VALIDATE THE PROGRAM                                                          
*                                                                               
VKPROG   LA    R2,SFMPROGH         POINT TO THE PROGRAM FIELD                   
         CLI   5(R2),0             ANYTHING IN PROGRAM FIELD?                   
         BE    MISSFLD             NO, MISSING PROGRAM                          
*                                                                               
* BUILD THE KEY FOR GENCON                                                      
*                                                                               
VKBKEY   XC    KEY,KEY             CLEAN OUT THE KEY                            
         LA    R4,KEY                                                           
         USING CTLDRECD,R4         OVERLAY KEY WITH OUR TEMPLATE                
         MVI   CTLDKID,CTLDKIDQ    LOAD UP THE IDENTIFIERS                      
         MVI   CTLDKSB,CTLDKSBQ                                                 
         MVC   CTLDKSY,SFMSYST                                                  
         MVC   CTLDKPR,SFMPROG                                                  
         B     EXIT0C              FINISHED VALIDATING KEY                      
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY THE KEY                                                     *         
***********************************************************************         
DK       DS    0H                                                               
         LA    R2,KEY              POINT TO THE KEY                             
         USING CTLDRECD,R2                                                      
         MVC   SFMSYST,CTLDKSY     SHOW THE SYSTEM                              
         MVC   SFMPROG,CTLDKPR              PROGRAM                             
         OI    SFMSYSTH+6,X'80'    TRANSMIT                                     
         OI    SFMPROGH+6,X'80'                                                 
         B     EXIT0C              DONE DISPLAYING KEY                          
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY THE RECORD                                                  *         
***********************************************************************         
DR       DS    0H                                                               
         XC    DATADISP,DATADISP   ZERO OUT THE DISPLACEMENT                    
         MVI   DATADISP+1,CTLDFRST-CTLDRECD LOAD UP PROPER DISPLACEMENT         
*                                                                               
* DISPLAY THE ELEMENT, KEY DISPLAYED ALREADY                                    
*                                                                               
DRELEM   L     R6,AIO              POINT TO THE RECORD                          
         MVI   ELCODE,CTLD05Q      ELEMENT CODE X'05'                           
         BAS   RE,GETEL            IS IT THE SYSTEM ELEMENT?                    
         BE    *+6                 YES                                          
         DC    H'0'                IMPOSSIBLE, MUST HAVE A HEX EQUIV.           
         USING CTLD05D,R6          ELEMENT TEMPLATE                             
         MVC   SFMESYS,CTLD05SY    SHOW THE LOADED PROGRAM SYSTEM               
         MVC   SFMEPRG,CTLD05PR                            PROGRAM              
         OI    SFMESYSH+6,X'80'    TRANSMIT NO MATTER WHAT                      
         OI    SFMEPRGH+6,X'80'    TRANSMIT NO MATTER WHAT                      
         B     EXIT0C              DONE DISPLAYING RECORD                       
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE RECORD ON SCREEN                                       *         
***********************************************************************         
VR       DS    0H                                                               
         LA    R2,SFMESYSH         POINT TO LOADED PROGRAM SYSTEM               
         CLC   SFMSYST,SFMESYS     CHECK IF BOTH SYSTEMS SAME                   
         BNE   BADSYSES            BAD SYSTEMS                                  
         LA    R2,SFMEPRGH         POINT TO LOADED PROGRAM PROGRAM              
         CLI   5(R2),0             NOTHING IN PROGRAM?                          
         BE    MISSFLD             YES, INVALID                                 
* BUILD THE RECORD                                                              
BLDREC   MVI   ELCODE,CTLD05Q      REMOVE ELEMENTS WITH THIS CODE               
         GOTO1 REMELEM                                                          
         XC    ELEM,ELEM                                                        
         LA    R3,ELEM             SET UP THE SYSTEM ELEMENT                    
         USING CTLD05D,R3                                                       
         MVI   CTLD05,CTLD05Q      LOAD UP THE ELEMENT CODE                     
         MVI   CTLD05LN,CTLD05LQ   LOAD UP THE LENGTH                           
         MVC   CTLD05SY,SFMESYS    COPY THE SYSTEM OVER                         
         MVC   CTLD05PR,SFMEPRG    COPY THE PROGRAM OVER                        
         GOTO1 ADDELEM             ADD ELEMENT TO RECORD                        
         CLI   DMCB+12,0           SUCCESSFUL OPERATION?                        
         BE    EXIT0C              YES                                          
         DC    H'0'                NO, DIE                                      
         DROP  R3                                                               
         EJECT                                                                  
RELO     DS    A                                                                
*                                                                               
* ERROR MESSAGES                                                                
*                                                                               
MYERROR  GOTO1 ERREX2                                                           
ERREXIT  GOTO1 ERREX                                                            
*                                                                               
MISSFLD  MVI   ERROR,MISSING                                                    
         B     ERREXIT                                                          
*                                                                               
BADSYSES XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'BADSMSG),BADSMSG                                       
         LA    R2,SFMESYSH                                                      
         B     MYERROR                                                          
BADSMSG  DC    C'ERROR: SYSTEMS ARE NOT THE SAME'                               
         SPACE 3                                                                
         GETEL R6,DATADISP,ELCODE  USED FOR THE GETEL OPERATIONS                
         EJECT                                                                  
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
       ++INCLUDE CTGENLOAD                                                      
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FALANG                                                         
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
       ++INCLUDE CTSFMFFD          (BASE SCREEN FOR SYSTEM)                     
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFMFCD          (OUR MAINTENANCE SCREEN OVERLAY)             
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE CTSFMWORKD        (SYSTEM AREAS)                               
         PRINT ON                                                               
* MY STORAGE AREA                                                               
         ORG   SYSSPARE                                                         
RECFOUND DS    CL1                 FLAG FOR CONDITION RECOUND FOUND             
         EJECT                                                                  
*                                                                               
* ONLINE LIST LINE                                                              
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LSTPHSE  DS    CL6                                                              
LSTLVEL  DS    CL1                                                              
         DS    CL1                                                              
LSTLANG  DS    CL3                                                              
         DS    CL2                                                              
LSTRDTE  DS    CL8                                                              
         DS    CL4                                                              
LSTDESC  DS    CL28                                                             
         DS    CL1                                                              
LSTNODE  DS    CL2                                                              
         DS    CL3                                                              
LSTCDOS  DS    CL4                                                              
         DS    CL1                                                              
LSTSPRE  DS    CL10                                                             
         SPACE 3                                                                
*                                                                               
* PRINT LINE                                                                    
*                                                                               
SPOOLD   DSECT                                                                  
         ORG   P                                                                
PRTPHSE  DS    CL6                                                              
PRTLVEL  DS    CL1                                                              
         DS    CL2                                                              
PRTLANG  DS    CL3                                                              
         DS    CL4                                                              
PRTRDTE  DS    CL8                                                              
         DS    CL4                                                              
PRTDESC  DS    CL64                                                             
         DS    CL3                                                              
PRTNODE  DS    CL2                                                              
         DS    CL4                                                              
PRTPORS  DS    CL1                                                              
         DS    CL3                                                              
PRTOPTN  DS    CL13                                                             
         DS    CL4                                                              
PRTSPRE  DS    CL10                                                             
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007CTSFM0C   05/01/02'                                      
         END                                                                    
