*          DATA SET ACPRO58    AT LEVEL 012 AS OF 08/10/00                      
*PHASE T60B58A                                                                  
         TITLE 'ADVERTISER MAINTAIN'                                            
T60B58   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 (MYDEND-MYD),T60B58**,R4,R5,RR=R2,CLEAR=YES                      
         LR    R7,RC                                                            
         USING MYD,R7              LOCAL W/S                                    
         ST    R2,RELO                                                          
         L     RC,0(R1)                                                         
         USING GEND,RC             GENERAL W/S                                  
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9                                                       
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         GOTO1 DICTATE,DMCB,C'LL  ',DDIN,DDOUT                                  
         SPACE 2                                                                
         CLI   MODE,VALKEY                                                      
         BNE   MODE2                                                            
         BAS   RE,VKEY                                                          
         BAS   RE,VPFKEY                                                        
         B     XIT                                                              
         SPACE 1                                                                
MODE2    CLI   MODE,VALREC                                                      
         BNE   XIT                                                              
         BAS   RE,VREC                                                          
         B     XIT                                                              
         EJECT                                                                  
*********************************************************************           
*              VALIDATE KEY FOR ADVERTISER RECORD                   *           
*********************************************************************           
         SPACE 1                                                                
VKEY     NTR1                                                                   
         BAS   RE,SPFKEY                                                        
*                                                                               
VKEY02   GOTO1 SETCOMP                                                          
         BAS   RE,VHED                                                          
         MVI   DISPLAY,C'Y'        GET RECORD FOR DISPLAY                       
         LA    R2,ADVMCODH                                                      
         LA    R3,KEY                                                           
         USING ADVRECD,R3                                                       
         XC    ADVKEY,ADVKEY                                                    
         MVI   ADVKTYP,ADVKTYPQ                                                 
         MVC   ADVKCPY(L'ADVKCPY),CUL                                           
         MVC   ADVKADV(L'ADVKADV),XADV                                          
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'ADVKEY),KEYSAVE                                            
         BE    VKEY04                                                           
*                                                                               
         MVI   DISPLAY,C'N'        NEW RECORD SO NOTHING TO DISPLAY             
         USING ACTRECD,R3          CHECK NEW CODE IS NOT A CLIENT CODE          
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCULA(L'CUL),CUL                                              
         XR    RF,RF                                                            
         IC    RF,LCLI                                                          
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   ACTKACT(0),XADV                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(L'ACTKEY),KEYSAVE                                            
         BNE   VKEY04                                                           
         MVI   ERROR,ADVISCLI                                                   
         B     ERREND                                                           
*                                                                               
VKEY04   MVI   INTMODE,EDTSCR      SET INTERNAL MODE TO EDIT SCREEN             
         CLI   RACHANGE,C'Y'       TEST RECORD/ACTION CHANGED                   
         BNE   *+8                                                              
         MVI   INTMODE,FSTSCR      YES-FORCE FIRST SCREEN                       
         CLI   KEYCHG,C'Y'         TEST KEY FIELDS HAVE CHANGED                 
         BNE   *+8                                                              
         MVI   INTMODE,FSTSCR      YES-FORCE FIRST SCREEN                       
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*********************************************************************           
*              VALIDATE ADVERTISER RECORD                           *           
*********************************************************************           
         SPACE 1                                                                
VREC     NTR1                                                                   
         CLI   INTMODE,FSTSCR                                                   
         BE    *+12                                                             
         BAS   RE,VPFKEY                                                        
         B     *+8                                                              
         BAS   RE,DREC                                                          
         BAS   RE,TSTEDT                                                        
         BNE   VREC02                                                           
         BAS   RE,EDT                                                           
         BAS   RE,DREC                                                          
         GOTO1 PERSIN                                                           
*                                                                               
VREC02   BAS   RE,SHEADER          SET HEADER LINE                              
         LA    R2,ADVMCODH         SET CURSOR TO CODE FIELD                     
         CLI   CALLSP,0                                                         
         BE    *+8                                                              
         LA    R2,ADVMNAMH         OR CODE NAME FIELD IF GOING TO LIST          
         OI    6(R2),X'40'                                                      
         B     XIT                                                              
         EJECT                                                                  
*********************************************************************           
*              VALIDATE HEADLINE FIELD                              *           
*********************************************************************           
         SPACE 1                                                                
VHED     NTR1                                                                   
         GOTO1 SETHEIR                                                          
         MVC   XADV,SPACES                                                      
         LA    R2,ADVMCODH                                                      
         BAS   RE,TSTKEY           TEST ALREADY VALIDATED                       
         GOTO1 ANY                 MUST HAVE INPUT                              
         MVI   ERROR,TOOLNG        CODE MUST BE NO LONGER THAN CLIENT           
         CLC   5(1,R2),LCLI                                                     
         BH    ERREND                                                           
         XR    RF,RF                                                            
         IC    RF,LCLI                                                          
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   XADV(0),8(R2)                                                    
         OI    6(R2),X'80'         TRANSMIT                                     
         OI    4(R2),X'20'         VALIDATED                                    
         CLI   CALLSP,0            ARE WE GOING BACK TO LIST?                   
         BE    XIT                                                              
         OI    1(R2),X'20'         YES - PROTECT THIS FIELD                     
         B     XIT                                                              
*                                                                               
TSTKEY   TM    4(R2),X'20'                                                      
         BOR   RE                                                               
         MVI   KEYCHG,C'Y'                                                      
         BR    RE                                                               
         EJECT                                                                  
*********************************************************************           
*              DISPLAY ADVERTISER RECORD                            *           
*********************************************************************           
         SPACE 1                                                                
DREC     NTR1                                                                   
         CLI   DISPLAY,C'Y'                                                     
         BE    DREC01                                                           
         LA    R2,ADVMNAMH                                                      
DREC00   NI    4(R2),X'DF'                                                      
         BAS   RE,BUMPTOUN                                                      
         CLI   0(R2),0                                                          
         BNE   DREC00                                                           
         B     DRECX                                                            
*                                                                               
DREC01   GOTO1 VCLEARF,DMCB,ADVMNAMH,ADVMPFAH                                   
         L     R6,AIO                                                           
         AH    R6,DATADISP                                                      
         USING NAMELD,R6                                                        
DREC02   CLI   NAMEL,NAMELQ                                                     
         BNE   DREC04                                                           
         LA    R2,ADVMNAMH                                                      
         XR    RF,RF                                                            
         IC    RF,NAMLN                                                         
         SH    RF,=Y(NAMLN1Q+1)                                                 
         EX    RF,*+4                                                           
         MVC   L'ADVMNAMH(0,R2),NAMEREC                                         
         OI    6(R2),X'80'                                                      
         OI    4(R2),X'20'                                                      
         B     DREC06                                                           
*                                                                               
         USING RSTELD,R6                                                        
DREC04   CLI   RSTEL,RSTELQ                                                     
         BNE   DREC06                                                           
         MVC   ADVMF1,RSTFILT1                                                  
         OI    ADVMF1H+4,X'20'                                                  
         MVC   ADVMF2,RSTFILT2                                                  
         OI    ADVMF2H+4,X'20'                                                  
         MVC   ADVMF3,RSTFILT3                                                  
         OI    ADVMF3H+4,X'20'                                                  
         MVC   ADVMF4,RSTFILT4                                                  
         OI    ADVMF4H+4,X'20'                                                  
         CLI   RSTLN,RSTLN2Q                                                    
         BNE   *+14                                                             
         MVC   ADVMF5,RSTFILT5                                                  
         OI    ADVMF5H+4,X'20'                                                  
         B     DREC06                                                           
*                                                                               
DREC06   XR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BNE   DREC02                                                           
*                                                                               
DRECX    B     XIT                                                              
         EJECT                                                                  
*********************************************************************           
*              TEST FOR SCREEN CHANGES                              *           
*********************************************************************           
         SPACE 1                                                                
TSTEDT   NTR1                                                                   
         CLI   DISPLAY,C'N'                                                     
         BE    TSTEDTY                                                          
         LA    R2,ADVMCODH                                                      
         SPACE 1                                                                
TSTEDT2  TM    4(R2),X'20'         TEST FOR CHANGE IN FIELD                     
         BZ    TSTEDTY             YES                                          
         BAS   RE,BUMPTOUN                                                      
         CLI   0(R2),0                                                          
         BNE   TSTEDT2             NO                                           
         SPACE 1                                                                
TSTEDTN  LTR   RB,RB               SET CC=NEQ                                   
         B     XIT                                                              
TSTEDTY  CR    RB,RB               SET CC=EQ                                    
         B     XIT                                                              
         EJECT                                                                  
*********************************************************************           
*              EDIT ROUTINE                                         *           
*********************************************************************           
         SPACE 1                                                                
EDT      NTR1                                                                   
         MVI   ADDSW,C'Y'          SET ADDING ADVERTISER RECORD                 
         MVI   UPDATE,C'N'         SET UPDATE NEEDED TO NO                      
         LA    R2,ADVMCODH                                                      
         LA    R4,KEY                                                           
         USING ADVRECD,R4                                                       
         XC    ADVKEY,ADVKEY                                                    
         MVI   ADVKTYP,ADVKTYPQ                                                 
         MVC   ADVKCPY(L'ADVKCPY),CUL                                           
         MVC   ADVKADV(L'ADVKADV),XADV                                          
*                                                                               
         OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 HIGH                                                             
         CLC   KEY(L'ADVKEY),KEYSAVE                                            
         BNE   EDT1                                                             
         MVI   ADDSW,C'N'          FOUND RECORD SO NO NEED TO ADD IT            
         TM    ACCOSTAT(R4),X'80'  TEST FOR DELETED RECORD                      
         BO    EDT1                YES - BUILD SKELETAL RECORD                  
         B     EDT2                                                             
*                                                                               
EDT1     L     R4,AIO              R4=A(IO AREA)                                
         LR    RE,R4                                                            
         L     RF,SIZEIO                                                        
         XR    R1,R1                                                            
         MVCL  RE,R0               CLEAR IO AREA                                
         MVC   ADVKEY,KEYSAVE      INITIALIZE KEY                               
         MVC   ACCORLEN(2,R4),=Y(ACCORFST+1) AND RECORD LENGTH                  
*                                                                               
EDT2     LA    R2,ADVMNAMH         NAME FIELD                                   
         TM    4(R2),X'20'         TEST FOR CHANGE                              
         BO    EDT4                                                             
*                                                                               
         MVI   UPDATE,C'Y'         YES - REPLACE ELEMENT                        
         MVI   ELCODE,NAMELQ                                                    
         GOTO1 REMELEM                                                          
         GOTO1 ANY                                                              
         XC    ELEMENT,ELEMENT                                                  
         LA    R6,ELEMENT                                                       
         USING NAMELD,R6                                                        
         MVI   NAMEL,NAMELQ                                                     
         XR    RF,RF                                                            
         IC    RF,5(R2)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   NAMEREC(0),8(R2)                                                 
         LA    RF,NAMLN1Q+1(RF)                                                 
         STC   RF,NAMLN                                                         
         GOTO1 ADDELEM                                                          
*                                                                               
EDT4     LA    R0,5                                                             
         LA    R2,ADVMF1H          FIRST FILTER FIELD                           
EDT6     TM    4(R2),X'20'         TEST FOR CHANGE                              
         BO    EDT8                                                             
*                                                                               
         MVI   UPDATE,C'Y'         YES - REPLACE ELEMENT                        
         MVI   ELCODE,RSTELQ                                                    
         GOTO1 REMELEM                                                          
         XC    ELEMENT,ELEMENT                                                  
         LA    R6,ELEMENT                                                       
         USING RSTELD,R6                                                        
         MVI   RSTEL,RSTELQ                                                     
         MVC   RSTFILT1,ADVMF1                                                  
         MVC   RSTFILT2,ADVMF2                                                  
         MVC   RSTFILT3,ADVMF3                                                  
         MVC   RSTFILT4,ADVMF4                                                  
         MVC   RSTFILT5,ADVMF5                                                  
         MVI   RSTLN,RSTLN2Q                                                    
         GOTO1 ADDELEM                                                          
         B     EDT10                                                            
*                                                                               
EDT8     BAS   RE,BUMPTOUN         NEXT FILTER FIELD                            
         CLI   0(R2),0                                                          
         BE    EDT10                                                            
         BCT   R0,EDT6                                                          
*                                                                               
EDT10    CLI   UPDATE,C'N'                                                      
         BE    EDTX                                                             
*                                                                               
         CLI   ADDSW,C'Y'                                                       
         BNE   EDT12                                                            
         GOTO1 ADD                                                              
         MVI   DISPLAY,C'Y'                                                     
         B     EDTX                                                             
*                                                                               
EDT12    GOTO1 WRITE                                                            
*                                                                               
EDTX     B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*              SET PFKEY LINE                                         *         
***********************************************************************         
         SPACE 1                                                                
SPFKEY   NTR1                                                                   
         LA    RE,ADVMPFAH                                                      
         LA    RF,AS$PFRET                                                      
         CLI   CALLSP,0            ARE WE GOING BACK TO LIST?                   
         BNE   SPFK02              YES                                          
         CLI   ACTNUM,ACTNMNT                                                   
         BNE   SPFK04                                                           
         B     SPFK04              (NO PFKEYS DEFINED)                          
*                                                                               
SPFK02   XC    WORK,WORK                                                        
         GOTO1 GETTXT,WORK,(RF),('PFLMAX',(RE)),(C'S',0)                        
         B     SPFK06                                                           
*                                                                               
SPFK04   MVC   8(L'ADVMPFA,RE),SPACES                                           
*                                                                               
SPFK06   OI    ADVMPFAH+6,X'80'                                                 
         B     XIT                                                              
         SPACE 1                                                                
***********************************************************************         
*              SET HEADER LINE                                        *         
***********************************************************************         
         SPACE 1                                                                
SHEADER  NTR1                                                                   
         CLI   CALLSP,0            ARE WE GOING BACK TO LIST?                   
         BE    SHEAD02             NO                                           
         LA    RF,AI$RDECH         'RECORD DISPLAYED - ENTER CHANGES'           
         CLI   INTMODE,FSTSCR                                                   
         BE    SHEAD04                                                          
         LA    RF,AI$ACTOK         'ACTION COMPLETE'                            
         B     SHEAD04                                                          
SHEAD02  CLI   INTMODE,FSTSCR                                                   
         BE    XIT                                                              
         LA    RF,AI$RECCH         'RECORD CHANGED - ENTER NEXT'                
         CLI   ADDSW,C'Y'                                                       
         BNE   SHEAD04                                                          
         LA    RF,AI$RECAD         'NEW RECORD ADDED - ENTER NEXT'              
SHEAD04  XR    RE,RE                                                            
         ICM   RE,4,GETMSYS                                                     
         XC    WORK,WORK                                                        
         GOTO1 GETTXT,WORK,(RF),(0,CONHEADH),(C'I',0),0,0,(RE)                  
         OI    CONHEADH+6,X'80'                                                 
         B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
*              VALIDATE PFKEYS                                       *          
**********************************************************************          
         SPACE 1                                                                
VPFKEY   NTR1                                                                   
         LA    R2,ADVMCODH                                                      
         CLI   ACTNUM,ACTNMNT                                                   
         BNE   XIT                                                              
         B     XIT                 (NO PFKEYS DEFINED)                          
         EJECT                                                                  
BUMP     ZIC   RF,0(R2)            BUMP TO NEXT HEADER                          
         AR    R2,RF                                                            
         BR    RE                                                               
*                                                                               
BUMPTOUN ZIC   RF,0(R2)            BUMP TO NEXT UNPROTECTED                     
         AR    R2,RF                                                            
         CLI   0(R2),0                                                          
         BER   RE                                                               
         TM    1(R2),X'20'                                                      
         BO    BUMPTOUN                                                         
         BR    RE                                                               
*                                                                               
MISSCODE MVI   ERROR,MISSING                                                    
         B     XIT                                                              
*                                                                               
ERREND   GOTO1 VERRCUR                                                          
*                                                                               
XIT      XIT1                                                                   
*                                                                               
DDIN     DS    0C                                                               
         DCDDL AC#YES,3,C                                                       
DDINX    DC    X'00'                                                            
         SPACE                                                                  
DDOUT    DS    0C                                                               
         DSDDL PRINT=YES                                                        
BLANKS   DC    CL80' '                                                          
         EJECT                                                                  
*********************************************************************           
*              LITERAL POOL                                         *           
*********************************************************************           
         SPACE                                                                  
         LTORG                                                                  
MYD      DSECT                                                                  
         DS    0F                                                               
RELO     DS    A                                                                
KEYCHG   DS    CL1                                                              
UPDATE   DS    CL1                                                              
ADDSW    DS    CL1                                                              
*                                                                               
       ++INCLUDE ACGOBLOCK                                                      
MYDEND   EQU   *                                                                
         EJECT                                                                  
*********************************************************************           
*              DSECTS ETC                                           *           
*********************************************************************           
         SPACE 1                                                                
         EJECT                                                                  
*********************************************************************           
*              INCLUDED DSECT BOOKS ETC                             *           
*********************************************************************           
         SPACE 1                                                                
* ACPROWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACPROWORKD                                                     
         PRINT ON                                                               
* DDSPOOLD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
* ACGENBOTH                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
* DDLANGEQUS                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDLANGEQUS                                                     
         PRINT ON                                                               
* DDPERVALD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDPERVALD                                                      
         PRINT ON                                                               
* FAGETTXTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
* ACMSGEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACMSGEQUS                                                      
         PRINT ON                                                               
T60BFFD  DSECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE ACPROACD                                                       
         ORG   ADVWORK                                                          
LASTSCHM DS    XL2                 LAST SCHEME RECORD DISPLAYED                 
MYLAST   DS    XL15                LAST JOB KEY                                 
XADV     DS    XL5                 CURRENT ADVERTISER CODE                      
INTMODE  DS    XL1                                                              
FSTSCR   EQU   1                                                                
EDTSCR   EQU   2                                                                
DISPLAY  DS    CL1                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012ACPRO58   08/10/00'                                      
         END                                                                    
