*          DATA SET TAGENAB    AT LEVEL 006 AS OF 02/11/11                      
*PHASE T702ABA                                                                  
         TITLE 'T702AB - FILTER LIST MAINTENANCE'                               
T702AB   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T702AB                                                         
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES                                         
         SPACE 2                                                                
         GOTO1 INITIAL,DMCB,PFTBL  INITIALIZE                                   
         SPACE 3                                                                
         CLI   MODE,VALKEY         FIRST TIME IN                                
         BNE   LST10                                                            
         MVI   TGLTYP,TLGLTYPF     SET FILTER-TYPE LIST                         
         SPACE 1                                                                
         GOTO1 RECVAL,DMCB,TLGLCDQ,(X'40',SGLLSTH)  BUILD THE KEY               
         B     XIT                                                              
         SPACE 3                                                                
LST10    CLI   MODE,DISPREC        IF DISPLAY RECORD                            
         BE    LST15                                                            
         CLI   MODE,XRECADD        OR RECORD ADDED                              
         BE    LST14                                                            
         CLI   MODE,XRECDEL        OR DELETED                                   
         BE    LST14                                                            
         CLI   MODE,XRECREST       OR RESTORED                                  
         BE    LST14                                                            
         CLI   MODE,XRECPUT        OR CHANGED                                   
         BNE   LST20                                                            
LST14    GOTO1 ADDPTRS,DMCB,(X'02',PTRBLK)                                      
LST15    BAS   RE,DISPLAY          (RE-)DISPLAY THE RECORD                      
         B     XIT                                                              
         SPACE 3                                                                
LST20    CLI   MODE,VALREC         VALIDATE RECORD                              
         BNE   LST30                                                            
         GOTO1 SAVPTRS,DMCB,PTRBLK                                              
         BAS   RE,BLDREC           BUILD IT                                     
         B     XIT                                                              
         SPACE 3                                                                
LST30    CLI   MODE,DISPKEY        DISPLAY KEY FOR SELECT                       
         BNE   XIT                                                              
         L     R3,AIO                                                           
         USING TLGLD,R3                                                         
         MVC   SGLLST,TLGLLST      LIST CODE                                    
         OI    SGLLSTH+6,X'80'     TRANSMIT                                     
         B     XIT                                                              
         EJECT                                                                  
*              DISPLAY THE RECORD                                               
         SPACE 1                                                                
DISPLAY  NTR1                                                                   
         TWAXC SGLNAMEH            CLEAR THE SCREEN                             
         SPACE 1                                                                
         GOTO1 CHAROUT,DMCB,TANAELQ,SGLNAMEH  LIST NAME                         
         SPACE 1                                                                
         XR    R2,R2               R2=N'ENTRIES IN BLOCK                        
         L     R3,AIO2             R3=A(BLOCK)                                  
         SPACE 1                                                                
         L     R4,AIO                                                           
         MVI   ELCODE,TAGLELQ      SET TO GET GENERAL LIST ELEMENTS             
         BAS   RE,GETEL                                                         
         B     *+8                                                              
DISP10   BAS   RE,NEXTEL                                                        
         BNE   DISP20                                                           
         USING TAGLD,R4                                                         
         MVC   0(30,R3),SPACES     PRE-CLEAR THIS ENTRY                         
         SPACE 1                                                                
         ZIC   R1,TAGLLEN          SET FOR EXECUTED MOVE                        
         SH    R1,=AL2(TAGLLNQ+1)                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),TAGLDATA    MOVE DATA TO BLOCK                           
         SPACE 1                                                                
         AH    R2,=H'1'            BUMP COUNT                                   
         LA    R3,30(R3)           BUMP TO NEXT ENTRY IN BLOCK                  
         CH    R2,=AL2(4000/30)    IF STILL HAVE ROOM                           
         BL    DISP10              LOOK FOR MORE ELEMENTS                       
         SPACE 1                                                                
DISP20   MVC   DMCB,AIO2           SET A(BLOCK)                                 
         STC   R2,DMCB             SET N'ENTRIES TO PRINT                       
         LA    R2,SGLDATAH         R2=A(FIRST DATA FIELD)                       
         SPACE 1                                                                
DISP30   CLI   DMCB,0              TEST ANYTHING (LEFT) TO PRINT                
         BE    DISPX                                                            
         GOTO1 UNSCAN,DMCB,,(R2),0,(20,C' $LT')                                 
         ZIC   R1,0(R2)            BUMP TO NEXT FIELD                           
         AR    R2,R1                                                            
         LA    RF,SGLDLSTH         INSURE NOT PAST LAST FIELD                   
         CR    R2,RF                                                            
         BNH   DISP30                                                           
         SPACE 1                                                                
DISPX    GOTO1 ACTVOUT,DMCB,SGLLCHGH  LAST CHANGED                              
         B     XIT                                                              
         EJECT                                                                  
*              BUILD THE RECORD                                                 
         SPACE 1                                                                
BLDREC   NTR1                                                                   
         GOTO1 NAMIN,DMCB,TANAELQ,(X'80',SGLNAMEH)  LIST NAME                   
         SPACE 1                                                                
         MVI   ELCODE,TAGLELQ      DELETE EXISTING LIST ELEMENTS                
         GOTO1 REMELEM                                                          
         SPACE 1                                                                
         LA    R2,SGLDATAH         R2=A(FIRST DATA FIELD)                       
         GOTO1 ANY                                                              
BLDR10   GOTO1 SCANNER,DMCB,(R2),(X'80',AIO2)                                   
         MVI   ERRDISP,0                                                        
         CLI   4(R1),0                                                          
         BE    FLDINV                                                           
         ZIC   R0,4(R1)            R0=N'ENTRIES IN BLOCK                        
         L     R3,AIO2             R3=A(BLOCK)                                  
         USING SCAND,R3                                                         
         SPACE 1                                                                
BLDR20   MVC   ERRDISP,SCDISP1     SET DISP. TO THIS DATA                       
         SPACE 1                                                                
         CLI   SCLEN1,20           MAX SIZE OF LHS                              
         BH    FLDINV                                                           
         CLI   SCLEN1,1            MIN SIZE OF LHS                              
         BL    FLDINV                                                           
         CLI   SCLEN2,0            SHOULD BE NO RHS                             
         BNE   FLDINV                                                           
         SPACE 1                                                                
         XC    ELEMENT,ELEMENT     BUILD NEW ELEMENT                            
         LA    R4,ELEMENT                                                       
         USING TAGLD,R4                                                         
         MVI   TAGLEL,TAGLELQ      ELEMENT CODE                                 
         SPACE 1                                                                
         ZIC   R1,SCLEN1           SET FOR EXECUTED MOVE                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TAGLDATA(0),SCDATA1 MOVE DATA TO ELEMENT                         
         SPACE 1                                                                
         LA    R1,TAGLLNQ+1(R1)    NOW SET ELEMENT LENGTH                       
         STC   R1,TAGLLEN                                                       
         GOTO1 ADDELEM             AND ADD THE ELEMENT                          
         SPACE 1                                                                
         LA    R3,SCANNEXT         BUMP TO NEXT SCAN BLOCK ENTRY                
         BCT   R0,BLDR20                                                        
         SPACE 1                                                                
BLDR90   ZIC   R1,0(R2)            BUMP TO NEXT FIELD                           
         AR    R2,R1                                                            
         LA    RF,SGLDLSTH         HAVE WE REACHED EOS                          
         CR    R2,RF                                                            
         BH    *+16                                                             
         CLI   5(R2),0             SKIP EMPTY FIELDS                            
         BE    BLDR90                                                           
         B     BLDR10              SCAN THIS FIELD                              
         SPACE 1                                                                
         MVI   ERRDISP,0           CLEAR DISPLACEMENT INTO FIELD                
         GOTO1 ACTVIN,DMCB,0       LAST CHANGED                                 
         B     XIT                                                              
         EJECT                                                                  
*              EXITS, ETC.                                                      
         SPACE 3                                                                
         GETEL (R4),DATADISP,ELCODE                                             
         SPACE 3                                                                
XIT      XIT1                                                                   
         SPACE 1                                                                
FLDINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         SPACE 1                                                                
THEEND   GOTO1 EXIT,DMCB,0                                                      
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 2                                                                
PFTBL    DS    0C                  PF KEYS TABLE                                
         SPACE 1                                                                
         DC    AL1(PF13X-*,13,0,0,0)                                            
         DC    CL3' ',CL8'FLIST',CL8'LIST'                                      
PF13X    EQU   *                                                                
         DC    X'FF'                                                            
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCRABD                                                       
         EJECT                                                                  
*        LOCAL SAVED STORAGE AT END OF TWA                                      
PTRBLK   DS    CL((40*L'TLDRREC)+1)                                             
         SPACE                                                                  
BEG      EQU   (*-CONHEADH)                                                     
         DS    CL(3520-BEG)         CANNOT EXCEED DDGENTWA                      
         EJECT                                                                  
* DDGENTWA                                                                      
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* TASYSEQUS                                                                     
* TASYSDSECT                                                                    
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006TAGENAB   02/11/11'                                      
         END                                                                    
