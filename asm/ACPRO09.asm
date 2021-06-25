*          DATA SET ACPRO09    AT LEVEL 022 AS OF 02/01/08                      
*PHASE T60B09A                                                                  
         TITLE 'T60B09 - COMMENT MAINT'                                         
T60B09   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T60B09**,RA                                                    
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9                                                       
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7                                                   
         SPACE 1                                                                
         CLI   MODE,VALKEY                                                      
         BNE   MODE2                                                            
         BAS   RE,VKEY                                                          
         B     XIT                                                              
         SPACE 1                                                                
MODE2    CLI   MODE,VALREC                                                      
         BNE   MODE4                                                            
         BAS   RE,VREC                                                          
         BAS   RE,DKEY                                                          
         BAS   RE,DREC                                                          
         B     XIT                                                              
         SPACE 1                                                                
MODE4    CLI   MODE,DISPKEY                                                     
         BNE   MODE6                                                            
         BAS   RE,DKEY                                                          
         B     XIT                                                              
         SPACE 1                                                                
MODE6    CLI   MODE,DISPREC                                                     
         BNE   XIT                                                              
         BAS   RE,DKEY                                                          
         BAS   RE,DREC                                                          
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE KEY FIELDS                                              
         SPACE 3                                                                
VKEY     NTR1                      COMMENT RECORD                               
         LA    R2,PROCOMH                                                       
         GOTO1 ANY                                                              
         XC    KEY,KEY                                                          
         MVI   KEY,SCMKTYPQ                                                     
         MVC   KEY+1(1),CUL                                                     
         MVC   KEY+2(6),SPACES                                                  
         LA    R3,KEY+2+6          CODES ARE RIGHT ALIGNED                      
         ZIC   R1,5(R2)                                                         
         SR    R3,R1                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),8(R2)                                                    
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE RECORD                                                  
         SPACE 3                                                                
VREC     NTR1                                                                   
         GOTO1 PERSIN                                                           
         LA    R2,PRODESCH                                                      
         XC    ELEMENT,ELEMENT                                                  
         MVI   ELCODE,COIELQ       CHECK IF EXISTING ELEMENT                    
         GOTO1 REMELEM                                                          
         LA    R6,ELEMENT                                                       
         USING COIELD,R6                                                        
         MVI   COIEL,COIELQ                                                     
         MVI   COILN,COILNQ                                                     
         MVC   COIDESC,SPACES                                                   
         MVC   COIFILT,SPACES                                                   
         CLI   5(R2),0                                                          
         BE    VREC2                                                            
         GOTO1 ANY                                                              
         MVC   COIDESC,WORK                                                     
         SPACE 1                                                                
VREC2    LA    R2,PROFILTH                                                      
         CLI   5(R2),0                                                          
         BE    VREC3                                                            
         GOTO1 ANY                                                              
         MVC   COIFILT,WORK                                                     
         SPACE 1                                                                
VREC3    LA    R2,PROABOH                                                       
         L     R4,AIO                                                           
         USING SCMRECD,R4                                                       
         NI    SCMRSTAT,X'FF'-SCMKSABO                                          
         CLI   5(R2),0                                                          
         BE    VREC4                                                            
         MVI   ERROR,INVALID                                                    
         SR    RE,RE                                                            
         ICM   RE,1,5(R2)                                                       
         BCTR  RE,0                                                             
         EX    RE,COMPNO                                                        
         BE    VREC4                                                            
         EX    RE,COMPYES                                                       
         BNE   ERREND                                                           
         OI    SCMRSTAT,SCMKSABO                                                
                                                                                
VREC4    GOTO1 ADDELEM                                                          
         BAS   RE,FINDLOW          FIND THE LOWEST TEXT LINE                    
         MVI   ALLDEL,C'Y'         SET SWITCH FOR ALL DELETES                   
         MVI   ELCODE,SCMELQ       REMOVE COMMENT ELEMENTS                      
         GOTO1 REMELEM                                                          
         LA    R2,PROACTH                                                       
         LA    R3,1                (SEQUENCE NUMBER)                            
         ZIC   R0,LOWLIN           LOWEST LINE IS COUNTER                       
         XC    ELEMENT,ELEMENT                                                  
         LA    R6,ELEMENT                                                       
         USING SCMELD,R6                                                        
         MVI   SCMEL,SCMELQ                                                     
         SPACE 1                                                                
VREC6    MVC   THISACT,8(R2)       SAVE ACTION FIELD                            
         CLI   5(R2),0             TEST FOR INPUT IN ACTION                     
         BE    VREC8               NO                                           
         MVI   ERROR,INVALID                                                    
         CLI   THISACT,C'D'        TEST FOR DELETE                              
         BE    VREC7                                                            
         CLI   THISACT,C'I'        TEST FOR INSERT                              
         BNE   ERREND              NO-MUST BE AN ERROR                          
         CH    R0,=H'1'            TEST INSERT ON LAST LINE                     
         BE    ERREND              YES-RETURN AN ERROR                          
         B     VREC8                                                            
         SPACE 1                                                                
VREC7    CH    R0,=H'1'             TEST ON LAST LINE                           
         BNE   *+16                                                             
         MVI   ERROR,INVALID                                                    
         CLI   ALLDEL,C'Y'         TEST ALL DELETES                             
         BE    ERREND                                                           
         BAS   RE,BUMP             JUMP OVER DELETED LINE                       
         BAS   RE,BUMP                                                          
         BCT   R0,VREC6                                                         
         B     XIT                                                              
         SPACE 1                                                                
VREC8    BAS   RE,BUMP                                                          
         MVI   SCMLN,5             SPACE COMMENT IF NO DATA                     
         STC   R3,SCMSEQ                                                        
         MVI   SCMNARR,C' '                                                     
         CLI   5(R2),0                                                          
         BE    VREC9                                                            
*                                                                               
         MVI   ALLDEL,C'N'         NOT ALL DELETES                              
         ZIC   R1,5(R2)                                                         
         LA    R1,4(R1)                                                         
         STC   R1,SCMLN                                                         
         SH    R1,=H'5'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SCMNARR(0),8(R2)                                                 
         SPACE 1                                                                
VREC9    GOTO1 ADDELEM                                                          
         BAS   RE,BUMP                                                          
         LA    R3,1(R3)                                                         
         CLI   THISACT,C'I'        INSERT AFTER THIS LINE                       
         BNE   VREC10                                                           
         MVI   SCMLN,5             POP IN A SPACE ELEMENT                       
         STC   R3,SCMSEQ                                                        
         MVI   SCMNARR,C' '                                                     
         GOTO1 ADDELEM                                                          
         LA    R3,1(R3)                                                         
         BCT   R0,VREC6                                                         
         B     XIT                                                              
         SPACE 1                                                                
VREC10   BCT   R0,VREC6                                                         
         B     XIT                                                              
*                                                                               
COMPYES  CLC   8(0,R2),=C'YES'     OR YES, YE OR Y                              
COMPNO   CLC   8(0,R2),=C'NO'      ALLOW NO OR N                                
         SPACE 2                                                                
* SUB-ROUTINE TO FIND LOWEST SCREEN LINE WITH TEXT INPUT                        
*                                                                               
FINDLOW  ST    RE,FULL                                                          
         MVI   LOWLIN,0                                                         
         LA    R2,PROTEXTH                                                      
         LA    R3,1                R3=LINE NUMBER                               
         LA    R0,NLINES           R0=COUNTER                                   
*                                                                               
FINDLOW2 CLI   5(R2),0                                                          
         BE    *+8                                                              
         STC   R3,LOWLIN                                                        
         LA    R3,1(R3)            INCREMENT LINE NUMBER                        
         LA    R2,LINELEN(R2)                                                   
         BCT   R0,FINDLOW2                                                      
*                                                                               
FINDLOW4 CLI   LOWLIN,0            TEST FOR ANYTHING INPUT                      
         BNE   FINDLOWX                                                         
*                                                                               
         LA    R2,PROTEXTH                                                      
         MVI   ERROR,MISSING                                                    
         B     ERREND                                                           
*                                                                               
FINDLOWX L     RE,FULL                                                          
         BR    RE                                                               
         EJECT                                                                  
*              DISPLAY KEY                                                      
         SPACE 3                                                                
DKEY     NTR1                                                                   
         L     R4,AIO                                                           
         MVC   WORK,SPACES                                                      
         MVC   WORK(6),2(R4)                                                    
         SPACE 1                                                                
DKEY2    CLI   WORK,C' '           LEFT ALIGN COMMENT CODE IN WORK              
         BH    DKEY4                                                            
         MVC   WORK(6),WORK+1                                                   
         B     DKEY2                                                            
         SPACE 1                                                                
DKEY4    MVC   PROCOM,WORK                                                      
         OI    PROCOMH+6,X'80'                                                  
*                                                                               
         MVC   PROABO,SPACES                                                    
         OI    PROABOH+6,X'80'                                                  
         USING SCMRECD,R4                                                       
         TM    SCMRSTAT,SCMKSABO                                                
         BZ    *+10                                                             
         MVC   PROABO,=C'YES'                                                   
         B     XIT                                                              
         EJECT                                                                  
*              DISPLAY RECORD                                                   
         SPACE 3                                                                
DREC     NTR1                                                                   
         MVC   PROABO,SPACES                                                    
         OI    PROABOH+6,X'80'                                                  
         USING SCMRECD,R4                                                       
         L     R4,AIO                                                           
         TM    SCMRSTAT,SCMKSABO                                                
         BZ    *+10                                                             
         MVC   PROABO,=C'YES'                                                   
*                                                                               
         MVC   PROFILT,SPACES                                                   
         OI    PROFILTH+6,X'80'                                                 
         MVC   PRODESC,SPACES                                                   
         OI    PRODESCH+6,X'80'                                                 
         MVI   ELCODE,COIELQ                                                    
         BAS   RE,GETELIO                                                       
         BNE   DREC2                                                            
         USING COIELD,R6                                                        
         LA    R2,PRODESCH                                                      
         MVC   PRODESC,COIDESC                                                  
         LA    R2,PROFILTH                                                      
         MVC   PROFILT,COIFILT                                                  
         SPACE 1                                                                
DREC2    GOTO1 PERSOUT                                                          
         MVC   PROLACT,SPACES                                                   
         MVC   PROLACT(14),=C'LAST ACTIVITY:'                                   
         MVC   PROLACT+15(20),WORK+20                                           
         OI    PROLACTH+6,X'80'                                                 
         LA    R2,PROACTH                                                       
         LA    R0,NLINES                                                        
         MVI   ELCODE,SCMELQ                                                    
         BAS   RE,GETELIO                                                       
         B     DREC5                                                            
         SPACE 1                                                                
DREC4    BAS   RE,NEXTEL                                                        
         SPACE 1                                                                
DREC5    BNE   DREC6                                                            
         OI    6(R2),X'80'                                                      
         MVC   8(L'PROACT,R2),SPACES                                            
         BAS   RE,BUMP                                                          
         OI    6(R2),X'80'                                                      
         MVC   8(L'PROTEXT,R2),SPACES                                           
         USING SCMELD,R6                                                        
         ZIC   R1,SCMLN                                                         
         SH    R1,=H'5'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),SCMNARR                                                  
         BAS   RE,BUMP                                                          
         BCT   R0,DREC4                                                         
         B     XIT                                                              
         SPACE 1                                                                
DREC6    OI    6(R2),X'80'                                                      
         MVC   8(L'PROACT,R2),SPACES                                            
         BAS   RE,BUMP                                                          
         OI    6(R2),X'80'                                                      
         MVC   8(L'PROTEXT,R2),SPACES                                           
         BAS   RE,BUMP                                                          
         BCT   R0,DREC6                                                         
         B     XIT                                                              
         EJECT                                                                  
*              SUPPORTING SUBROUTINES                                           
         SPACE 3                                                                
BUMP     ZIC   RF,0(R2)            BUMP TO NEXT HEADER                          
         AR    R2,RF                                                            
         BR    RE                                                               
         SPACE 1                                                                
BUMPTOUN ZIC   RF,0(R2)            BUMP TO NEXT UNPROTECTED                     
         AR    R2,RF                                                            
         CLI   0(R2),0                                                          
         BER   RE                                                               
         TM    1(R2),X'20'                                                      
         BO    BUMPTOUN                                                         
         BR    RE                                                               
         SPACE 1                                                                
GETELIO  L     R6,AIO                                                           
         GETEL (R6),DATADISP,ELCODE                                             
         SPACE 1                                                                
MYCURSOR GOTO1 VERRCUR             I'M POSITIONING CURSOR                       
         SPACE 1                                                                
MYEND    MVI   ERROR,X'FE'         USING MY OWN ERROR MSG                       
         SPACE 1                                                                
ERREND   GOTO1 VERRCUR                                                          
         SPACE 1                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
* PATCH AREA                                                                    
*                                                                               
PATCH    DS    0H                                                               
         DC    XL32'00'                                                         
         SPACE 2                                                                
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*              DSECTS ARE HIDDEN IN HERE                                        
         SPACE 3                                                                
*ACPROWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACPROWORKD                                                     
         PRINT ON                                                               
*DDSPOOLD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
*DDSPLWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
*ACGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
*ACGENBOTH                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
         EJECT                                                                  
T60BFFD  DSECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE ACPROF9D                                                       
         SPACE 2                                                                
THISACT  DS    CL1                                                              
LOWLIN   DS    X                                                                
ALLDEL   DS    C                                                                
         SPACE 2                                                                
* EQUATES                                                                       
*                                                                               
LINELEN  EQU   PROACT2H-PROACTH                                                 
DISPTEXT EQU   PROTEXTH-PROACTH                                                 
NLINES   EQU   (PROENDH-PROACTH)/LINELEN                                        
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'022ACPRO09   02/01/08'                                      
         END                                                                    
