*          DATA SET TAGENBD    AT LEVEL 021 AS OF 03/20/15                      
*PHASE T702BDC,*                                                                
         TITLE 'T702BD - SECURITY MAINTENANCE'                                  
T702BD   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T702BD                                                         
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
         SPACE 1                                                                
         GOTO1 INITIAL,DMCB,PFTAB                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VK                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    SEC10                                                            
*                                                                               
         CLI   MODE,XRECADD        IF NEW RECORD ADDED                          
         BE    SEC05                                                            
         CLI   MODE,XRECPUT        OR RECORD CHANGED                            
         BE    SEC05                                                            
         CLI   MODE,XRECDEL        OR RECORD DELETED                            
         BE    SEC05                                                            
         CLI   MODE,XRECREST       OR RECORD RESTORED                           
         BNE   SEC20                                                            
SEC05    DS    0H                                                               
*EC05    CLC   TGUSER,=H'7538'     IF USER-ID IS NOT TALFQA                     
*        BE    SEC10                                                            
*        CLC   TGUSER,=H'7697'     OR CLIFQA                                    
*        BE    SEC10                                                            
*                                                                               
         L     R3,TGAGRACT         R3=A($GEN RECACT TABLE)                      
         CLC   SECPROG,=C'REP'                                                  
         BNE   *+8                                                              
         L     R3,TGARRACT         R3=A($REP RECACT TABLE)                      
*                                                                               
         USING RACTD,R3                                                         
SEC07    GOTO1 PROTOFF             TURN OFF STORAGE PROTECTION                  
         MVI   RACTSTAT,RACTINIT   SET TO RE-INITIALIZE TABLE                   
*                                                                               
         LA    RF,PSREC                                                         
         USING PROGSPCD,RF                                                      
         XC    PSREC,PSREC                                                      
         MVC   PSSYS(4),=X'000A88E3'                                            
         DROP  RF                                                               
         GOTO1 PROTON              TURN ON STORAGE PROTECTION                   
*                                  TABLE REBUILT                                
         ICM   R0,7,=X'000A88'      TASYSTAB                                    
         ICM   R0,8,=C'S'                                                       
         GOTO1 CALLOV,DMCB,PSREC,(R0),(C'D',0)                                  
         CLC   PSREC(4),=X'000A88E3'   MAKE SURE IT'S THE SAME                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    RF,PSREC                                                         
         ICM   RF,8,=C'W'                                                       
         GOTO1 CALLOV,DMCB,TASYSTBL,(RF),0     TGTABLES=A(TASYSTAB)             
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
SEC10    BAS   RE,DISPLAY          (RE-)DISPLAY THE RECORD                      
         B     SECX                                                             
*                                                                               
SEC20    CLI   MODE,VALREC         VALIDATE THE RECORD                          
         BNE   SECX                                                             
         BAS   RE,BLDREC                                                        
*                                                                               
SECX     B     XIT                                                              
PSREC    DS    XL(PROGSPCL)                                                     
*                                                                               
         EJECT                                                                  
*                                                                               
*              DISPLAY THE KEY                                                  
*                                                                               
         SPACE 2                                                                
DK       L     R4,AIO              ADDRESS OF RECORD                            
         USING TLSED,R4                                                         
         MVC   SECPROG,=C'GEN'                                                  
         CLI   TLSEPROG,2          PROGRAM NUMBER                               
         BE    DK10                                                             
         MVC   SECPROG,=C'REP'                                                  
*                                                                               
DK10     MVC   SECREC,=C'ALL     ' DEFAULT                                      
         OC    TLSEREC,TLSEREC                                                  
         BZ    *+10                                                             
         MVC   SECREC,TLSEREC      RECORD                                       
*                                                                               
         MVC   SECACT,=C'ALL     ' DEFAULT                                      
         OC    TLSEACT,TLSEACT                                                  
         BZ    *+10                                                             
         MVC   SECACT,TLSEACT      ACTION                                       
*                                                                               
         OI    SECPROGH+6,X'80'    TRANSMIT                                     
         OI    SECRECH+6,X'80'                                                  
         OI    SECACTH+6,X'80'                                                  
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*              VALIDATE THE KEY                                                 
*                                                                               
VK       LA    R4,KEY                                                           
         USING TLSED,R4                                                         
         XC    KEY,KEY                                                          
         MVI   TLSECD,TLSECDQ      SET RECORD CODE                              
*                                                                               
         LA    R2,SECPROGH         SET PROGRAM NUMBER                           
         CLI   5(R2),0                                                          
         BE    MISSERR                                                          
         MVI   TLSEPROG,02                                                      
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),=C'GEN'                                                  
         BNE   VK05                                                             
         MVC   8(3,R2),=C'GEN'                                                  
         OI    6(R2),X'80'                                                      
         L     R3,TGAGRACT         A(RECACT TABLE)                              
         USING RACTD,R3                                                         
         B     VK10                                                             
*                                                                               
VK05     MVI   TLSEPROG,03                                                      
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),=C'REP'                                                  
         BNE   INVERR                                                           
         MVC   8(3,R2),=C'REP'                                                  
         OI    6(R2),X'80'                                                      
         L     R3,TGARRACT         A(RECACT TABLE)                              
         USING RACTD,R3                                                         
*                                                                               
VK10     LA    R3,RACTTBL                                                       
         LA    R2,SECRECH          RECORD NAME (OR BINARY 0'S)                  
         CLI   5(R2),0                                                          
         BE    MISSERR                                                          
         CLC   =C'ALL',8(R2)                                                    
         BE    VK20                                                             
         MVI   WORK,1                                                           
         ZIC   R1,5(R2)                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK+1(0),8(R2)     FIRST LOOK UP RECORD TYPE                    
         BAS   RE,LOOKUP                                                        
         BE    INVERR              ERROR IF NOT FOUND                           
         MVC   BYTE,9(R3)          SAVE RECORD NUMBER                           
         MVC   TLSEREC,1(R3)                                                    
         MVC   8(8,R2),1(R3)                                                    
         OI    6(R2),X'80'         RETRANSMIT                                   
*                                                                               
VK20     MVI   WORK,2              NOW LOOK UP ACTION                           
         LA    R2,SECACTH                                                       
         CLI   5(R2),0                                                          
         BE    MISSERR                                                          
         CLC   =C'ALL',SECREC      IF RECORD = ALL, ACTION CANNOT               
         BE    VK30                                                             
         CLC   =C'ALL',8(R2)       IF ACTION = ALL, DON'T BOTHER                
         BE    VKX                 CHECKING RECORD/ACTION VALIDITY              
*                                                                               
VK30     BAS   RE,VALACT           CERTAIN ACTIONS ARE INVALID                  
         ZIC   R1,5(R2)                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK+1(0),8(R2)      FIRST LOOK UP RECORD TYPE                   
*                                                                               
         BAS   RE,LOOKUP                                                        
         BE    INVERR              ERROR IF NOT FOUND                           
         MVC   TLSEACT,1(R3)                                                    
         MVC   8(8,R2),1(R3)                                                    
         OI    6(R2),X'80'         RETRANSMIT                                   
*                                                                               
         CLC   =C'ALL',SECREC      IF RECORD = ALL, DON'T BOTHER                
         BE    VKX                 CHECKING RECORD/ACTION VALIDITY              
         MVI   WORK,3                                                           
         MVC   WORK+1(1),BYTE      NOW LOOK UP RECORD/ACTION COMB.              
         MVC   WORK+2(1),10(R3)                                                 
         LA    R1,3-1                                                           
         BAS   RE,LOOKUP                                                        
         BE    INVERR              GET OUT IF NOT FOUND                         
*                                                                               
VKX      B     XIT                                                              
*                                                                               
         SPACE 3                                                                
LOOKUP   DS    0H                  LOOK UP ROUTINE FOR RECACT TABLE             
LOOK10   CLI   0(R3),X'FF'         END OF TABLE                                 
         BER   RE                  RETURN CC EQ                                 
*                                                                               
LOOK20   EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   WORK(0),0(R3)       MATCH ON SEARCH ARG. IN WORK                 
         BE    LOOK30                                                           
         CLI   WORK,1              IF LOOKING UP RECORDS                        
         BNE   *+12                                                             
         MVI   WORK,4              TRY FOR PROGREC                              
         B     LOOK20                                                           
*                                                                               
LOOK25   CLI   WORK,4              IF JUST CHECK PROGREC                        
         BNE   *+8                                                              
         MVI   WORK,1              RESTORE RECORD TYPE                          
         ZIC   R0,LRECACT                                                       
         AR    R3,R0               NO MATCH - TRY NEXT ENTRY IN TABLE           
         B     LOOK10                                                           
*                                                                               
LOOK30   CLC   12(4,R3),=4X'00'    IF ACCESS DEFINED AT THIS LEVEL              
         BE    LOOKX                                                            
         MVC   FULL,12(R3)         CURRENT ACCESS                               
         NC    FULL,SECMASKS       'AND' WITH SECURITY MASK                     
         BZ    LOOK25                                                           
*                                                                               
LOOKX    LTR   RE,RE               RETURN CC NE                                 
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
*        VALIDATE CERTAIN ACTIONS                                               
*                                                                               
VALACT   NTR1                                                                   
         CLC   =C'ALL',SECREC      ONLY CARE IF RECORD TYPE DEFINED             
         BE    VAX                                                              
         LA    R3,ACTTAB           TABLE OF INVALID ACTIONS                     
         CLI   5(R2),1             IF INPUT JUST A C'D'                         
         BNE   VA05                                                             
         CLI   8(R2),C'D'          THEN IT'S DISPLAY NOT DELETE                 
         BE    VAX                                                              
*                                                                               
VA05     ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
*                                                                               
VA10     CLI   0(R3),X'FF'         END OF TABLE                                 
         BE    VA30                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R3),8(R2)                                                    
         BNE   VA20                                                             
         CLC   PAYACT,0(R3)        IF ACTION IS PAY                             
         BNE   *+14                                                             
         CLC   =C'GRT',SECREC      ALLOW FOR GRT                                
         BE    VA30                                                             
         MVC   8(8,R2),0(R3)                                                    
         OI    6(R2),X'80'         RETRANSMIT                                   
         B     INVERR                                                           
*                                                                               
VA20     LA    R3,L'ACTTAB(R3)     BUMP TO NEXT ACTION                          
         B     VA10                                                             
*                                                                               
VA30     DS    0H                                                               
*                                                                               
VAX      B     XIT                                                              
         EJECT                                                                  
*              DISPLAY THE RECORD                                               
*                                                                               
DISPLAY  NTR1                                                                   
         TWAXC SECCATH                                                          
         L     R4,AIO                                                           
         MVI   ELCODE,TASEELQ                                                   
         USING TASED,R4                                                         
         BAS   RE,GETEL                                                         
         BNE   DSX                                                              
         BAS   RE,DISCAT           DISPLAY SECURITY CODES                       
*                                                                               
DSX      GOTO1 ACTVOUT,DMCB,SECLCHGH               LAST CHANGED                 
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*              DISPLAY SECURITY MASK                                            
*                                                                               
DISCAT   NTR1                                                                   
         LA    R2,SECCAT                                                        
         LA    R3,TASEMASK                                                      
         OC    TASEMASK,TASEMASK   IF BINARY 0'S                                
         BNZ   DCAT10                                                           
         MVC   0(3,R2),=C'ALL'     THEN ALL CATS ARE VALID                      
         B     DCATX                                                            
*                                                                               
DCAT10   L     R4,TGASTAFS         LOOP THROUGH STAFF TYPE TABLE                
         USING STAFTABD,R4                                                      
*                                                                               
DCAT20   ZIC   R1,STAFDSP          DISPLACEMENT TO MASK BYTE                    
         LA    RF,0(R3,R1)                                                      
         IC    R1,STAFBIT          BIT VALUE                                    
         EX    R1,*+8                                                           
         B     *+8                                                              
         TM    0(RF),0             IF BIT IS ON                                 
         BZ    DCAT30                                                           
         MVC   0(1,R2),STAFEQU     DISPLAY EQUATE CHARACTER                     
         MVI   1(R2),C','                                                       
         LA    R2,2(R2)                                                         
*                                                                               
DCAT30   LA    R4,STAFNEXT                                                      
         CLI   0(R4),X'FF'         IF WE HAVEN'T REACHED END                    
         BNE   DCAT20              CONTINUE                                     
*                                                                               
         BCTR  R2,0                                                             
         CLI   0(R2),C','          CHECK IF WE NEED TO CLEAR                    
         BNE   *+8                                                              
         MVI   0(R2),C' '          TRAILING COMMA                               
*                                                                               
DCATX    B     XIT                                                              
         EJECT                                                                  
*                                                                               
*              BUILD THE RECORD                                                 
*                                                                               
BLDREC   NTR1                                                                   
         MVI   ELCODE,TASEELQ                                                   
         GOTO1 REMELEM                                                          
*                                                                               
         LA    R4,ELEMENT          ADD SECURITY ELEMENT                         
         XC    ELEMENT,ELEMENT                                                  
         USING TASED,R4                                                         
         MVI   TASEEL,TASEELQ                                                   
         MVI   TASELEN,TASELNQ                                                  
         LA    R2,SECCATH                                                       
         CLI   5(R2),0                                                          
         BE    BLD10                                                            
         BAS   RE,VALCAT           VALIDATE SECURITY                            
*                                                                               
BLD10    GOTO1 ADDELEM                                                          
         GOTO1 ACTVIN,DMCB,0       LAST CHANGED                                 
*                                                                               
BLDX     B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        VALIDATE SECURITY CODES                                                
*                                                                               
VALCAT   NTR1                                                                   
         LA    R3,8(R2)                                                         
         ZIC   R0,5(R2)                                                         
         CH    R0,=H'3'                                                         
         BNE   VC10                                                             
         CLC   =C'ALL',8(R2)       ALLOW INPUT = ALL                            
         BE    VCX                                                              
*                                                                               
VC10     CLI   0(R3),C','          IF THIS IS A COMMA - IGNORE IT               
         BE    VC30                                                             
         GOTO1 STAFVAL,DMCB,0(R3),0                                             
         BE    VC20                                                             
         SR    R3,R2                                                            
         SH    R3,=H'8'                                                         
         B     ERRINV                                                           
*                                                                               
VC20     ZIC   R1,TGSTDSP          DISPLACEMENT TO SECURITY BYTE                
         LA    R5,TASEMASK(R1)     R5 = CORRECT BYTE                            
         OC    0(1,R5),TGSTBIT     OR IN BIT                                    
*                                                                               
VC30     LA    R3,1(R3)            BUMP R3 TO NEXT ENTRY                        
         BCT   R0,VC10             REPEAT FOR EACH ENTRY                        
*                                                                               
         OI    TASEMASK,BP         ALWAYS ALLOW PROGRAMMERS                     
*                                                                               
VCX      B     XIT                                                              
         EJECT                                                                  
*                                                                               
INVERR   MVI   ERROR,INVALID                                                    
         B     ERRXIT                                                           
*                                                                               
ERRINV   MVI   ERROR,INVALID                                                    
         STC   R3,ERRDISP                                                       
         B     ERRXIT                                                           
*                                                                               
MISSERR  MVI   ERROR,MISSING                                                    
         B     ERRXIT                                                           
*                                                                               
ERRXIT   XC    DMCB,DMCB                                                        
         GOTO1 EXIT,DMCB                                                        
         SPACE 2                                                                
YES      XR    RC,RC               SET CONDITION CODE                           
NO       LTR   RC,RC                                                            
*                                                                               
XIT      XIT1                                                                   
         SPACE 2                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 2                                                                
PFTAB    DS    0C                                                               
         DC    AL1(PF13X-*,13,0,(PF13X-PF13)/KEYLNQ,0)                          
         DC    CL3' ',CL8'SECURITY',CL8'LIST    '                               
PF13     DC    AL1(KEYTYTWA,L'SECPROG-1),AL2(SECPROG-T702FFD)                   
PF13X    EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
         SPACE 2                                                                
ACTTAB   DS    0CL8                ACTIONS NOT ALLOWED WITH RECORD TYPE         
         DC    CL8'DELETE'                                                      
         DC    CL8'RESTORE'                                                     
         DC    CL8'UNAPPR'                                                      
         DC    CL8'REOPEN'                                                      
PAYACT   DC    CL8'PAY'                                                         
         DC    X'FF'                                                            
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCRBDD                                                       
         EJECT                                                                  
         ORG   SECWORK                                                          
*                                                                               
         EJECT                                                                  
*                                                                               
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* TASYSEQUS                                                                     
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE FAPROGSPCD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'021TAGENBD   03/20/15'                                      
         END                                                                    
