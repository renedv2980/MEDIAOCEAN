*          DATA SET PRSFM21    AT LEVEL 004 AS OF 09/29/10                      
*PHASE T41C21A                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* CHANGE LOG                                                                    
*                                                                               
* 07/26/10 SMYE ALLOW MEDIA O (OUT-OF-HOME)                                     
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
*  TITLE        T41C21 - STANDARD SPACE DESCRIPTION MAINT/LIST        *         
*                                                                     *         
*  CALLED FROM  GENCON VIA T41C00 (SFM CONTROLLER)                    *         
*                                                                     *         
*  COMMENTS     SUPPORTS MAINT, REPORT                                *         
*                                                                     *         
*  INPUTS       SCREEN T41CD6 (MAINTENANCE)                           *         
*                                                                     *         
*                                                                     *         
*  OUTPUTS      UPDATED STANDARD SPACE DESCRIPTION RECORDS            *         
*                           (**  PRINT DIRECTORY ONLY  **)            *         
*                                                                     *         
*  REGISTERS    R0 -- WORK                                            *         
*               R1 -- WORK                                            *         
*               R2 -- WORK                                            *         
*               R3 -- WORK                                            *         
*               R4 -- WORK                                            *         
*               R5 -- WORK                                            *         
*               R6 -- WORK                                            *         
*               R7 -- SECOND BASE                                     *         
*               R8 -- SPOOLD                                          *         
*               R9 -- SYSD                                            *         
*               RA -- TWA                                             *         
*               RB -- FIRST BASE                                      *         
*               RC -- GEND                                            *         
*               RD -- SYSTEM                                          *         
*               RE -- SYSTEM                                          *         
*               RF -- SYSTEM                                          *         
*                                                                     *         
*  I/O AREAS    IO1 -                                                 *         
*               IO2 -                                                 *         
*               IO3 -                                                 *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'T41C21 - SPACE DESCRIPTION MAINTENANCE/LIST'                    
T41C21   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T41C21,R7,RR=R3                                                
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         ST    R3,RELO                                                          
*                                                                               
INT      BAS   RE,INIT             INITIALIZE                                   
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CM       DS    0H                                                               
*                                                                               
CM50     CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VK                                                               
         CLI   MODE,PRINTREP       ONLINE REPORT                                
         BE    PR                                                               
*NOP*    CLI   MODE,VALREC         VALIDATE RECORD                              
*NOP*    BE    VR                                                               
         B     VR                                                               
*                                                                               
XIT      XIT1                                                                   
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         SPACE 3                                                                
INIT     NTR1                                                                   
         MVI   IOOPT,C'Y'          GENCON DOES NO WRITES                        
         MVI   ACTELOPT,C'N'       DON'T WRITE 'F1' ELEMENTS                    
         OI    GENSTAT4,NODELLST   NO DELETIONS FROM LIST                       
*                                                                               
         MVI   CONSERVH+6,X'81'    CHA TO MODIFIED FLD (GAIN CONTROL)           
*                                                                               
INITX    B     XIT                                                              
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*        VALIDATE KEY                                                           
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VK       DS    0H                                                               
*                                                                               
VK05     XC    WKKEY,WKKEY                                                      
         LA    R4,WKKEY            READ TO BUILD KEY                            
         USING PSPDKEY,R4                                                       
         MVC   PSPDAGY,AGENCY                                                   
         MVI   PSPDRCOD,PSPDRECQ                                                
*                                                                               
         MVI   ERROR,MISSING                                                    
         LA    R2,SFCMEDH          MEDIA                                        
         CLI   5(R2),0                                                          
         BE    TRAPERR             REQUIRED                                     
         GOTO1 VALIMED                                                          
*****    CLI   QMED,C'O'           OUT-OF-HOME ?                                
*****    BNE   VK10                NO - OK                                      
*****    MVI   ERROR,NGMEDO        "INVALID FOR SELECTED RECORD TYPE"           
*****    B     TRAPERR            (OUT-OF-HOME NOT VALID FOR STDSPACE)          
VK10     MVC   PSPDMED,QMED                                                     
*                                                                               
VK50     DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(4),WKKEY        THROUGH MED/RC                               
         MVC   AIO,AIO1            RESTORE AIO                                  
*                                                                               
*NOP*    MVI   ERROR,NOTFOUND                                                   
         CLI   SFCSTRTH+5,0                                                     
         BNE   *+10                                                             
         XC    SFCSTRT,SFCSTRT                                                  
         MVC   PSPDDESC,SFCSTRT    START VALUE                                  
         GOTO1 HIGH                                                             
*NOP*    CLC   KEY(4),KEYSAVE      THROUGH MEDIA/RC                             
*NOP*    BNE   TRAPERR             RECORD NOT FOUND                             
         CLC   CONACT(3),=C'REP'   REPORT ?                                     
         BNE   VR                  START WITH VALREC IF NOT REPORT              
*                                                                               
VKX      B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*        VALIDATE RECORD                                                        
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VR       DS    0H                                                               
*                                                                               
         CLC   CONACT(3),=C'REP'   REPORT ?                                     
         BE    XIT                                                              
*                                                                               
         MVI   VRSW,C' '           NO FIELD HAS BEEN MODIFIED YET               
*                                                                               
VR10     LA    R2,SFCNEWH          LIST OF SPACE DESC'S. TO BE ADDED            
         MVI   VALUES,X'FF'        INITIALIZE VALUES WITH X'FF'                 
         MVC   VALUES+1(L'VALUES-1),VALUES                                      
         LA    R3,VALUES                                                        
         USING VALUED,R3                                                        
         LA    R4,KEY                                                           
         USING PSPCDESD,R4                                                      
*                                                                               
VR20     CLI   5(R2),0                                                          
         BE    VR60                                                             
*                                                                               
         ST    R2,ACURFORC         IN CASE OF ERROR                             
*                                                                               
         MVI   ERROR,INVALID                                                    
         CLI   8(R2),C' '                                                       
         BNH   TRAPERR                                                          
*                                  ELIMINATE SOME "FIRST" CHARACTERS            
         CLI   8(R2),C'+'                                                       
         BE    TRAPERR                                                          
         CLI   8(R2),C'#'                                                       
         BE    TRAPERR                                                          
         CLI   8(R2),C'*'                                                       
         BE    TRAPERR                                                          
         CLI   8(R2),C'='                                                       
         BE    TRAPERR                                                          
*                                                                               
         XC    MYWORK,MYWORK       BUILD FAKE TWA FIELD                         
         MVI   MYWORK,8+18         HEADER + 18-BYTE FIELD                       
         ZIC   R1,5(R2)                                                         
         LA    RF,8(R2)                                                         
         CLI   QMED,C'N'           NEWSPAPERS ?                                 
         BNE   VR26                NO                                           
         LA    RE,8                MAX LENGTH NEWSPAPER DESC                    
         CLI   0(RF),C'-'                                                       
         BNE   *+8                                                              
         LA    RE,9                ALLOW 9 FOR DELETE (-)                       
         CR    R1,RE               INPUT LONGER THAN 8 (OR 9) ?                 
         BH    NGLENGTH            YES - ERROR                                  
VR26     MVI   VALUEACT,C'+'                                                    
         CLI   0(RF),C'-'                                                       
         BNE   VR28                                                             
         MVI   VALUEACT,C'-'                                                    
         BCTR  R1,0                                                             
         LA    RF,1(RF)                                                         
VR28     STC   R1,MYWORK+5                                                      
         MVC   MYWORK+8(L'PSPDDESC),SPACES  SPACE-FILL BEFORE EX'D MOVE         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   MYWORK+8(0),0(RF)                                                
*                                                                               
         MVC   VALUEVAL,MYWORK+8                                                
*                                                                               
VR40     DS    0H                                                               
         L     R2,ACURFORC                                                      
         XC    KEY,KEY                                                          
         MVC   PSPDAGY,AGENCY                                                   
         MVC   PSPDMED,QMED                                                     
         MVC   PSPDDESC,VALUEVAL                                                
         MVI   PSPDRCOD,PSPDRECQ                                                
*                                                                               
         MVI   DMINBTS,X'08'       LOOK FOR RECORD - READ FOR DELETES           
         GOTO1 HIGH                                                             
         CLI   DMCB+8,0                                                         
         BE    *+14                                                             
         TM    DMCB+8,X'02'        PASSIVE MAY BE DELETED                       
         BO    *+6                                                              
         DC    H'0'                                                             
         MVI   DMINBTS,0                                                        
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+16                                                             
         CLI   VALUEACT,C'+'                                                    
         BE    VR50                NOT PRESENT -- OK TO ADD                     
         B     NODELETE            . . . BUT CAN'T DELETE IT                    
*                                                                               
         TM    PSPDCNTL,X'80'      WAS POINTER DELETED?                         
         BZ    *+16                NO                                           
         CLI   VALUEACT,C'+'                                                    
         BE    VR50                                                             
         B     NODELETE            CAN'T DELETE -- IT'S ALREADY GONE            
         CLI   VALUEACT,C'-'                                                    
         BE    VR50                                                             
         B     NOADD               CAN'T ADD -- IT'S ALREADY THERE              
*                                                                               
VR50     LA    R1,VALUES                                                        
LP       CR    R1,R3                                                            
         BE    VR55                                                             
         CLC   VALUEVAL,0(R1)      DID THEY GIVE SAME ONE TWICE?                
         BE    NODUP               ERROR                                        
         LA    R1,VALUELNQ(R1)                                                  
         B     LP                                                               
VR55     LA    R3,VALUELNQ(R3)     BUMP TO NEXT ENTRY IN VALUE TABLE            
         MVI   VRSW,C'Y'           FIELD HAS BEEN MODIFIED                      
*                                                                               
VR60     ZIC   R0,0(R2)            BUMP TO NEXT FIELD                           
         AR    R2,R0                                                            
         CLI   0(R2),9             ANY MORE FIELDS?                             
         BH    VR20                YES                                          
*                                                                               
* RECORD HAS NOW BEEN VALIDATED                                                 
*                                                                               
         EJECT                                                                  
*                                                                               
* ADD OR DELETE SPACE DESCRIPTION RECORDS                                       
*                                                                               
*                                                                               
VR80     LA    R3,VALUES           LIST OF SAVED PRODUCTS                       
         CLI   0(R3),X'FF'         ANY MORE VALUES TO PROCESS?                  
         BE    VR190               NO                                           
*                                                                               
VR82     DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   PSPDAGY,AGENCY                                                   
         MVC   PSPDMED,QMED                                                     
         MVC   PSPDDESC,VALUEVAL                                                
         MVI   PSPDRCOD,PSPDRECQ                                                
*                                                                               
         MVI   DMINBTS,X'08'       LOOK FOR RECORD - READ FOR DELETES           
         GOTO1 HIGH                                                             
         MVI   DMINBTS,0                                                        
         CLC   KEY(25),KEYSAVE                                                  
         BE    VR90                                                             
         CLI   VALUEACT,C'+'                                                    
         BE    VRADD               NOT PRESENT -- OK TO ADD                     
         B     VR180               . . . NOTHING TO DELETE - NEXT ENTRY         
*                                                                               
VR90     DS    0H                                                               
         TM    PSPDCNTL,X'80'      WAS RECORD DELETED?                          
         BZ    VR92                NO                                           
         CLI   VALUEACT,C'+'                                                    
         BNE   VR180               . . . ALREADY DELETED   - NEXT ENTRY         
         NI    PSPDCNTL,X'FF'-X'80'    "UNDELETE"                               
         B     VRWRITE             RESTORE DELETED RECORD                       
VR92     DS    0H                                                               
         CLI   VALUEACT,C'-'                                                    
         BNE   VR180           CANNOT ADD - ALREADY EXISTS - NEXT ENTRY         
         OI    PSPDCNTL,X'80'                                                   
         B     VRWRITE             DELETE THE RECORD                            
*                                                                               
VRADD    DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   PSPDKEY,KEYSAVE         KEYSAVE HAS "RECORD NOT FOUND"           
         MVC   PSPDCNTL(2),=X'00FF'    DIRECTORY ONLY RECORD                    
         GOTO1 ADD                     ADD THE RECORD                           
         CLI   DMCB+8,0                                                         
         BE    VR180               TEST NEXT ENTRY                              
         DC    H'0'                                                             
*                                                                               
VRWRITE  DS    0H                                                               
         GOTO1 WRITE                   REWRITE THE RECORD                       
         CLI   DMCB+8,0                                                         
         BE    VR180               TEST NEXT ENTRY                              
         DC    H'0'                                                             
*                                                                               
VR180    DS    0H                                                               
         LA    R3,VALUELNQ(R3)     BUMP TO NEXT ENTRY IN VALUE TABLE            
         CLI   0(R3),X'FF'         ANY MORE VALUES TO PROCESS?                  
         BNE   VR82                YES                                          
*                                                                               
VR190    DS    0H                                                               
         LA    R2,SFCNEWH          CLEAR INPUT FIELDS                           
*                                                                               
VR200    CLI   5(R2),0             IF ANYTHING IS IN FIELD,                     
         BE    VR210                                                            
         ZIC   R1,0(R2)            . . . THEN CLEAR IT                          
         AHI   R1,-9               L'HDR + 1 FOR EX                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)                                                    
*                                                                               
VR210    OI    6(R2),X'80'         XMIT                                         
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),9             ANY MORE FIELDS TO CLEAR?                    
         BH    VR200               MAYBE                                        
*                                                                               
VRX      B     DR                                                               
*                                                                               
VRSW     DS    X                   VALREC SWITCH                                
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*        DISPLAY RECORD                                                         
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DR       DS    0H                                                               
         LA    R2,SFCLISTH                                                      
         TWAXC (R2),PROT=Y,TRNS=T  CLEAR LIST FIELDS                            
*                                                                               
         BAS   RE,CNTPLST          GET TOTAL NUMBER OF LIST MEMBERS             
*                                                                               
DR05     DS    0H                                                               
*        READ THE DIRECTORY ONLY RECORDS                                        
*                                                                               
* DISPLAY DESC'S FROM DIR.RECS IN LARGE PROTECTED FIELDS (76 CHARS)             
*                                                                               
         XC    DISP,DISP                                                        
         MVC   DISLEN,=H'17'       FOR SPACE DESCRIPTIONS                       
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PSPCDESD,R4                                                      
         MVC   PSPDAGY,AGENCY                                                   
         MVC   PSPDMED,QMED                                                     
         MVI   PSPDRCOD,PSPDRECQ                                                
         CLI   SFCSTRTH+5,0                                                     
         BNE   *+10                                                             
         XC    SFCSTRT,SFCSTRT                                                  
         MVC   PSPDDESC(17),SFCSTRT  LAST VALUE ALREADY DISPLAYED               
*                                                                               
         MVI   POSDSPSW,0          SWITCH FOR POSTION DISPLAYING                
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
DR20     DS    0H                                                               
         CLC   KEY(04),KEYSAVE     CHECK THROUGH MEDIA/RC                       
         BE    DR25                                                             
*                                                                               
         MVI   BYTE,C'P'           TELL SUBROUTINE TO DO POSITIONING            
         BAS   RE,CNTPLST          WILL RETURN POSNUMB (FULL WORD)              
*                                                                               
         LA    R1,SFCPFKH                                                       
         LA    R1,51(R1)           MOVE OVER 51 SPACES                          
         CLC   POSNUMB,NUMBMEM                                                  
         BNH   *+6                                                              
         DC    H'0'                POSITION NUMB HAS TO BE EQ OR LOWER          
*                                                                               
         L     RF,POSNUMB          CHECK IF NO MEMBER FOUND                     
         CHI   RF,0                                                             
         BE    DR23C                                                            
         L     RF,NUMBMEM          CHECK IF ZERO MEMBER IN LIST                 
         CHI   RF,0                                                             
         BNE   DR23E                                                            
DR23C    MVI   0(R1),C'0'                                                       
         LA    R1,2(R1)            MOVE OVER 2 SPACES                           
         MVC   0(7,R1),=C'members'                                              
         LA    R1,8(R1)            MOVE OVER 8 SPACES                           
         B     DR23P                                                            
*                                                                               
DR23E    DS    0H                                                               
         L     RF,POSNUMB                                                       
         L     R6,NUMBMEM                                                       
*                                                                               
         EDIT  (B4,POSNUMB),(4,00(R1)),0,COMMAS=YES,ZERO=NOBLANK,      +        
               ALIGN=LEFT                                                       
         AR    R1,R0               ADD NUMBER OF SIGNIFICANT NUMBS              
         MVI   0(R1),C'-'                                                       
         LA    R1,1(R1)            MOVE OVER A SPACE                            
         EDIT  (R6),(4,00(R1)),0,COMMAS=YES,ZERO=NOBLANK,ALIGN=LEFT             
         AR    R1,R0               ADD NUMBER OF SIGNIFICANT NUMBS              
         LA    R1,1(R1)            MOVE OVER A SPACE                            
         MVC   0(2,R1),=C'of'                                                   
         LA    R1,3(R1)            MOVE OVER 3 SPACES                           
         EDIT  (B4,NUMBMEM),(4,00(R1)),0,COMMAS=YES,ZERO=NOBLANK,      +        
               ALIGN=LEFT                                                       
         AR    R1,R0               ADD NUMBER OF SIGNIFICANT NUMBS              
         LA    R1,1(R1)            MOVE OVER A SPACE                            
DR23P    MVC   0(9,R1),=C'displayed'                                            
*                                                                               
         XC    SFCSTRT,SFCSTRT                                                  
         B     DR80                                                             
*                                                                               
DR25     DS    0H                                                               
         CLI   POSDSPSW,C'Y'       POSTION DISPLAYING SWITCH ON?                
         BE    *+14                                                             
         MVC   POSVALUE,PSPDDESC   NEEDED FOR VALUE TO BE POSITIONED            
         MVI   POSDSPSW,C'Y'       TURN SWITCH ON                               
*                                                                               
         LR    R1,R2               SEE IF I CAN FIT ON THIS LINE                
         LA    R1,8(R1)            GET PAST HEADER                              
         AH    R1,DISP             ADD CURRENT DISPLACEMNET                     
         AH    R1,DISLEN           ADD LENGTH OF ENTRY                          
         LR    R0,R2                                                            
         AHI   R0,84               76+8 END OF DISPLAY LINE                     
         CR    R1,R0                                                            
         BH    DR30                SKIP TO NEXT LINE                            
*                                                                               
         SH    R1,DISLEN                                                        
         LH    RE,DISLEN                                                        
         BCTR  RE,0                                                             
         EX    RE,DRMOVE                                                        
         LH    R1,DISP                                                          
         AH    R1,DISLEN                                                        
         AHI   R1,2                                                             
         STH   R1,DISP                                                          
         B     DR35                                                             
*                                                                               
DRMOVE   MVC   0(0,R1),PSPDDESC    EXECUTED                                     
*                                                                               
DR30     DS    0H                                                               
         OI    6(R2),X'80'         TRANSMIT                                     
         ZIC   R0,0(R2)                                                         
         AR    R2,R0               BUMP TO NEXT TWA FIELD                       
         LA    R0,SFCPFKH          A(PFKEY INSTRUCTIONS)                        
         CR    R2,R0               ANY MORE FIELDS AVAILABLE?                   
         BNL   DR50                NO                                           
         XC    DISP,DISP                                                        
         B     DR25                                                             
*                                                                               
DR35     GOTO1 SEQ                                                              
         MVC   SFCSTRT(17),PSPDDESC                                             
         B     DR20                                                             
*                                                                               
DR50     MVC   SFCSTRT(17),PSPDDESC  LAST DESCRIPTION DISPLAYED                 
         OI    SFCSTRTH+6,X'80'                                                 
         OI    6(R2),X'80'         TRANSMIT BIG BLOCK OF TEXT                   
*                                                                               
         MVI   BYTE,C'P'           TELL SUBROUTINE TO DO POSITIONING            
         BAS   RE,CNTPLST          WILL RETURN POSNUMB (FULL WORD)              
*                                                                               
         LA    R1,SFCPFKH                                                       
         LA    R1,51(R1)           MOVE OVER 51 SPACES                          
*                                                                               
         L     RF,POSNUMB          CHECK IF NO MEMBER FOUND                     
         CHI   RF,0                                                             
         BE    DR70C                                                            
         L     RF,NUMBMEM          CHECK IF ZERO MEMBER IN LIST                 
         CHI   RF,0                                                             
         BNE   DR70E                                                            
DR70C    MVI   0(R1),C'0'                                                       
         LA    R1,2(R1)            MOVE OVER 2 SPACES                           
         MVC   0(6,R1),=C'member'                                               
         LA    R1,7(R1)            MOVE OVER 7 SPACES                           
         B     DR70P                                                            
*                                                                               
DR70E    L     R6,POSNUMB                                                       
         AHI   R6,47               48=MAX NUM OF DESC'S PER SCREEN              
         EDIT  (B4,POSNUMB),(4,00(R1)),0,COMMAS=YES,ZERO=NOBLANK,      +        
               ALIGN=LEFT                                                       
         AR    R1,R0               ADD NUMBER OF SIGNIFICANT NUMBS              
         MVI   0(R1),C'-'                                                       
         LA    R1,1(R1)            MOVE OVER A SPACE                            
         EDIT  (R6),(4,00(R1)),0,COMMAS=YES,ZERO=NOBLANK,ALIGN=LEFT             
         AR    R1,R0               ADD NUMBER OF SIGNIFICANT NUMBS              
         LA    R1,1(R1)            MOVE OVER A SPACE                            
         MVC   0(2,R1),=C'of'                                                   
         LA    R1,3(R1)            MOVE OVER 3 SPACES                           
         EDIT  (B4,NUMBMEM),(4,00(R1)),0,COMMAS=YES,ZERO=NOBLANK,      +        
               ALIGN=LEFT                                                       
         AR    R1,R0               ADD NUMBER OF SIGNIFICANT NUMBS              
         LA    R1,1(R1)            MOVE OVER A SPACE                            
DR70P    MVC   0(9,R1),=C'displayed'                                            
*                                                                               
DR70U    OI    SFCPFKH+6,X'80'     TRANSMIT FLD                                 
*                                                                               
         MVC   CONHEAD,SPACES                                                   
         MVC   CONHEAD(35),=C'Items displayed, hit ENTER for more'              
         MVC   CONHEAD+35(17),=C' or enter changes'                             
         B     DR85                                                             
*                                                                               
DR80     DS    0H                                                               
         OI    6(R2),X'80'         TRANSMIT BIG BLOCK OF TEXT                   
         OI    SFCSTRTH+6,X'80'                                                 
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(19),=C'All items displayed'                              
         MVC   CONHEAD+19(15),=C', enter changes'                               
*                                                                               
DR85     OI    CONHEADH+6,X'80'                                                 
         OI    GENSTAT2,USMYOK                                                  
*                                                                               
         CLI   VRSW,C'Y'           RECORD HAS BEEN MODIFIED?                    
         BNE   DR99                                                             
         MVC   CONHEAD,SPACES                                                   
         MVC   CONHEAD+00(23),=C'Record has been changed'                       
         MVC   CONHEAD+23(23),=C', hit ENTER to continue'                       
*                                                                               
         OI    CONHEADH+6,X'80'                                                 
*                                                                               
DR99     DS    0H                                                               
         LA    R2,SFCNEWH          POINT TO FIRST "ADD SPACE.." FIELD           
         ST    R2,ACURFORC                                                      
         B     GOERREX2                                                         
*                                                                               
DRX      B     XIT                                                              
         DROP  R4                                                               
*                                                                               
POSDSPSW DS    X                   POSITION DISPLAYING SWITCH                   
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*        DISPLAY KEY     (NOT USED - NO LIST)                                   
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DK       DS    0H                                                               
         LA    R4,KEY                                                           
         USING PSPCDESD,R4                                                      
         MVC   DKSVKEY,PSPDKEY                                                  
*                                                                               
         MVC   SFCMED(1),PSPDKEY+2                                              
         LA    R2,SFCMEDH                                                       
         MVI   SFCMEDH+5,1                                                      
         OI    SFCMEDH+6,X'80'                                                  
         GOTO1 VALIMED                                                          
*                                                                               
         MVC   KEY,DKSVKEY                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                RECORD NOT RESTORED, BAD                     
*                                                                               
*                                                                               
DK90     DS    0H                                                               
         MVC   WKKEY,DKSVKEY      FOR VR USE                                    
*                                                                               
DKX      B     XIT                                                              
*                                                                               
DKSVKEY  DS    CL32               SAVING KEY FOR DK USES                        
*                                                                               
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*        LIST RECORDS    (NOT USED)                                             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
LR       DS    0H                                                               
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PSPDKEY,R4                                                       
         MVC   PSPDAGY,AGENCY                                                   
         MVI   PSPDMED,C'I'        FIRST MEDIA IS INTERACTIVE                   
*                                                                               
LR20     DS    0H                                                               
         GOTO1 HIGH                                                             
         CLI   KEYSAVE+3,X'FF'     SEE IF SKIP READING MEDIA                    
         BNE   LR30                                                             
         MVI   PSPDRCOD,PSPDRECQ                                                
         B     LR20                                                             
*                                                                               
LR30     DS    0H                                                               
         CLC   PSPDAGY,AGENCY      SAME AGENCY?                                 
         BNE   LRX                 NO - DONE                                    
*                                                                               
         CLI   PSPDRCOD,PSPDRECQ   MUST BE STD SPACE DESC REC                   
         BNE   LR85                NEXT MEDIA                                   
*                                                                               
         MVC   AIO,AIO2                                                         
         L     R6,AIO                                                           
         MVC   0(25,R6),KEY                                                     
*                                                                               
         MVC   LSTMED(LSTLNQ),SPACES                                            
*                                                                               
         MVC   LSTMED,PSPDMED                                                   
*                                                                               
         GOTO1 LISTMON                                                          
*                                                                               
         MVC   AIO,AIO1                                                         
*                                                                               
LR85     DS    0H                                                               
         MVI   PSPDRCOD,X'FF'                                                   
         XC    KEY+4(28),KEY+4                                                  
         B     LR20                SKIP TO NEXT MEDIA                           
*                                                                               
LRX      B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*        PRINT RECORDS                                                          
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PR       DS    0H                                                               
         LA    R3,HEADING          SET                                          
         ST    R3,SPECS                UP                                       
         LA    R3,HDHK                    REPORT                                
         ST    R3,HEADHOOK                    HEADINGS                          
*                                                                               
         LA    R2,P1+2             R2 POINTS TO PRINT-LINE                      
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY              CLEAR KEY                                    
         USING PSPCDESD,R4                                                      
         MVC   PSPDAGY,AGENCY                                                   
         MVC   PSPDMED,QMED                                                     
         MVI   PSPDRCOD,PSPDRECQ                                                
         GOTO1 HIGH                                                             
         B     PR20                                                             
*                                                                               
PR10     DS    0H                                                               
         GOTO1 SEQ                                                              
*                                                                               
PR20     DS    0H                                                               
         CLC   KEY(4),KEYSAVE     CHECK THROUGH MED/RC                          
         BNE   PRX                 DONE                                         
*                                                                               
         LA    R0,P1+112                                                        
         CR    R2,R0               ANY MORE FIELDS AVAILABLE?                   
         BL    PR30                YES                                          
         GOTO1 SPOOL,DMCB,(R8)     NO - PRINT THE LINE                          
         LA    R2,P1+2             RESET R2 TO BEGINNING OF LINE                
*                                                                               
PR30     DS    0H                                                               
         MVC   0(L'PSPDDESC,R2),PSPDDESC                                        
         LA    R2,20(R2)                                                        
         B     PR10                NEXT RECORD                                  
*                                                                               
*                                                                               
PRX      DS    0H                                                               
         GOTO1 SPOOL,DMCB,(R8)     LAST LINE                                    
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*        HEADER ROUTINE                                                         
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
HDHK     NTR1                                                                   
         MVC   H1+10(1),QMED                                                    
         MVC   H1+15(10),MEDNM                                                  
         OC    H1+10(15),SPACES                                                 
         MVC   H1+52(27),=C'STANDARD SPACE DESCRIPTIONS'                        
         MVC   H2+52(27),=C'---------------------------'                        
*                                                                               
HDHKX    XIT1                                                                   
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
HEADING  SSPEC H1,1,C'MEDIA'                                                    
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H1,95,AGYNAME                                                    
         SSPEC H2,95,AGYADD                                                     
         SSPEC H3,95,RUN                                                        
         SSPEC H4,95,REPORT                                                     
         SSPEC H4,112,PAGE                                                      
         DC    X'00'                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*        ROUTINE COUNTING NUMBER OF SPACE DESCRIPTION RECORDS                   
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CNTPLST  NTR1                      *****  KEY IS NOT SAVED  *****               
         CLI   BYTE,C'P'           DOING POSITION CALCULATION?                  
         BE    *+10                                                             
         XC    NUMBMEM,NUMBMEM     CLEAR COUNTER                                
         XC    POSNUMB,POSNUMB     CLEAR POSTION NUMBER COUNTER                 
         XC    KEY,KEY                                                          
         LA    RE,KEY                                                           
         USING PSPDKEY,RE                                                       
         MVC   PSPDAGY,AGENCY                                                   
         MVC   PSPDMED,QMED                                                     
         MVI   PSPDRCOD,PSPDRECQ                                                
         DROP  RE                                                               
         GOTO1 HIGH                                                             
*                                                                               
         B     CNTPL50                                                          
*                                                                               
CNTPL30  GOTO1 SEQ                                                              
*                                                                               
CNTPL50  CLC   KEY(04),KEYSAVE     CHECK THROUGH MEDIA                          
         BNE   CNTPLX                                                           
         CLI   BYTE,C'P'           DOING POSITION CALCULATIONS?                 
         BNE   CNTPL60                                                          
         CLC   KEY+4(17),POSVALUE  COMPARE SPACE DESCRIPTION                    
         BH    CNTPLX                                                           
         L     RE,POSNUMB                                                       
         AHI   RE,1                                                             
         ST    RE,POSNUMB                                                       
         B     CNTPL30                                                          
CNTPL60  L     RE,NUMBMEM                                                       
         AHI   RE,1                COUNTING MEMBERS FOUND                       
         ST    RE,NUMBMEM                                                       
         B     CNTPL30             NEXT RECORD                                  
*                                                                               
CNTPLX   B     XIT                                                              
*                                                                               
NUMBMEM  DS    F                   NUMBER OF MEMBERS COUNTED                    
POSNUMB  DS    F                   "POSITION NUMBER" OF SPACE DESC.             
POSVALUE DS    CL17                VALUE THAT NEED TO BE POSTIONED              
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
INITBKNM DS    0H                  INIT BREAK NAME/LEN FLDS AND XMIT            
*                                                                               
INITBNX  BR    RE                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
RELO     DS    F                                                                
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
*                                                                               
GOERREX2 GOTO1 ERREX2                                                           
*                                                                               
*                                                                               
NODELETE XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NODELETM),NODELETM                                     
         B     GOERREX2                                                         
NODELETM DC    C'* ERROR * CANNOT DELETE - RECORD NOT ON FILE'                  
*                                                                               
NOADD    XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NOADDM),NOADDM                                         
         B     GOERREX2                                                         
NOADDM   DC    C'* ERROR * RECORD ALREADY ON FILE'                              
*                                                                               
NODUP    XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NODUPM),NODUPM                                         
         B     GOERREX2                                                         
NODUPM   DC    C'* ERROR * CANNOT HAVE DUPLICATES'                              
*                                                                               
NGLENGTH XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'TOOLONGM),TOOLONGM                                     
         B     GOERREX2                                                         
TOOLONGM DC    C'* ERROR * 8 CHARACTER MAXIMUM FOR NEWSPAPER DESC''S'           
*                                                                               
NGMEDO   EQU   87                  "INVALID FOR SELECTED RECORD TYPE"           
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         PRINT OFF                                                              
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE PRSFMFFD                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFMD6D          MAINT SCREEN                                 
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE PRSFMWORKD                                                     
         PRINT ON                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         EJECT                                                                  
*                                                                               
* WORK AREA                                                                     
*                                                                               
         ORG   SYSSPARE                                                         
*                                                                               
SAVEKEY  DS    XL32                                                             
*                                                                               
VALUES   DS    XL((7*VALUELNQ)+1) SAVED VALUES FROM SCREEN                      
*                      (MAXIMUM OF 7 ADD OR DELETE SPACE DESCRIPTONS)           
MYWORK   DS    XL64                                                             
SH7      DS    CL132                                                            
SVNMS    DS    CL55                                                             
DFLG     DS    C                   STUFF TO PRINT                               
CONT     DS    C                   CONTINUATION FLAG                            
GOTU     DS    C                   GOT ONE TO UNDERLINE FLAG                    
*                                                                               
DISP     DS    H                                                                
DISLEN   DS    H                                                                
*                                                                               
SVNAME1  DS    XL24                SAVE NAME 1 FOR VR USES                      
SVNAME2  DS    XL24                SAVE NAME 2 FOR VR USES                      
*                                                                               
WKKEY    DS    CL32                WORKING STORAGE KEY                          
*                                                                               
         EJECT                                                                  
       ++INCLUDE PSPCDES                                                        
         EJECT                                                                  
VALUED   DSECT                                                                  
*                                                                               
VALUEVAL DS    CL(L'PSPDDESC)      VALUE TO BE STORED IN RECORD                 
VALUEACT DS    C                   USER ACTION (C'+' OR C'-')                   
*                                                                               
VALUELNQ EQU   *-VALUED                                                         
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LSTMED   DS    CL1                                                              
LSTLNQ   EQU   *-LSTMED                                                         
*                                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004PRSFM21   09/29/10'                                      
         END                                                                    
