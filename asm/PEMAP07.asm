*          DATA SET PEMAP07    AT LEVEL 003 AS OF 08/22/00                      
*PHASE TE1B07A                                                                  
         TITLE 'TE1B07 - DIARY RECORD MAINTENANCE'                              
TE1B07   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**DIMN**                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         SPACE 1                                                                
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VKEY                                                             
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VREC                                                             
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DKEY                                                             
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DREC                                                             
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LIST                                                             
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BNE   XIT                                                              
         LA    R1,HEDSPECS                                                      
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         B     LIST                                                             
         EJECT                                                                  
*              VALIDATE AND DISPLAY KEY                                         
         SPACE 3                                                                
VKEY     LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         CLI   ACTEQU,1                                                         
         BNE   XIT                                                              
         USING MAPKEY,R4                                                        
         MVC   MAPAGY,AGENCY                                                    
         MVI   MAPSYS,C'B'                                                      
         MVI   DIAKTYP,X'07'                                                    
         LA    R2,DIACODH                                                       
         GOTO1 VALIPERS                                                         
         MVC   DIAKCODE,WORK                                                    
*                                  VALIDATE THE DATE                            
         LA    R2,DIAWEEKH                                                      
         CLI   5(R2),0                                                          
         BE    VKEY8               NO INPUT, LET VALIPER SAY MISSING            
         ZIC   RF,5(R2)            CHECK FOR KEYWORDS, IE. THIS,LAST            
         BCTR  RF,0                NEXT.                                        
         LA    RE,WEEKTAB                                                       
VKEY2    CLI   0(RE),X'FF'                                                      
         BE    VKEY8               NOT A KEYWORD                                
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),0(RE)                                                    
         BE    VKEY4               FOUND THE KEYWORD                            
         LA    RE,L'WEEKTAB(RE)                                                 
         B     VKEY2                                                            
*                                                                               
VKEY4    LH    R0,8(RE)            GET INC/DECREMENT                            
         GOTO1 DATCON,DMCB,(3,BTODAY),(0,WORK+6)                                
         GOTO1 ADDAY,DMCB,WORK+6,WORK,(R0)                                      
         B     VKEY82              NOW GET WEEK START                           
*                                                                               
VKEY8    DS    0H                                                               
*&&UK*&& GOTO1 VALIPER,DMCB,WORK                                                
*&&US*&& GOTO1 VALIDATE,DMCB,WORK                                               
VKEY82   GOTO1 GETDAY,DMCB,WORK,DUB                                             
         LA    RF,1                OBTAIN MONDAY OF WEEK                        
         ZIC   RE,DMCB                                                          
         SR    RF,RE                                                            
         BZ    VKEYA                                                            
         GOTO1 ADDAY,DMCB,WORK,WORK+6,(RF)                                      
         MVC   WORK(6),WORK+6                                                   
*                                                                               
VKEYA    GOTO1 DATCON,DMCB,(0,WORK),(1,DIAKDATE)                                
         LA    R2,DIADAYS+3        COMPLETE THE HEAD LINE                       
         LA    R0,5                                                             
VKEYC    GOTO1 DATCON,DMCB,(0,WORK),(7,0(R2))                                   
         GOTO1 ADDAY,DMCB,WORK,WORK+6,1                                         
         MVC   WORK(6),WORK+6                                                   
         LA    R2,10(R2)                                                        
         BCT   R0,VKEYC                                                         
         OI    DIADAYSH+6,X'80'                                                 
         B     XIT                                                              
         SPACE 1                                                                
DKEY     LA    R4,KEY              DISPLAY KEY (FOR SELECT)                     
         LA    R2,DIACODH          DIARIST                                      
         MVC   8(8,R2),DIAKCODE                                                 
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R2,DIAWEEKH         WEEK START                                   
         GOTO1 DATCON,DMCB,(1,DIAKDATE),(8,DIAWEEK)                             
         OI    6(R2),X'80'                                                      
*                                                                               
         GOTO1 DATCON,DMCB,(1,DIAKDATE),(0,WORK)                                
         LA    R2,DIADAYS+3        COMPLETE THE HEAD LINE                       
         LA    R0,5                                                             
DKEY6    GOTO1 DATCON,DMCB,(0,WORK),(7,0(R2))                                   
         GOTO1 ADDAY,DMCB,WORK,WORK+6,1                                         
         MVC   WORK(6),WORK+6                                                   
         LA    R2,10(R2)                                                        
         BCT   R0,DKEY6                                                         
         OI    DIADAYSH+6,X'80'                                                 
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE AND DISPLAY RECORD                                      
         SPACE 3                                                                
VREC     DS    0H                  DISPLAY                                      
DREC     LA    R2,DIALINEH         VALIDATE DIARY DETAILS                       
DVREC    MVI   ELCODE,X'70'                                                     
         MVI   MAX,18                                                           
         MVI   OPTION,0                                                         
         GOTO1 VALICHAT                                                         
         B     XIT                                                              
         EJECT                                                                  
*              LIST RECORDS                                                     
         SPACE 3                                                                
LIST     LA    R4,KEY                                                           
         OC    KEY,KEY             WAS A KEY PROVIDED?                          
         BNZ   LISTG                                                            
         USING MAPKEYD,R4                                                       
         MVC   MAPAGY,AGENCY                                                    
         MVI   MAPSYS,C'B'                                                      
         MVI   DIAKTYP,X'07'                                                    
         CLI   DILCODH+5,0         OPTIONAL DIARIST INPUT                       
         BE    LISTC               NO PERSON, NO DATE                           
         LA    R2,DILCODH                                                       
         GOTO1 VALIPERS                                                         
         MVC   DIAKCODE,WORK                                                    
*                                                                               
         CLI   DILDATH+5,0         OPTIONAL START DATE                          
         BE    LISTC                                                            
         LA    R2,DILDATH                                                       
         CLI   5(R2),0                                                          
         BE    LIST8               NO INPUT, LET VALIPER SAY MISSING            
         ZIC   RF,5(R2)            CHECK FOR KEYWORDS, IE. THIS,LAST            
         BCTR  RF,0                NEXT.                                        
         LA    RE,WEEKTAB                                                       
LIST2    CLI   0(RE),X'FF'                                                      
         BE    LIST8               NOT A KEYWORD                                
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),0(RE)                                                    
         BE    LIST4               FOUND THE KEYWORD                            
         LA    RE,L'WEEKTAB(RE)                                                 
         B     LIST2                                                            
*                                                                               
LIST4    LH    R0,8(RE)            GET INC/DECREMENT                            
         GOTO1 DATCON,DMCB,(3,BTODAY),(0,WORK+6)                                
         GOTO1 ADDAY,DMCB,WORK+6,WORK,(R0) GET TODAY+INC/DEC                    
         B     LIST82              NOW GET WEEK START                           
*                                                                               
LIST8    GOTO1 VALIPER,DMCB,WORK                                                
LIST82   GOTO1 GETDAY,DMCB,WORK,DUB                                             
         LA    RF,1                OBTAIN MONDAY OF WEEK                        
         ZIC   RE,DMCB                                                          
         SR    RF,RE                                                            
         BZ    LISTA                                                            
         GOTO1 ADDAY,DMCB,WORK,WORK+6,(RF)                                      
         MVC   WORK(6),WORK+6                                                   
*                                                                               
LISTA    GOTO1 DATCON,DMCB,(0,WORK),(1,DIAKDATE)                                
         SPACE 1                                                                
LISTC    GOTO1 HIGH                                                             
         B     LISTG                                                            
         SPACE 1                                                                
LISTE    GOTO1 SEQ                                                              
         SPACE 1                                                                
LISTG    CLC   KEY(3),KEYSAVE      CHECKMAIN C/B                                
         BNE   XIT                                                              
         CLI   DILCODH+5,0                                                      
         BE    *+14                NO DIARIST INPUT                             
         CLC   KEY(11),KEYSAVE                                                  
         BNE   XIT                 CHANGE OF DIARIST                            
         GOTO1 GETREC                                                           
         MVC   LISTAR,SPACES       SHOW CODE                                    
         LA    R3,LISTAR                                                        
         LA    R5,DILHED                                                        
         OI    DILHEDH+6,X'80'                                                  
         MVC   0(8,R3),DIAKCODE                                                 
         MVC   0(7,R5),=C'DIARIST'                                              
         SPACE 1                                                                
         LA    R3,10(R3)           DATE                                         
         LA    R5,10(R5)                                                        
         GOTO1 DATCON,DMCB,(1,DIAKDATE),(8,0(R3))                               
         MVC   0(4,R5),=C'DATE'                                                 
         SPACE 1                                                                
         GOTO1 LISTMON                                                          
         B     LISTE                                                            
         SPACE 1                                                                
BUMP     ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         BR    RE                                                               
         SPACE 1                                                                
XIT      XIT1                                                                   
         SPACE 1                                                                
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 1                                                                
TRAPERR  GOTO1 ERREX                                                            
         B     XIT                                                              
         SPACE 1                                                                
SPLAT    NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         EJECT                                                                  
*              HEADLINE ROUTINES                                                
         SPACE 3                                                                
HOOK     NTR1                                                                   
         B     XIT                                                              
         SPACE 2                                                                
HEDSPECS SSPEC H1,2,C'SYSTEM CONTROL'                                           
         SSPEC H2,2,C'--------------'                                           
         SSPEC H1,40,C'PERSON REPORT'                                           
         SSPEC H2,40,C'-------------'                                           
         SSPEC H1,77,AGYNAME                                                    
         SSPEC H2,77,AGYADD                                                     
         SSPEC H4,77,REPORT                                                     
         SSPEC H5,77,REQUESTOR                                                  
         SSPEC H5,103,PAGE                                                      
         DC    X'00'               END MARKER FOR SPECS                         
         EJECT                                                                  
         DS    0H                                                               
WEEKTAB  DS    0CL10                                                            
         DC    CL8'THIS',Y(0)                                                   
         DC    CL8'NEXT',Y(7)                                                   
         DC    CL8'LAST',Y(-7)                                                  
         DC    AL1(255)                                                         
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE PEMAPFILE                                                      
       ++INCLUDE PEMAPFFD                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE PEMAPF7D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE PEMAPE7D                                                       
         PRINT OFF                                                              
       ++INCLUDE PEMAPWORKD                                                     
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003PEMAP07   08/22/00'                                      
         END                                                                    
