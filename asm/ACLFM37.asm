*          DATA SET ACLFM37    AT LEVEL 030 AS OF 05/01/02                      
*PHASE T60337A,+0                                                               
*INCLUDE ACDELEL                                                                
         TITLE 'MAINTAIN CLIENT GROUPS ON UNIT F'                               
T60337   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 LWSX-LWSD,**LFM37*,R9,RR=R5,CLEAR=YES                            
         LR    R8,RC                                                            
         USING LWSD,R8                                                          
         L     RA,0(R1)                                                         
         USING T603FFD,RA                                                       
         L     RC,4(R1)                                                         
         USING LOGWORKD,RC                                                      
         ST    R5,PRELOC                                                        
         ST    RB,SAVRB                                                         
                                                                                
         USING COMFACSD,R5                                                      
         L     R5,COMFACS                                                       
         EJECT                                                                  
*-----------------------------------------------------------------*             
*        IS THIS FIRST TIME IN                                    *             
*        BUILD KEY FOR COMPANY/UNIT/LEDGER/ACCOUNT                *             
*        - DISPLAY NAMES OF ABOVE                                 *             
*-----------------------------------------------------------------*             
         MVI   ERROR,X'FF'                                                      
         CLI   MODE,BUILDKEY       IS THIS A NEW RECORD?                        
         BNE   DSP10               NO                                           
                                                                                
         TM    LOGACTH+4,X'80'                                                  
         BZ    BLD05                                                            
         NI    LOGLEDGH+4,X'FF'-X'20'                                           
         NI    LOGACCTH+4,X'FF'-X'20'                                           
                                                                                
BLD05    DS    0H                                                               
         MVC   KEY,SPACES          CLEAR KEY                                    
         MVC   KEY(1),COMPANY      ADD COMPANY TO KEY                           
         MVI   KEY+1,C'F' UNIT                                                  
         LA    R2,LOGLEDGH         LEDGER HEADER                                
         GOTO1 ANY                                                              
         MVC   KEY+2(1),LOGLEDG    ADD LEDGER TO KEY                            
         TM    LOGLEDGH+4,X'20'    WAS PREVALID BIT SET                         
         BO    BLD25               YES                                          
         FOUT  LOGLNAMH,SPACES,36  CLEAR LEDGER NAME                            
         NI    LOGACCTH+4,X'FF'-X'20'    UNVALIDATE FOR DISPLAY MODE            
         BAS   RE,CLRSCRN          CLEAR SCREEN                                 
         GOTO1 READ                READ LEDGER RECORD                           
         GOTO1 NAMOUT              DISPLAY LEDGER NAME                          
         OI    LOGLEDGH+4,X'20'    SET PREVALID BIT FOR LEDGER                  
         MVI   ANYKEY,C'Y'                                                      
                                                                                
         LA    R4,IO               GET HEIRARCHY ELEMENT                        
         AH    R4,DATADISP                                                      
BLD15    CLI   0(R4),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R4),ACLELQ                                                     
         BE    BLD18                                                            
         ZIC   R5,1(R4)                                                         
         AR    R4,R5                                                            
         B     BLD15                                                            
                                                                                
         USING ACLELD,R4                                                        
BLD18    MVC   TSIDHEIR,0(R4)      SAVE HEIRARCHY ELEMENT OFF THIS              
         MVI   MINLEN,0            UNIT AND LEDGER                              
         LA    R5,ACLVALS          CALCULATE THE MINIMUM LENGTH                 
BLD19    CLI   0(R5),12            ACCOUNT INPUT MUST BE                        
         BE    BLD20               STORE IN MINLEN                              
         MVC   MINLEN,0(R5)                                                     
         LA    R5,16(R5)                                                        
         B     BLD19                                                            
BLD20    ZIC   R5,MINLEN                                                        
         LA    R5,1(R5)                                                         
         STC   R5,MINLEN                                                        
                                                                                
BLD25    DS    0H                                                               
         TM    LOGACCTH+4,X'20'                                                 
         BO    BLDXIT                                                           
         FOUT  LOGANAMH,SPACES,36                                               
         BAS   RE,CLRSCRN          CLEAR SCREEN                                 
         LA    R2,LOGACCTH         CHECK TO SEE IF IT IS LOW LEVEL              
         GOTO1 ANY                 ACCOUNT                                      
         CLC   MINLEN,LOGACCTH+5                                                
         BNH   BLD30                                                            
         MVI   ERROR,ACCINVAL                                                   
         B     BLDXIT                                                           
                                                                                
         USING ACTRECD,R3                                                       
BLD30    LA    R3,IO2                                                           
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,COMPANY                                                  
         MVI   ACTKUNT,C'F'                                                     
         MVC   ACTKLDG,LOGLEDG                                                  
         ZIC   R5,MINLEN                                                        
         BCTR  R5,0                                                             
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   ACTKACT(0),LOGACCT                                               
         MVC   COMMAND,=C'DMREAD  '                                             
         GOTO1 DATAMGR,DMCB,(DMINBTS,COMMAND),=C'ACCFIL',IO2,IO2                
         CLI   8(R1),0                                                          
         BE    BLD35                                                            
         MVC   LOGHEAD,SPACES                                                   
         MVC   LOGHEAD(18),=C'MISSING HIGH LEVEL'                               
         OI    LOGHEADH+6,X'80'                                                 
         MVI   ERROR,X'FE'                                                      
         LA    R2,LOGACCTH                                                      
         B     BLDXIT                                                           
                                                                                
BLD35    LA    R2,LOGACCTH                                                      
         MVI   ANYKEY,C'Y'                                                      
                                                                                
         USING ACTRECD,R3                                                       
         LA    R3,IO2                                                           
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,COMPANY                                                  
         MVI   ACTKUNT,C'F'                                                     
         MVC   ACTKLDG,LOGLEDG                                                  
         ZIC   R5,LOGACCTH+5                                                    
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   ACTKACT(0),LOGACCT                                               
         OI    DMINBTS,X'88'                                                    
         MVC   COMMAND,=C'DMREAD  '                                             
         GOTO1 DATAMGR,DMCB,(DMINBTS,COMMAND),=C'ACCFIL',IO2,IO2                
         CLI   8(R1),0                                                          
         BE    BLD40                                                            
         LA    RE,IO2                                                           
         LA    RF,2000                                                          
         XCEFL                                                                  
         OI    LOGACCTH+4,X'20'    VALIDATED PREVIOUSLY                         
         CLI   LOGACT,C'N'                                                      
         BE    BLDXIT                                                           
         MVC   LOGHEAD,SPACES                                                   
         MVC   LOGHEAD(18),=C'RECORD NOT ON FILE'                               
         OI    LOGHEADH+6,X'80'                                                 
         MVI   ERROR,X'FE'                                                      
         LA    R2,LOGACTH                                                       
         NI    LOGACCTH+4,X'FF'-X'20'                                           
         B     BLDXIT                                                           
                                                                                
BLD40    CLI   LOGACT,C'N'                                                      
         BNE   BLD42                                                            
         MVC   LOGHEAD,SPACES                                                   
         MVC   LOGHEAD(22),=C'RECORD ALREADY ON FILE'                           
         OI    LOGHEADH+6,X'80'                                                 
         MVI   ERROR,X'FE'                                                      
         LA    R2,LOGACTH                                                       
         NI    LOGACCTH+4,X'FF'-X'20'                                           
         B     BLDXIT                                                           
                                                                                
BLD42    LA    R3,IO2                                                           
         MVI   ELCODE,NAMELQ                                                    
         BAS   RE,GETEL                                                         
         BNE   BLD60                                                            
         USING NAMELD,R3                                                        
         ZIC   R6,1(R3)                                                         
         SH    R6,=H'3'                                                         
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   LOGANAM(0),NAMEREC                                               
         OI    LOGANAMH+6,X'80'                                                 
                                                                                
BLD60    BAS   RE,BLDCTAB          BUILD TABLE OF CLIENTS FROM RECORD           
         MVI   SCRNUM,0            INITIALIZE SCREEN NUMBER                     
         LA    R5,CLIBLOCK                                                      
         BAS   RE,DSPSCRN          DISPLAY SCREEN                               
         OI    LOGACCTH+4,X'20'    VALIDATED PREVIOUSLY                         
                                                                                
BLD99    LA    R2,LOGLEDGH         SET CURSOR POSITION                          
BLDXIT   B     XXIT                                                             
         EJECT                                                                  
*-----------------------------------------------------------------*             
*        DISPLAY MODE                                                           
*-----------------------------------------------------------------*             
DSP10    CLI   MODE,DSPLYREC       ARE WE IN DISPLAY MODE?                      
         BNE   CHA01               NO                                           
                                                                                
         LA    R2,LOGACCTH         R2 = ACCOUNT HEADER                          
         CLI   PFKEY,0             IF NO PFKEY CHECK TO SEE IF STARTING         
         BE    DSP50               ACCOUNT HAS CHANGED                          
         CLI   PFKEY,5             PF5=DISPLAY UP                               
         BNE   DSP30                                                            
         CLI   SCRNUM,0            IF DISPLAYING FIRST KEY IN TABLE             
         BE    DSP90               CAN'T GO BACK ANY FURTHER                    
         BAS   RE,CLRSCRN          CLEAR SCREEN                                 
         LA    R5,CLIBLOCK                                                      
         ZIC   R6,SCRNUM           RESET CURRENT SCREEN DISPLAY NUMBER          
         BCTR  R6,0                                                             
         STC   R6,SCRNUM                                                        
         LTR   R6,R6                                                            
         BZ    DSP25                                                            
         MH    R6,=H'90'                                                        
         AR    R5,R6                                                            
DSP25    BAS   RE,DSPSCRN          DISPLAY NEW SCREEN                           
         B     DSP90                                                            
                                                                                
DSP30    CLI   PFKEY,6             DISPLAY DOWN ONE SCREEN                      
         BNE   DSPXIT                                                           
         CLI   SCRNUM,SCRNMAX                                                   
         BE    DSP90                                                            
         LA    R5,CLIBLOCK                                                      
         ZIC   R6,SCRNUM           RESET CURRENT SCREEN DISPLAY NUMBER          
         LA    R6,1(R6)                                                         
         MH    R6,=H'90'                                                        
         AR    R5,R6                                                            
         OC    0(1,R5),0(R5)                                                    
         BZ    DSP90                                                            
         CLI   0(R5),X'FF'                                                      
         BE    DSP90                                                            
         ZIC   R6,SCRNUM           RESET CURRENT SCREEN DISPLAY NUMBER          
         LA    R6,1(R6)                                                         
         STC   R6,SCRNUM                                                        
         BAS   RE,CLRSCRN          CLEAR SCREEN                                 
         BAS   RE,DSPSCRN          DISPLAY NEW SCREEN                           
         B     DSP90                                                            
                                                                                
DSP50    DS    0H                                                               
DSP90    MVC   LOGHEAD,SPACES                                                   
         MVC   LOGHEAD(10),=C'ENTER NEXT'                                       
DSP99    OI    LOGHEADH+6,X'80'                                                 
         MVI   ERROR,X'FE'                                                      
         LA    R2,LOGACCTH                                                      
DSPXIT   B     XXIT                                                             
         EJECT                                                                  
*-----------------------------------------------------------------*             
*        RECORD CHANGE MODE                                                     
*-----------------------------------------------------------------*             
CHA01    DS    0H                                                               
         MVI   ERROR,X'FF'                                                      
         BAS   RE,UPDCTAB                                                       
         CLI   ERROR,X'FF'                                                      
         BNE   CHAXIT                                                           
                                                                                
         CLI   PFKEY,9               DO THEY WANT TO DELETE RECORD              
         BE    CHADEL                                                           
                                                                                
         CLI   PFKEY,7                                                          
         BNE   CHA12                                                            
                                                                                
         LA    R2,LOGCLIH                                                       
         BAS   RE,UPDREC                                                        
         CLI   ERROR,X'FF'                                                      
         BNE   CHAXIT                                                           
                                                                                
         BAS   RE,BLDCTAB                                                       
         MVI   SCRNUM,0                                                         
         BAS   RE,CLRSCRN          CLEAR SCREEN                                 
         LA    R5,CLIBLOCK                                                      
         BAS   RE,DSPSCRN                                                       
         CLI   ERROR,X'FF'                                                      
         BNE   CHAXIT                                                           
         MVC   LOGHEAD,SPACES                                                   
         MVC   LOGHEAD(27),=C'RECORD UPDATED - ENTER NEXT'                      
         B     CHA99                                                            
                                                                                
CHA12    CLI   PFKEY,0             IF NO PFKEY CHECK TO SEE IF STARTING         
         BE    CHA90               ACCOUNT HAS CHANGED                          
         CLI   PFKEY,5             PF5=DISPLAY UP                               
         BNE   CHA30                                                            
         CLI   SCRNUM,0            IF DISPLAYING FIRST KEY IN TABLE             
         BE    CHA90               CAN'T GO BACK ANY FURTHER                    
         BAS   RE,CLRSCRN          CLEAR SCREEN                                 
         LA    R5,CLIBLOCK                                                      
         ZIC   R6,SCRNUM           RESET CURRENT SCREEN DISPLAY NUMBER          
         BCTR  R6,0                                                             
         STC   R6,SCRNUM                                                        
         LTR   R6,R6                                                            
         BZ    CHA15                                                            
         MH    R6,=H'90'                                                        
         AR    R5,R6                                                            
CHA15    BAS   RE,DSPSCRN          DISPLAY NEW SCREEN                           
         CLI   ERROR,X'FF'                                                      
         BNE   CHAXIT                                                           
         B     CHA90                                                            
                                                                                
CHA30    CLI   PFKEY,6             DISPLAY DOWN ONE SCREEN                      
         BNE   CHA90                                                            
         CLI   SCRNUM,SCRNMAX                                                   
         BE    CHA90                                                            
         LA    R5,CLIBLOCK                                                      
         ZIC   R6,SCRNUM           RESET CURRENT SCREEN DISPLAY NUMBER          
         LA    R6,1(R6)                                                         
         MH    R6,=H'90'                                                        
         AR    R5,R6                                                            
         ZIC   R6,SCRNUM           RESET CURRENT SCREEN DISPLAY NUMBER          
         LA    R6,1(R6)                                                         
         STC   R6,SCRNUM                                                        
         BAS   RE,CLRSCRN          CLEAR SCREEN                                 
         BAS   RE,DSPSCRN          DISPLAY NEW SCREEN                           
         CLI   ERROR,X'FF'                                                      
         BNE   CHAXIT                                                           
                                                                                
CHA90    MVC   LOGHEAD,SPACES                                                   
         MVC   LOGHEAD(13),=C'ENTER CHANGES'                                    
CHA99    OI    LOGHEADH+6,X'80'                                                 
         MVI   ERROR,X'FE'                                                      
         LA    R2,LOGACCTH                                                      
         B     CHAXIT                                                           
                                                                                
CHADEL   MVI   ERROR,X'FF'       INITIALIZE ERROR TO NO ERROR                   
         USING ACTRECD,R3                                                       
         LA    R3,MYKEY                                                         
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,COMPANY                                                  
         MVI   ACTKUNT,C'F'                                                     
         MVC   ACTKLDG,LOGLEDG                                                  
         ZIC   R5,LOGACCTH+5                                                    
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   ACTKACT(0),LOGACCT                                               
         OI    DMINBTS,X'80'        ONLY READ FOR UPDATE                        
         MVC   COMMAND,=C'DMREAD  '                                             
         GOTO1 DATAMGR,DMCB,(DMINBTS,COMMAND),=C'ACCFIL',MYKEY,IO2              
         CLI   8(R1),0                                                          
         BE    DELR10                                                           
         MVC   LOGHEAD,SPACES                                                   
         MVC   LOGHEAD(24),=C'** RECORD NOT ON FILE **'                         
         OI    LOGHEAD+6,X'80'                                                  
         MVI   ERROR,X'FE'                                                      
         LA    R2,LOGACCTH                                                      
         B     XXIT                                                             
                                                                                
DELR10   DS    0H                                                               
         CLC   LOGANAM(6),=C'DELETE'                                            
         BE    DELR20                                                           
         MVC   LOGHEAD,SPACES                                                   
         MVC   LOGHEAD(26),=C'** INDICATE DELETE HERE **'                       
         OI    LOGHEAD+6,X'80'                                                  
         MVI   ERROR,X'FE'                                                      
         LA    R2,LOGANAMH                                                      
         B     XXIT                                                             
                                                                                
DELR20   LA    R3,IO2                                                           
         OI    44(R3),X'80'                                                     
         MVC   COMMAND,=C'DMWRT   '                                             
         GOTO1 DATAMGR,DMCB,COMMAND,=CL8'ACCFIL',IO2,IO2                        
         CLI   8(R1),0                UPDATE RECORD                             
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   LOGACCT,SPACES                                                   
         OI    LOGACCTH+6,X'80'                                                 
         MVC   LOGANAM,SPACES                                                   
         OI    LOGANAMH+6,X'80'                                                 
         BAS   RE,CLRSCRN                                                       
         MVC   LOGHEAD,SPACES                                                   
         MVC   LOGHEAD(20),=C'** RECORD DELETED **'                             
         OI    LOGHEAD+6,X'80'                                                  
         MVI   ERROR,X'FE'                                                      
         LA    R2,LOGACCTH                                                      
         B     XXIT                                                             
                                                                                
CHAXIT   B     XXIT                                                             
         EJECT                                                                  
*-----------------------------------------------------------------*             
*        DISPLAY SCREEN ENTRY - MYKEY MUST BE SET TO ACCOUNT                    
*        YOU WANT SCREEN DISPLAY TO BEGIN AT                                    
*-----------------------------------------------------------------*             
DSPSCRN  NTR1                                                                   
         LA    R4,SCRNENTS         NUMBER OF LINES PER SCREEN                   
         LA    R2,LOGCLIH                                                       
                                                                                
         USING SCRND,R2                                                         
DSCR10   CLI   0(R5),X'FF'                                                      
         BE    DSCRXIT                                                          
         OC    0(1,R5),0(R5)                                                    
         BZ    DSCRXIT                                                          
         LA    R2,SCRCLI1H                                                      
         MVC   SCRCLI1,0(R5)                                                    
         OI    SCRCLI1H+6,X'80'                                                 
         CLC   0(3,R5),=C'***'                                                  
         BE    DSCR30                                                           
                                                                                
         MVC   MYKEY,SPACES                                                     
         MVC   MYKEY(1),COMPANY                                                 
         MVC   MYKEY+1(2),=C'SJ'                                                
         MVC   MYKEY+3(3),0(R5)                                                 
         OC    MYKEY+3(3),SPACES                                                
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCFIL',MYKEY,IO2                     
         CLI   8(R1),0                                                          
         BE    DSCR12                                                           
         FOUT  SCRNAM1H,SPACES,34                                               
         MVI   ERROR,INVALID                                                    
         B     DSCRXIT                                                          
                                                                                
DSCR12   LA    R3,IO2                                                           
         AH    R3,DATADISP                                                      
DSCR15   CLI   0(R3),0                                                          
         BE    DSCR30                                                           
         CLI   0(R3),NAMELQ                                                     
         BE    DSCR20                                                           
         ZIC   RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     DSCR15                                                           
                                                                                
         USING NAMELD,R3                                                        
DSCR20   ZIC   R6,1(R3)                                                         
         SH    R6,=H'3'                                                         
         CH    R6,=H'33'                                                        
         BNH   DSCR22                                                           
         LH    R6,=H'33'                                                        
DSCR22   EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   SCRNAM1(0),NAMEREC                                               
         OI    SCRNAM1H+6,X'80'                                                 
                                                                                
DSCR30   LA    R5,3(R5)                                                         
         LA    R2,SCRENTL(R2)      BUMP TO NEXT LINE ON SCREEN                  
         BCT   R4,DSCR10           PROCESS NEXT                                 
                                                                                
DSCRXIT  B     XXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
*-----------------------------------------------------------------*             
*        UPDATE THE RECORD FROM THE TABLE                                       
*-----------------------------------------------------------------*             
UPDREC   NTR1                                                                   
         CLC   LOGANAM,SPACES                                                   
         BH    UPDR05                                                           
         LA    R2,LOGANAMH                                                      
         MVI   ERROR,MISSING                                                    
         B     XXIT                                                             
                                                                                
UPDR05   DS    0H                                                               
         MVI   ERROR,X'FF'       INITIALIZE ERROR TO NO ERROR                   
         USING ACTRECD,R3                                                       
         LA    R3,MYKEY                                                         
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,COMPANY                                                  
         MVI   ACTKUNT,C'F'                                                     
         MVC   ACTKLDG,LOGLEDG                                                  
         ZIC   R5,LOGACCTH+5                                                    
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   ACTKACT(0),LOGACCT                                               
         MVI   DMINBTS,X'88'                                                    
         MVC   COMMAND,=C'DMREAD  '                                             
         GOTO1 DATAMGR,DMCB,(DMINBTS,COMMAND),=C'ACCFIL',MYKEY,IO2              
         CLI   8(R1),X'00'                                                      
         BE    UPDR10                                                           
         CLI   8(R1),X'02'                                                      
         BE    UPDR10                                                           
                                                                                
**       CLC   LOGANAM,SPACES                                                   
**       BH    UPDR05                                                           
**       LA    R2,LOGANAMH                                                      
**       MVI   ERROR,MISSING                                                    
**       B     XXIT                                                             
                                                                                
         LA    RE,IO2                                                           
         LA    RF,2000                                                          
         XCEFL                                                                  
                                                                                
         LA    R3,IO2                                                           
         USING ACTRECD,R3                                                       
         MVC   COMMAND,=C'DMADD   '                                             
         MVC   0(L'MYKEY,R3),MYKEY                                              
         MVC   42(2,R3),DATADISP                                                
                                                                                
**       XC    ELEMENT,ELEMENT                                                  
**       USING NAMELD,R4                                                        
**       LA    R4,ELEMENT                                                       
**       MVI   NAMEL,NAMELQ                                                     
**       ZIC   R5,LOGANAMH+5                                                    
**       LA    R5,2(R5)                                                         
**       STC   R5,NAMLN                                                         
**       SH    R5,=H'3'                                                         
**       EX    R5,*+8                                                           
**       B     *+10                                                             
**       MVC   NAMEREC(0),LOGANAM                                               
**       GOTO1 ADDANEL                                                          
                                                                                
         XC    ELEMENT,ELEMENT        BUILD NEW STATUS ELEMENT AND              
         USING RSTELD,R4              ADD TO RECORD                             
         LA    R4,ELEMENT                                                       
         MVI   RSTEL,RSTELQ                                                     
         MVI   RSTLN,RSTLN3Q                                                    
         MVI   RSTFILT4,C' '                                                    
         GOTO1 DATCON,DMCB,(5,0),(1,RSTBDATE)                                   
         MVC   RSTTDATE,RSTBDATE                                                
         MVI   RSTFILT3,C' '                                                    
         MVI   RSTFILT1,C' '                                                    
         MVI   RSTFILT2,C' '                                                    
         MVI   RSTCOSTG,C' '                                                    
         MVI   RSTFILT5,C' '                                                    
         GOTO1 ADDANEL                                                          
                                                                                
UPDR10   DS    0H                                                               
         LA    R3,IO2                 ACCOUNT RECORD                            
         NI    44(R3),X'FF'-X'80'                                               
         MVI   ELCODE,ABLELQ                                                    
         BAS   RE,GETEL                                                         
         BNE   *+8                                                              
         MVI   0(R3),X'FF'                                                      
                                                                                
         LA    R3,IO2                 ACCOUNT RECORD                            
         MVI   ELCODE,NAMELQ                                                    
         BAS   RE,GETEL                                                         
         BNE   *+8                                                              
         MVI   0(R3),X'FF'                                                      
         GOTO1 REMANEL                                                          
                                                                                
         XC    ELEMENT,ELEMENT                                                  
         USING NAMELD,R4                                                        
         LA    R4,ELEMENT                                                       
         MVI   NAMEL,NAMELQ                                                     
         ZIC   R5,LOGANAMH+5                                                    
         LA    R5,2(R5)                                                         
         STC   R5,NAMLN                                                         
         SH    R5,=H'3'                                                         
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   NAMEREC(0),LOGANAM                                               
         GOTO1 ADDANEL                                                          
                                                                                
         LA    R3,IO2                 ACCOUNT RECORD                            
         MVI   ELCODE,FFTELQ                                                    
         BAS   RE,GETEL                                                         
         BNE   UPDR15                 IF NO ELEMENTS ON RECORD                  
         USING FFTELD,R3                                                        
UPDR12   CLI   FFTTYPE,FFTTCLIC       IS IT TYPE CLIENT GROUP                   
         BNE   *+8                                                              
         MVI   0(R3),X'FF'                                                      
         BAS   RE,NEXTEL              GET NEXT CLIENT GROUP ELEMENT             
         BE    UPDR12                                                           
UPDR15   GOTO1 REMANEL                                                          
         DROP  R3                                                               
                                                                                
         LA    R3,IO2                 REMANEL DOESN'T UPDATE RECORD             
         LA    R3,49(R3)              LENGTH - SO I'M DOING IT HERE             
         LA    R5,49                                                            
UPDR23   CLI   0(R3),0                                                          
         BE    UPDR24                                                           
         ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         AR    R5,R0                                                            
         B     UPDR23                                                           
UPDR24   LA    R5,1(R5)               END OF RECORD MARKER                      
         LA    RF,IO2                                                           
         STCM  R5,3,42(RF)                                                      
                                                                                
         XC    CLICNT,CLICNT                                                    
         SR    R4,R4                                                            
         LA    R5,CLIBLOCK                                                      
         LA    R6,MAXCLIS                                                       
UPDR26   OC    0(1,R5),0(R5)                                                    
         BZ    UPDR27                                                           
         CLI   0(R5),X'FF'                                                      
         BE    UPDR27                                                           
         LA    R4,1(R4)                                                         
         STCM  R4,3,CLICNT                                                      
         LA    R5,3(R5)                                                         
         BCT   R6,UPDR26                                                        
                                                                                
UPDR27   GOTO1 CALLOV,DMCB,0,X'D900A12'    GET ADDR OF XSORT                    
         L     RF,DMCB                                                          
         LH    R6,CLICNT                                                        
         CH    R6,=H'1'                                                         
         BL    UPDR55                                                           
         BNH   UPDR30                                                           
         LA    R5,CLIBLOCK                                                      
         GOTO1 (RF),DMCB,(0,(R5)),(R6),3,3,0                                    
                                                                                
UPDR30   DS    0H                                                               
         XC    CLILAST,CLILAST                                                  
         MVI   SEQLAST,0                                                        
         LA    R5,CLIBLOCK                                                      
                                                                                
         USING FFTELD,R4                                                        
UPDR32   XC    ELEMENT,ELEMENT                                                  
         LA    R4,ELEMENT                                                       
         MVI   FFTEL,FFTELQ                                                     
         MVI   FFTLN,5                                                          
         MVI   FFTTYPE,FFTTCLIC                                                 
         MVC   FFTSEQ,SEQLAST                                                   
         MVI   FFTDLEN,0                                                        
         LA    R6,FFTDATA                                                       
UPDR35   CLC   0(3,R5),=C'***'                                                  
         BE    UPDR45                                                           
         CLC   0(3,R5),CLILAST                                                  
         BE    UPDR45                                                           
                                                                                
UPDR40   MVC   0(3,R6),0(R5)                                                    
         OC    0(3,R6),SPACES                                                   
         MVC   CLILAST,0(R5)                                                    
         LA    R6,3(R6)                                                         
         ZIC   R7,FFTLN                                                         
         LA    R7,3(R7)                                                         
         STC   R7,FFTLN                                                         
         ZIC   R7,FFTDLEN                                                       
         LA    R7,3(R7)                                                         
         STC   R7,FFTDLEN                                                       
UPDR45   LA    R5,3(R5)                                                         
         CLI   0(R5),X'FF'                                                      
         BE    UPDR50                                                           
         OC    0(1,R5),0(R5)                                                    
         BZ    UPDR50                                                           
         CLI   FFTLN,253                                                        
         BL    UPDR35                                                           
         GOTO1 ADDANEL                                                          
         ZIC   R7,SEQLAST                                                       
         LA    R7,1(R7)                                                         
         STC   R7,SEQLAST                                                       
         B     UPDR32                                                           
                                                                                
UPDR50   LA    R4,ELEMENT                                                       
         CLI   FFTDLEN,0                                                        
         BNH   UPDR55                                                           
         GOTO1 ADDANEL                                                          
         DROP  R4                                                               
                                                                                
UPDR55   CLC   COMMAND,=C'DMADD   '                                             
         BE    *+10                                                             
         MVC   COMMAND,=C'DMWRT   '                                             
         GOTO1 DATAMGR,DMCB,COMMAND,=CL8'ACCFIL',IO2,IO2                        
         CLI   8(R1),0                UPDATE RECORD                             
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         CLI   LOGACT,C'N'                                                      
         BNE   UPDR70                                                           
         MVC   LOGACT,=C'AMEND     '                                            
         OI    LOGACTH+6,X'80'                                                  
                                                                                
UPDR70   BAS   RE,BLDCTAB                                                       
UPDXIT   B     XXIT                                                             
         EJECT                                                                  
*-----------------------------------------------------------------*             
*        ANY CHANGES TO THIS SCREEN                                             
*-----------------------------------------------------------------*             
ANYCHA   NTR1                                                                   
         USING SCRND,R3                                                         
         LA    R3,LOGCLIH                                                       
         LA    R5,SCRNLINS                                                      
ANYC20   TM    SCRCLI1H+4,X'80'                                                 
         BZ    *+8                                                              
         OI    DISSW,DISCHA                                                     
         TM    SCRCLI2H+4,X'80'                                                 
         BZ    *+8                                                              
         OI    DISSW,DISCHA                                                     
         LA    R3,SCRLLEN(R3)                                                   
         BCT   R5,ANYC20                                                        
                                                                                
ANYCXIT  B     XXIT                                                             
         EJECT                                                                  
*-----------------------------------------------------------------*             
*        CLEAR THIS SCREEN                                                      
*-----------------------------------------------------------------*             
CLRSCRN  NTR1                                                                   
         USING SCRND,R3                                                         
         LA    R3,LOGCLIH                                                       
         LA    R5,SCRNLINS                                                      
CSCR10   XC    SCRCLI1,SCRCLI1                                                  
         OI    SCRCLI1H+6,X'80'                                                 
         MVC   SCRNAM1,SPACES                                                   
         OI    SCRNAM1H+6,X'80'                                                 
         XC    SCRCLI2,SCRCLI2                                                  
         OI    SCRCLI2H+6,X'80'                                                 
         MVC   SCRNAM2,SPACES                                                   
         OI    SCRNAM2H+6,X'80'                                                 
         LA    R3,SCRLLEN(R3)                                                   
         BCT   R5,CSCR10                                                        
         B     XXIT                                                             
         EJECT                                                                  
*-----------------------------------------------------------------*             
*        BUILD TABLE OF ALL CLIENTS ON THIS RECORD                              
*-----------------------------------------------------------------*             
BLDCTAB  NTR1                                                                   
         BAS   RE,CLRTAB              CLEAR CLIENT TABLE                        
                                                                                
         LA    R4,CLIBLOCK            CLIENT TABLE                              
         LA    R5,MAXCLIS             MAX ENTRIES IN CLIENT TABLE               
         LA    R3,IO2                 ACCOUNT RECORD                            
         MVI   ELCODE,FFTELQ                                                    
         BAS   RE,GETEL                                                         
         BNE   XXIT                   IF NO ELEMENTS ON RECORD                  
         USING FFTELD,R3                                                        
BLDC10   CLI   FFTTYPE,FFTTCLIC       IS IT TYPE CLIENT GROUP                   
         BNE   BLDC50                                                           
         LA    R6,FFTDATA             FREEFORM DATA (CLIENT TEXT)               
         ZIC   R7,FFTDLEN             LENGTH OF FREEFORM DATA                   
BLDC20   MVC   0(3,R4),0(R6)          MOVE CLIENT TO TABLE                      
         LA    R4,3(R4)               BUMP TABLE                                
         MVI   0(R4),X'FF'            MARK NEW END                              
         LA    R6,3(R6)               BUMP CLIENT TEXT                          
         BCTR  R5,0                   MAX COUNTER                               
         LTR   R5,R5                                                            
         BNZ   *+6                                                              
         DC    H'0'                   TABLE IS FULL                             
         SH    R7,=H'3'               PROCESS UNTIL FFTDLEN = 0                 
         LTR   R7,R7                                                            
         BNZ   BLDC20                                                           
                                                                                
BLDC50   BAS   RE,NEXTEL              GET NEXT CLIENT GROUP ELEMENT             
         BE    BLDC10                                                           
         B     XXIT                                                             
         EJECT                                                                  
*-----------------------------------------------------------------*             
*        UPDATE TABLE ENTRY WITH SCREEN CHANGES                                 
*-----------------------------------------------------------------*             
UPDCTAB  NTR1                                                                   
         LA    R5,CLIBLOCK                                                      
         ZIC   R6,SCRNUM           RESET CURRENT SCREEN DISPLAY NUMBER          
         MH    R6,=H'90'                                                        
         AR    R5,R6                                                            
         USING SCRND,R3                                                         
         LA    R3,LOGCLIH                                                       
         LA    R4,SCRNLINS                                                      
                                                                                
UPDC05   DS    0H                                                               
         MVC   0(3,R5),SCRCLI1                                                  
         MVC   3(3,R5),SCRCLI2                                                  
         LA    R3,SCRLLEN(R3)                                                   
         LA    R5,6(R5)                                                         
         BCT   R4,UPDC05                                                        
                                                                                
         LA    R5,CLIBLOCK                                                      
         ZIC   R6,SCRNUM           RESET CURRENT SCREEN DISPLAY NUMBER          
         MH    R6,=H'90'                                                        
         AR    R5,R6                                                            
         BAS   RE,DSPSCRN                                                       
         B     XXIT                                                             
         EJECT                                                                  
*-----------------------------------------------------------------*             
*        CLEAR KEY TABLE ENTRIES                                                
*-----------------------------------------------------------------*             
CLRTAB   NTR1                                                                   
         LA    R4,CLIBLOCK                                                      
         LA    R3,MAXCLIS                                                       
CLR10    XC    0(2,R4),0(R4)                                                    
         LA    R4,3(R4)                                                         
         BCT   R3,CLR10                                                         
         LA    R4,CLIBLOCK                                                      
         MVI   0(R4),X'FF'                                                      
         MVI   CLITEND,X'FF'                                                    
         B     XXIT                                                             
                                                                                
XXIT     XIT1  REGS=(R2)                                                        
                                                                                
         GETEL R3,DATADISP,ELCODE                                               
         EJECT                                                                  
*-----------------------------------------------------------------*             
*        LITERAL POOL                                                           
*-----------------------------------------------------------------*             
                                                                                
MAXCLIS  EQU   614                 NUMBER OF ENTRIES KEPT IN TABLE              
SCRNLINS EQU   15                  LINES DISPLAYED PER SCREEN                   
SCRNENTS EQU   30                  CLIENTS DISPLAYED PER SCREEN                 
SCRNMAX  EQU   20                  (BASED ON ZERO AS FIRST SCREEN)              
         LTORG                                                                  
         EJECT                                                                  
*-----------------------------------------------------------------*             
*        DSECT FOR LOCAL W/S                                                    
*-----------------------------------------------------------------*             
LWSD     DSECT                                                                  
PRELOC   DS    F                                                                
SAVRB    DS    F                   ENTRY POINT                                  
ELCODE   DS    CL1                                                              
MYKEY    DS    CL42                                                             
MYKEY2   DS    CL42                                                             
SVACCREM DS    CL14                                                             
LWSX     DS    0C                                                               
         EJECT                                                                  
       ++INCLUDE ACLFMFFD                                                       
         ORG   LOGTABH                                                          
       ++INCLUDE ACLFMCFD                                                       
                                                                                
DISSW    DC    XL1'00'                                                          
DISCHA   EQU   X'80'                                                            
                                                                                
SCRNUM   DC    XL1'00'                                                          
CLICNT   DC    H'0'                                                             
CLILAST  DC    CL3' '                                                           
SEQLAST  DC    XL1'0'                                                           
SCRNLEN  DC    H'90'                                                            
                                                                                
SVNAMEL  DS    CL38                                                             
BYTE     DS    CL1                                                              
TSIDHEIR DS    CL66                                                             
MINLEN   DS    CL1                                                              
TEMPHEIR DS    CL66                                                             
SVLGACC  DS    CL14                                                             
                                                                                
CLIBLOCK DS    0C                                                               
         DC    30CL3' '                                                         
SCRN1    EQU   *-CLIBLOCK                                                       
         DC    584CL3' '                                                        
CLITEND  DC    X'FF'                                                            
                                                                                
         EJECT                                                                  
                                                                                
SCRND    DSECT                                                                  
SCRCLI1H DS    CL8                                                              
SCRCLI1  DS    CL3                                                              
SCRNAM1H DS    CL8                                                              
SCRNAM1  DS    CL34                                                             
SCRENTL  EQU   *-SCRND                                                          
SCRCLI2H DS    CL8                                                              
SCRCLI2  DS    CL3                                                              
SCRNAM2H DS    CL8                                                              
SCRNAM2  DS    CL34                                                             
SCRLLEN  EQU   *-SCRND                                                          
                                                                                
                                                                                
       ++INCLUDE ACLFMWORK                                                      
         EJECT                                                                  
         PRINT OFF                                                              
*        ACGENBOTH                                                              
       ++INCLUDE ACGENBOTH                                                      
*        ACGENFILE                                                              
       ++INCLUDE ACGENFILE                                                      
         EJECT                                                                  
       ++INCLUDE ACLFMEQU                                                       
*        ACLFMEQU                                                               
       ++INCLUDE DDFLDIND                                                       
*        DDFLDIND                                                               
       ++INCLUDE DDCOMFACS                                                      
*        DDCOMFACS                                                              
       ++INCLUDE ACVATICAND                                                     
*        ACVATICAND                                                             
       ++INCLUDE DDFLDHDR                                                       
*        DDFLDHDR                                                               
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'030ACLFM37   05/01/02'                                      
         END                                                                    
