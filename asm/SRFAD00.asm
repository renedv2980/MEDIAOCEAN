*          DATA SET SRFAD00    AT LEVEL 003 AS OF 05/01/02                      
*PHASE T15E00A                                                                  
         PRINT NOGEN                                                            
         TITLE '$DS (T15E00) -- DISPLAY FACPAK DSECTS'                          
*                                                                               
* THIS IS AN ONLINE PROGRAM TO DISPLAY FADSECTS.                                
*                                                                               
SRFAD00  CSECT                                                                  
         NMOD1 WORKAEND-WORKAREA,*$DS*,R8,RR=R3                                 
         USING WORKAREA,RC                                                      
         L     R9,0(R1)                                                         
         USING SYSFACD,R9          A(SYSFACS)                                   
         L     RA,20(R1)                                                        
         USING T15EFFD,RA          A(TWA)                                       
         L     R6,12(R1)                                                        
         USING COMFACSD,R6                                                      
         MVC   VHEXIN,CHEXIN                                                    
         MVC   VHEXOUT,CHEXOUT                                                  
         MVC   VGETFACT,CGETFACT                                                
         DROP  R6                                                               
*                                                                               
* CHECK SHORT TABLE OF DSECTS                                                   
*                                                                               
         CLI   SRVFD1H+5,0         1ST FIELD EMPTY?                             
         BE    NOSTLST             FORGET ABOUT SHORT LIST                      
         LA    R7,SHRTLIST                                                      
         MVC   PADDER,=CL16' '                                                  
         ZIC   R4,SRVFD1H+5                                                     
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   PADDER(0),SRVFD1                                                 
         MVC   SRVFD1,PADDER                                                    
LSTLP    CLI   0(R7),X'00'         END OF SHORT LIST                            
         BE    NOSTLST             YES, STAY IN CURRENT SYSTEM                  
         CLC   0(8,R7),SRVFD1      IS FIELD 1 IN SHORT LIST                     
         BE    FROMLST                                                          
         LA    R7,9(R7)                                                         
         B     LSTLP                                                            
FROMLST  MVC   CURSYS,8(R7)        8TH BYTE HOLDS SYSTEM NUMBER                 
         B     GSYSNAME                                                         
*                                                                               
* CHECK IF USER WANTS NEW SYSTEM                                                
*                                                                               
NOSTLST  CLI   SRVFD2H+5,0                                                      
         BE    GETSYS                                                           
         B     GETSYSNO                                                         
GETSYS   L     R6,8(R1)                                                         
         USING UTLD,R6             A(UTL)                                       
         MVC   CURSYS,TOVSYS                                                    
         DROP  R6                                                               
GSYSNAME LA    R6,SYSLST+6                                                      
         USING SYSLSTD,R6                                                       
NXTSYS   CLI   SYSLNUM,X'0'                                                     
         BE    INVSYS                                                           
         CLC   SYSLNUM,CURSYS                                                   
         BE    FNDSYS                                                           
         LA    R6,SYSLLEN(R6)                                                   
         B     NXTSYS                                                           
INVSYS   MVI   CURSYS,X'0'                                                      
         MVC   SRVFD2,=CL16'FACPAK'                                             
         B     PSTSYS                                                           
FNDSYS   MVC   SRVFD2,=CL16'                '                                   
         MVC   SRVFD2(7),SYSLNAME                                               
         DROP  R6                                                               
         B     PSTSYS                                                           
*                                                                               
* GET THE SYSTEM NUMBER OF THE SYSTEM REQUESTED                                 
*                                                                               
GETSYSNO LA    R6,SYSLST+6                                                      
         USING SYSLSTD,R6                                                       
GSYSNOLP CLI   SYSLNUM,X'0'        END OF SYSTEM LIST?                          
         BNE   NOTEOLST            NO, NOT YET                                  
         MVI   CURSYS,X'0'         YES, SYSTEM IS FACPAK                        
         MVC   SRVFD2,=CL16'FACPAK'                                             
         B     PSTSYS                                                           
NOTEOLST ZIC   R4,SRVFD2H+5                                                     
         CH    R4,=H'8'                                                         
         BL    SYSLNOK                                                          
         LA    R4,7                                                             
SYSLNOK  BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   SYSLNAME(0),SRVFD2  COMPARE SYSTEM NAMES                         
         BE    FNDSYSNM                                                         
         LA    R6,SYSLLEN(R6)      NEXT SYSTEM                                  
         B     GSYSNOLP                                                         
FNDSYSNM MVC   CURSYS,SYSLEQU      SAVE SYSTEM NUMBER                           
         MVC   SRVFD2,=CL16' '     CLEAR 2ND FIELD                              
         MVC   SRVFD2(7),SYSLNAME  SAVE SYSTEM NAME                             
*                                                                               
*                                                                               
*                                                                               
         DROP  R6                                                               
PSTSYS   OI    SRVREQH+6,X'81'     INPUT EVERY TIME                             
         OI    SRVFD1H+6,X'81'     INPUT EVERY TIME                             
         OI    SRVFD2H+6,X'81'     INPUT EVERY TIME                             
         OI    SRVFD3H+6,X'81'     INPUT EVERY TIME                             
         OI    SRVFD4H+6,X'81'     INPUT EVERY TIME                             
         NI    SRVREQH+6,X'BF'     DON'T PUT CURSOR ON SERV REQ                 
         NI    SRVFD2H+6,X'BF'       AND 1ST FIELD                              
         NI    SRVFD3H+6,X'BF'       AND 1ST FIELD                              
         NI    SRVFD4H+6,X'BF'       AND 3RD FIELD                              
         OI    SRVFD1H+6,X'40'     POSITION CURSOR HERE                         
         MVC   LASTDSEC,=CL16' '                                                
*                                                                               
* THIS PART CHECKS THE NUMBER OF COLUMNS                                        
* IF IT IS NOT 2 THEN EVERYTHING ELSE WOULD MEAN 1                              
*                                                                               
         CLI   SRVFD3,C'2'                                                      
         BE    TWOCOL                                                           
         CLI   SRVFD3,C'?'                                                      
         BE    ONECOL                                                           
         MVC   PADDER,=CL16'                '                                   
         MVI   PADDER,C'1'                                                      
         MVC   SRVFD3,PADDER                                                    
         MVI   PADDER,C' '                                                      
ONECOL   MVI   COLS,X'1'                                                        
         MVC   SRVTL1+19(40),=C' COMMENT                               X        
                '                                                               
         MVC   SRVTL2+19(40),=C' --------------------------------------X        
               -'                                                               
         B     BCHKFD1                                                          
TWOCOL   MVI   COLS,X'2'                                                        
         MVC   SRVTL1,=C'TYPE               '                                   
         MVC   SRVTL2,=C'-------------------'                                   
         MVC   SRVTL1+19(40),=C' | DISP LABEL    DEF TYPE              X        
                '                                                               
         MVC   SRVTL2+19(40),=C' | ---- -------- --- ------------------X        
               -'                                                               
BCHKFD1  BAS   RE,NEWMEMBR         LOAD IN FIRST OVERLAY                        
         EJECT                                                                  
CHKFD1   CLI   SRVFD1H+5,0         CHECK IF NO MEMBER INPUTTED                  
         BNE   NO1                                                              
         CLI   SRVFD3,C'?'         NO ERRORS FOR SHOWING DSECTS ONLY            
         BE    NO1                                                              
         LA    R4,ERROR0           ADDRESS OF ERROR MESSAGE                     
         B     ERROR                                                            
NO1      NI    SRVFD1H+6,X'BF'     PLACE CURSOR ON NEXT PLACE                   
         OI    SRVFD4H+6,X'40'       3RD FIELD                                  
         CLI   SRVFD4H+5,0         CHECK IF 4TH FIELD IS EMPTY                  
         BNE   FNDLBL4                                                          
         CLI   SRVFD1H+5,X'0'                                                   
         BE    NODISP                                                           
         MVC   LABELNM,SRVFD1      SAVE LABEL FOR SEARCHING                     
         MVC   LABELLN,SRVFD1H+5   SAVE THE LENGTH TOO                          
         MVI   CKHDRLBL,X'1'       CHECK FOR DATA SET NAMES                     
         MVI   LBLNDSP,C'N'                                                     
         B     FINDLABL                                                         
FNDLBL4  ZIC   R4,SRVFD4H+5                                                     
         GOTO1 VHEXIN,DMCB,SRVFD4,HEXDSP,(R4)                                   
         L     R4,DMCB+12                                                       
         CH    R4,=H'0'            VALID HEX NUMBER?                            
         BE    NOTHEXNO            NO, USE FIELD 4 AS LABEL                     
         L     R5,HEXDSP           PAD ZEROS LEFT OF NUMBER                     
         LA    R8,4                4 DIGITS MAXIMUM                             
         ZIC   R4,SRVFD4H+5        LESS NUMBER OF DIGITS INPUTTED               
         SR    R8,R4                                                            
         CH    R8,=H'1'            4 DIGITS INPUTTED?                           
         BL    HEXNOOK             DON'T PAD ZEROS                              
SHFTMORE SRL   R5,4                SHIFT NUMBER BY A NIBBLE                     
         BCT   R8,SHFTMORE                                                      
         ST    R5,HEXDSP                                                        
HEXNOOK  CLC   =X'1000',HEXDSP     IF THE DISP IS > X'1000'                     
         BL    NOTHEXNO            USE IT AS A LABEL                            
         LA    R4,2                                                             
         STC   R4,HEXDSPLN                                                      
         MVI   LBLNDSP,C'Y'                                                     
         MVC   LABELNM,SRVFD1      SAVE LABEL FOR SEARCHING                     
         MVC   LABELLN,SRVFD1H+5   SAVE THE LENGTH TOO                          
         MVI   CKHDRLBL,X'1'       CHECK FOR DATA SET NAMES                     
         B     FINDLABL                                                         
NEEDDISP MVI   LBLNDSP,C'N'                                                     
         MVC   LABELNM,HEXDSP      SAVE LABEL FOR SEARCHING                     
         MVC   LABELLN,HEXDSPLN    SAVE THE LENGTH TOO                          
         B     FINDDISP                                                         
NOTHEXNO MVC   LABELNM,SRVFD4      SAVE LABEL FOR SEARCHING                     
         MVC   LABELLN,SRVFD4H+5   AND ITS LENGTH                               
         MVI   CKHDRLBL,X'0'       DON'T CHECK DATA SET NAMES                   
         MVI   LBLNDSP,C'N'                                                     
         B     FINDLABL                                                         
         EJECT                                                                  
********************************************                                    
* SUBROUTINE NEWMEMBR                      *                                    
* ---------------------------------------- *                                    
* USES CALLOV TO LOAD IN A NEW OVERLAY     *                                    
*   INPUT:   CURSYS = SYSTEM NUMBER        *                                    
*          OVLCOUNT = OVERLAY COUNTER      *                                    
*  OUTPUT:       R4 = RESULT AFTER LOADING *                                    
*                     X'FF' ERROR LOADING  *                                    
********************************************                                    
NEWMEMBR NTR1                                                                   
* TO LOAD OTHERS, CHANGE ^^^^^^^^^\/ THIS BYTE (DMCB+7)                         
         MVC   DMCB+4(4),=X'D9015E01'                                           
*                                /  \                                           
*                       SYSTEM NO.   OVERLAY NO.                                
*                                                                               
         ZIC   R4,CURSYS           CALCULATE THE OVERLAY TO LOAD                
         SLL   R4,4                THE SYSTEM # IS HIGH NIBBLE                  
         ZIC   R0,OVLCOUNT         THE OVERLAY COUNTER IS LOW NIBBLE            
         AR    R4,R0                                                            
         STC   R4,DMCB+7           PUT IT IN DMCB                               
         LA    RF,TESTDATA         LOAD OVERLAY AT TESTDATA                     
         ST    RF,DMCB             WHICH IS END OF PROGRAM                      
         L     RF,VCALLOV                                                       
         GOTO1 (RF),DMCB                                                        
         ZIC   R4,DMCB+4           R4 HOLD RESULT CODE X'FF' IS ERROR           
         L     RF,DMCB                                                          
         ST    RF,DSECDATA                                                      
         XIT1  REGS=(R4)           SAVE R4                                      
         EJECT                                                                  
************************************************                                
* SET R4 TO THE ADDRESS OF THE MATCHING RECORD *                                
************************************************                                
NODISP   MVI   OVLCOUNT,X'01'                                                   
         CLI   CURSYS,X'0'                                                      
         BE    NOREOVLS                                                         
         MVI   OVLCOUNT,X'00'                                                   
NOREOVLS CLI   OVLCOUNT,X'10'      INTO NEXT SYSTEM?                            
         BL    NOADOVL             NO, LOAD OVERLAY                             
         LA    R4,ERROR1                                                        
         B     ERROR                                                            
NOADOVL  BAS   RE,NEWMEMBR                                                      
         CH    R4,=H'255'          ERROR IN LOADING?                            
         BNE   LOVLOK              OVERLAY LOADED OK                            
         ZIC   R4,OVLCOUNT                                                      
         LA    R4,1(R4)                                                         
         STC   R4,OVLCOUNT                                                      
         B     NOREOVLS                                                         
LOVLOK   L     R4,DSECDATA         NO DISPL, DEFAULT TO ZERO                    
         NI    SRVFD4H+6,X'BF'     PLACE CURSOR ON NEXT PLACE                   
         OI    SRVFD5H+6,X'40'       5TH FIELD                                  
         MVC   SRVFD1,=C'                '                                      
         B     SHOWTHEM                                                         
         SPACE 4                                                                
********************************************                                    
* FIND A LABEL IN THE OVERLAY AT DSECDATA  *                                    
*   IF THERE IS A MATCH SAVE ADDRESS IN R4 *                                    
*   IF NOT, SHOULD LOAD NEXT OVERLAY       *                                    
*           NOW JUST GIVE ERROR            *                                    
********************************************                                    
FINDLABL MVI   OVLCOUNT,X'01'                                                   
         CLI   CURSYS,X'0'                                                      
         BE    MOREOVLS                                                         
         MVI   OVLCOUNT,X'00'                                                   
MOREOVLS CLI   OVLCOUNT,X'10'      INTO NEXT SYSTEM?                            
         BL    LOADOVL             NO, LOAD OVERLAY                             
         LA    R4,ERROR1                                                        
         B     ERROR                                                            
LOADOVL  BAS   RE,NEWMEMBR                                                      
         CH    R4,=H'255'          ERROR IN LOADING?                            
         BE    NXTOVL1             LOAD IN NEXT OVERLAY                         
         L     R7,DSECDATA         GET ADDRESS OF DATA                          
         USING SRFADSEC,R7                                                      
LAGAIN2  CLC   =X'FFFF00',SRFADDSP   END OF SYSTEM?                             
         BNE   EOFILE                                                           
         LA    R4,ERROR1                                                        
         B     ERROR                                                            
EOFILE   CLC   =X'FFFF01',SRFADDSP   END OF OVERLAY?                            
         BNE   CHKHDR                                                           
NXTOVL1  ZIC   R4,OVLCOUNT         SEE IF NEXT OVERLAY HAS IT                   
         LA    R4,1(R4)            INCREMENT OVERLAY COUNTER                    
         STC   R4,OVLCOUNT         SAVE OVERLAY COUNTER                         
         B     MOREOVLS                                                         
CHKHDR   CLC   =X'FFFFFF',SRFADDSP   HEADER RECORD?                             
         BNE   CHKLBL                                                           
         MVI   MISSDSEC,C'Y'       NO DSECT IN DATA SET                         
*                                                                               
* SAVE MEMBER NAME AND SYSTEM NAME FOR LATER                                    
*                                                                               
         CLI   CKHDRLBL,X'0'       CHECK DATA SET NAME?                         
         BE    NOHDRLBL                                                         
         ZIC   R4,LABELLN          LENGTH OF SEARCH LABEL                       
         CH    R4,=H'11'                                                        
         BL    REALLEN0                                                         
         LA    R4,10                                                            
REALLEN0 BCTR  R4,0                MINUS 1 (TRUE LENGTH FOR MVC)                
         EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   LABELNM(0),SRFADLBL+1                                            
         BNE   NOHDRLBL                                                         
         CLI   SRVFD3,C'?'                                                      
         BE    DONTWANT            DON'T SAVE LABEL                             
         MVC   SRVFD1(10),SRFADLBL+1                                            
DONTWANT LA    R7,13(R7)                                                        
         B     FOUNDLBL                                                         
NOHDRLBL CLI   SRVFD3,C'?'                                                      
         BE    *+10                                                             
         MVC   SRVFD1(10),SRFADLBL+1                                            
         LA    R7,13(R7)                                                        
         B     LAGAIN2                                                          
CHKLBL   CLI   SRFADDEF,C'D'       SAVE DSECT FOR LATER                         
         BNE   NOPE1                                                            
         MVC   LASTDSEC,=CL16' '                                                
         MVC   LASTDSEC(8),SRFADLBL                                             
         MVI   MISSDSEC,C'N'       DATA SET HAS DSECT                           
NOPE1    ZIC   R4,LABELLN          LENGTH OF SEARCH LABEL                       
         CH    R4,=H'9'                                                         
         BL    REALLEN                                                          
         LA    R4,8                                                             
REALLEN  BCTR  R4,0                MINUS 1 (TRUE LENGTH FOR MVC)                
         EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   LABELNM(0),SRFADLBL                                              
         BE    FOUNDLBL                                                         
CALCDISP LA    R6,SRFADTYP                                                      
         ZIC   R4,SRFADTYP         NEXT DISPLACEMENT                            
         AR    R6,R4               SHOULD BE ONE FOR LENGTH BYTE                
         LA    R6,1(R6)            AND THE TYPE                                 
         ZIC   R4,0(R6)            AND THE COMMENT LENGTH                       
         AR    R6,R4               AND THE COMMENT                              
         LA    R6,1(R6)                                                         
         LR    R7,R6                                                            
         B     LAGAIN2                                                          
FOUNDLBL LR    R4,R7               *ORIGINAL <FOUNDLBL>                         
         NI    SRVFD4H+6,X'BF'     PLACE CURSOR ON NEXT PLACE                   
         OI    SRVFD5H+6,X'40'       3RD FIELD                                  
         B     SHOWTHEM                                                         
         EJECT                                                                  
***************************************************                             
* FIND A DISPLACEMENT IN THE OVERLAY AT R4        *                             
*   IF THERE IS A MATCH SAVE ADDRESS IN R4        *                             
*   IF NOT, SHOULD LOAD NEXT OVERLAY              *                             
*           NOW JUST GIVE ERROR                   *                             
***************************************************                             
FINDDISP LR    R7,R4               GET ADDRESS OF DATA                          
         B     DAGAIN2             FIRST TIME, DON'T LOAD OVERLAY               
DOREOVLS CLI   OVLCOUNT,X'10'      INTO NEXT SYSTEM?                            
         BL    LOADOVLD            NO, LOAD OVERLAY                             
         LA    R4,ERROR2                                                        
         B     ERROR                                                            
LOADOVLD BAS   RE,NEWMEMBR                                                      
         CH    R4,=H'255'          ERROR IN LOADING?                            
         BE    NXTOVL2             LOAD IN NEXT OVERLAY                         
         L     R7,DSECDATA         GET ADDRESS OF DATA                          
         USING SRFADSEC,R7                                                      
DAGAIN2  CLC   =X'FFFF00',SRFADDSP   END OF SYSTEM?                             
         BNE   DOFILE                                                           
         LA    R4,ERROR2                                                        
         B     ERROR                                                            
DOFILE   CLC   =X'FFFF01',SRFADDSP   END OF OVERLAY?                            
         BNE   CHKHDRD                                                          
NXTOVL2  ZIC   R4,OVLCOUNT         SEE IF NEXT OVERLAY HAS IT                   
         LA    R4,1(R4)            INCREMENT OVERLAY COUNTER                    
         STC   R4,OVLCOUNT         SAVE OVERLAY COUNTER                         
         B     DOREOVLS                                                         
CHKHDRD  CLC   =X'FFFFFF',SRFADDSP   HEADER RECORD?                             
         BNE   CHKDSPD                                                          
         LA    R4,ERROR2           YES, NO LABEL IN DATA                        
         B     ERROR                                                            
CHKDSPD  CLI   SRFADDEF,C'D'       SAVE DSECT FOR LATER                         
         BNE   NOPE1D                                                           
         MVC   LASTDSEC,=CL16' '                                                
         MVC   LASTDSEC(8),SRFADLBL                                             
         MVI   MISSDSEC,C'N'       DATA SET HAS DSECT                           
NOPE1D   ZIC   R4,LABELLN          LENGTH OF SEARCH LABEL                       
         CH    R4,=H'9'                                                         
         BL    REALLEND                                                         
         LA    R4,8                                                             
REALLEND BCTR  R4,0                MINUS 1 (TRUE LENGTH FOR MVC)                
         EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   LABELNM(0),SRFADDSP                                              
         BE    FOUNDDSP            STOP IF EQUAL                                
         BL    FOUNDDSP            OR PASSED DISPLACEMENT                       
CALCDSPD LA    R6,SRFADTYP                                                      
         ZIC   R4,SRFADTYP         NEXT DISPLACEMENT                            
         AR    R6,R4               SHOULD BE ONE FOR LENGTH BYTE                
         LA    R6,1(R6)            AND THE TYPE                                 
         ZIC   R4,0(R6)            AND THE COMMENT LENGTH                       
         AR    R6,R4               AND THE COMMENT                              
         LA    R6,1(R6)                                                         
         LR    R7,R6                                                            
         B     DAGAIN2                                                          
FOUNDDSP LR    R4,R7                                                            
         NI    SRVFD4H+6,X'BF'     PLACE CURSOR ON NEXT PLACE                   
         OI    SRVFD5H+6,X'40'       3RD FIELD                                  
         B     SHOWTHEM                                                         
         EJECT                                                                  
**************************************                                          
* DISPLAY THE DSECT STARTING FROM R4 *                                          
**************************************                                          
SHOWTHEM MVI   FIRSTLN,C'Y'                                                     
         CLI   LBLNDSP,C'Y'                                                     
         BNE   STRTSHOW                                                         
         B     NEEDDISP                                                         
STRTSHOW LA    R3,SRVL01H                                                       
         USING FLDHDRD,R3                                                       
         LR    R7,R4                                                            
         USING SRFADSEC,R7         R4 HOLD THE ADDRESS OF DATA                  
         LA    R8,NLINES                                                        
OUTLP1   OI    FLDOIND,X'80'       TRANSMIT                                     
         CLI   COLS,2                                                           
         BNE   NODIVLN                                                          
         MVI   FLDLEN+46,C'|'      DIVIDING LINE                                
NODIVLN  MVI   FLDILEN,X'4F'       79 CHARACTERS                                
         LA    R3,87(R3)                                                        
         BCT   R8,OUTLP1                                                        
         LA    R3,SRVL01           FIRST LINE OF DSECTS                         
         USING PRLINE,R3                                                        
         SR    R8,R8               DO THIS TWICE                                
         STC   R8,ONCE                                                          
LOOP1    LA    R8,NLINES                                                        
LOOP2    MVC   PRNTDSP(38),=38C' '                                              
         MVI   SCRNFILD,C'N'                                                    
         CLC   =X'FFFF00',SRFADDSP   END OF SYSTEM?                             
         BNE   NOTEOD                                                           
         CLI   SRVFD3,C'?'                                                      
         BNE   STOPPRN                                                          
         MVC   PRNTCMT,=CL39' '                                                 
         MVC   PADDER,=CL16' '                                                  
         B     TOPAGAIN            YES, STOP PRINTING                           
NOTEOD   CLC   =X'FFFF01',SRFADDSP   END OF OVERLAY?                            
         BNE   NOTEOF                                                           
NXTOVL3  ZIC   R4,OVLCOUNT         SEE IF NEXT OVERLAY HAS IT                   
         LA    R4,1(R4)            INCREMENT OVERLAY COUNTER                    
         STC   R4,OVLCOUNT         SAVE OVERLAY COUNTER                         
         CLI   OVLCOUNT,X'10'      INTO NEXT SYSTEM?                            
         BL    LOADOVLS            NO, LOAD OVERLAY                             
         B     DATA2PRN                                                         
LOADOVLS BAS   RE,NEWMEMBR                                                      
         CH    R4,=H'255'          ERROR IN LOADING?                            
         BE    NXTOVL3             LOAD IN NEXT OVERLAY                         
         L     R7,DSECDATA         GET ADDRESS OF DATA                          
         B     LOOP2                                                            
NOTEOF   CLC   =X'FFFFFF',SRFADDSP   HEADER RECORD?                             
         BNE   NOTHEADR                                                         
         CLI   SRVFD3,C'?'                                                      
         BE    SKPHDRS                                                          
         CLI   SRVFD1,C' '                                                      
         BNE   DATA2PRN                                                         
*                                                                               
* SAVE MEMBER NAME AND SYSTEM NAME FOR LATER                                    
*                                                                               
         MVC   SRVFD1(10),SRFADLBL+1                                            
         LA    R7,13(R7)                                                        
         B     LOOP2                                                            
SKPHDRS  CLI   SRVFD1H+5,X'0'                                                   
         BNE   SH1HEADR                                                         
         LA    R7,13(R7)                                                        
         B     LOOP2                                                            
SH1HEADR MVC   PRNTCMT,=CL39' '                                                 
         MVC   PADDER,=CL16' '                                                  
         B     TOPAGAIN            YES, STOP PRINTING                           
NOTHEADR CLI   MISSDSEC,C'N'                                                    
         BNE   NOT2DSEC                                                         
         CLI   SRVFD3,C'?'                                                      
         BE    NOT2DSEC                                                         
         CLI   SRFADDEF,C'D'                                                    
         BNE   NOT2DSEC                                                         
         CLI   FIRSTLN,C'Y'                                                     
         BE    NOT2DSEC                                                         
*&&UK*&& B     NOT2DSEC            CONTINUE TO NEXT DSECT IN UK                 
*&&US*&& B     DATA2PRN            ORIGINAL US ACTION                           
NOT2DSEC MVI   FIRSTLN,C'N'                                                     
         MVC   PRNTLBL,SRFADLBL                                                 
         LA    R6,SRFADTYP                                                      
         CLI   SRFADTYP,0          ANY TYPE DEFINITION?                         
         BE    NOTYPE              NO, LOOK FOR COMMENTS                        
         MVC   PADTYPE,=CL19'                   '                               
         ZIC   R4,SRFADTYP         GET LENGTH OF TYPE                           
         BCTR  R4,0                LESS 1 FOR MVC                               
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   PADTYPE(0),SRFADTYP+1                                            
         CLI   COLS,1                                                           
         BE    SHOWTYPA                                                         
         MVC   PRNTTYP,PADTYPE     SHOW THE TYPE                                
         CLI   SRFADTYP,20                                                      
         BL    NEXTADD                                                          
         MVI   PRNTTYP+19,C'>'                                                  
         B     NEXTADD                                                          
SHOWTYPA EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   PRNTTYP(0),PADTYPE                                               
NEXTADD  LA    R4,1(R4)            RESTORE ORIGINAL LENGTH                      
         AR    R6,R4               NEXT ADDRESS, LESS 1                         
NOTYPE   LA    R6,1(R6)                                                         
         CLI   0(R6),0             ANY COMMENTS?                                
         BE    BCOLS_2             NO, CONTINUE WITH DEFINITIONS                
         CLI   SRFADTYP,20         SHOW TYPE BEFOR COMMENTS                     
         BL    COMMNTOK                                                         
         ZIC   R4,0(R6)                                                         
         BCTR  R4,0                                                             
         B     SKIPCMT                                                          
COMMNTOK MVC   PADCMNT,=CL39'                                       '           
         ZIC   R4,0(R6)            GET LENGTH OF COMMENT                        
         BCTR  R4,0                LESS 1 FOR MVC                               
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   PADCMNT(0),1(R6)                                                 
         CLI   COLS,X'1'                                                        
         BNE   SKIPCMT                                                          
         MVC   PRNTCMT,PADCMNT     SHOW THE COMMENT                             
SKIPCMT  LA    R4,1(R4)            RESTORE ORIGINAL LENGTH                      
         AR    R6,R4               NEXT ADDRESS, LESS 1                         
BCOLS_2  LA    R6,1(R6)                                                         
COLS_2   CLI   SRFADDEF,C'E'                                                    
         BNE   MAYBED                                                           
         MVC   PRNTDEF,=C'EQU'                                                  
         MVC   PRNTDSP,=C'    '                                                 
         B     NEXTONE                                                          
MAYBED   CLI   SRFADDEF,C'D'                                                    
         BNE   MAYBEO                                                           
         MVC   PRNTDEF(5),=C'DSECT'                                             
         MVC   LASTDSEC,=CL16' '                                                
         MVC   LASTDSEC(8),SRFADLBL                                             
         MVI   MISSDSEC,C'N'       DATA SET HAS DSECT                           
         B     DISPHEX                                                          
MAYBEO   CLI   SRFADDEF,C'O'                                                    
         BNE   MAYBEC                                                           
         MVC   PRNTDEF,=C'ORG'                                                  
         B     DISPHEX                                                          
MAYBEC   CLI   SRFADDEF,C'C'                                                    
         BNE   MAYBES                                                           
         MVC   PRNTDEF,=C'DC '                                                  
         B     DISPHEX                                                          
MAYBES   CLI   SRFADDEF,C'S'                                                    
         BNE   MUSTBESP                                                         
         MVC   PRNTDEF,=C'DS '                                                  
         B     DISPHEX                                                          
MUSTBESP MVC   PRNTDEF,=C'   '                                                  
         B     NEXTONE                                                          
*                                                                               
* CONVERT NUMBERS TO EBCIDIC                                                    
*                                                                               
DISPHEX  GOTO1 VHEXOUT,DMCB,SRFADDSP,PRNTDSP,L'SRFADDSP                         
         CLC   =X'0000',DMCB+16    NO ERRORS IN HEXOUT                          
         BE    NEXTONE                                                          
         DC    H'0'                                                             
NEXTONE  ST    R7,LASTLBL                                                       
         LR    R7,R6               NEXT ONE                                     
         MVI   SCRNFILD,C'Y'                                                    
         SPACE 4                                                                
*                                                                               
* THIS PART TRANSMITS THE UPDATED TWA                                           
*                                                                               
DATA2PRN CLI   SRVFD3,C'?'                                                      
         BNE   PRNOK                                                            
         CLC   =C'DSECT',PRNTDEF                                                
         BE    PRNOK                                                            
         B     LOOP2                                                            
PRNOK    LA    R3,87(R3)           CALC NEXT RECORD ADDRESS                     
         BCT   R8,LOOP2                                                         
         CLI   COLS,X'1'                                                        
         BE    STOPPRN                                                          
         CLI   ONCE,0              IS THIS THE FIRST TIME?                      
         BNE   STOPPRN             NO, READY TO LEAVE                           
         OI    ONCE,X'FF'          DID IT ONCE                                  
         LA    R3,SRVL01+40        NEXT COLUMN                                  
         B     LOOP1                                                            
*                                                                               
* STOP PRINTING AND SAVE THE LAST LINE AS THE NEXT STARTING LABEL               
*                                                                               
STOPPRN  MVC   PADDER,=CL16'                '                                   
         CLI   SCRNFILD,C'N'                                                    
         BE    NOFILL                                                           
         L     R7,LASTLBL          ADDRESS OF LAST RECORD                       
         CLC   =C'        ',SRFADLBL                                            
         BE    USEDISP                                                          
         MVC   PADDER(8),SRFADLBL  PAD IT WITH SPACES                           
         B     TOPAGAIN                                                         
USEDISP  GOTO1 VHEXOUT,DMCB,SRFADDSP,PADDER,L'SRFADDSP                          
         B     TOPAGAIN                                                         
NOFILL   CLI   MISSDSEC,C'Y'                                                    
*        BNE   DSECEXST                                                         
         MVC   PADDER,=CL16' '                                                  
         B     TOPAGAIN                                                         
DSECEXST MVC   PADDER(8),LASTDSEC  USE DSECT NAME AS START LABEL                
TOPAGAIN MVC   SRVFD4,PADDER       PUT IN 4TH FIELD                             
         XMOD1                                                                  
         EJECT                                                                  
****************************************************************                
* DISPLAY AN ERROR MESSAGE.  R4 HOLDS THE ADDRESS OF THE ERROR *                
****************************************************************                
ERROR    OI    SRVMSGH+6,X'80'     TRANSMIT                                     
         MVI   SRVMSGH+5,X'3C'                                                  
         MVC   SRVMSG(30),0(R4)                                                 
         MVC   SRVFD1,=CL16' '                                                  
         XMOD1                                                                  
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE FASYSLST                                                       
         EJECT                                                                  
* SHORT LIST OF DSECT NAMES FOR FACPAK                                          
SHRTLIST DC    C'ADRREC  ',X'00'                                                
         DC    C'DMDTFIS ',X'00'                                                
         DC    C'DMDTFPH ',X'00'                                                
         DC    C'JOBTAB  ',X'00'                                                
         DC    C'LANGTAB ',X'00'                                                
         DC    C'PGMLST  ',X'00'                                                
         DC    C'PHLIST  ',X'00'                                                
         DC    C'SELIST  ',X'00'                                                
         DC    C'SRS     ',X'00'                                                
         DC    C'SSB     ',X'00'                                                
         DC    C'SYSFAC  ',X'00'                                                
         DC    C'TCB     ',X'00'                                                
         DC    C'TWA     ',X'00'                                                
         DC    C'UTL     ',X'00'                                                
         DC    X'00'                                                            
ERROR0   DC    C'** A NAME IS REQUIRED         '                                
ERROR1   DC    C'** LABEL NOT IN DSECT         '                                
ERROR2   DC    C'** DISPLACEMENT NOT IN DSECT  '                                
ERROR3   DC    C'** NOT DISPLACEMENT OR LABEL  '                                
PADDER   DC    CL16'                '                                           
PADTYPE  DC    CL19'                   '                                        
PADCMNT  DC    CL100'                                       '                   
COLS     DS    C                   NUMBER OF COLUMNS REQUESTED                  
SCRNFILD DS    C                   SCREEN FILLED FLAG                           
CURSYS   DS    X                   CURRENT SYSTEM NUMBER                        
OVLNO    DS    X                   OVERLAY NUMBER                               
OVLCOUNT DS    X                   OVERLAY COUNTER                              
NLINES   EQU   16                  NUMBER OF LINE PER COLUMN                    
LASTLBL  DS    F                   LAST LABEL FOR PAGING                        
HEXDSP   DS    XL8                                                              
HEXDSPLN DS    X                                                                
LASTDSEC DS    CL16'                '                                           
LABELNM  DS    CL16'                '                                           
LABELLN  DS    X                                                                
CKHDRLBL DS    X                                                                
LBLNDSP  DS    C                                                                
MEMNAME  DC    CL16'                '                                           
SYSNAME  DC    CL16'                '                                           
MISSDSEC DS    C                                                                
FIRSTLN  DS    C                                                                
TESTDATA DS    0C' '               PLACE WHERE THE OVERLAYS ARE LOADED          
         EJECT                                                                  
*                                                                               
WORKAREA DSECT                                                                  
DMCB     DS    6F                                                               
* NEEDS TO BE CHANGED                                                           
DSECDATA DS    F                                                                
VHEXIN   DS    A                                                                
VHEXOUT  DS    A                                                                
VGETFACT DS    A                                                                
ONCE     DS    C                                                                
WORKAEND DS    0C                                                               
*                                                                               
* OUTPUT TO TRANSMIT DSECT                                                      
*                                                                               
PRLINE   DSECT                                                                  
PRNTDSP  DS    CL4                                                              
         DS    C                                                                
PRNTLBL  DS    CL8                                                              
         DS    C                                                                
PRNTDEF  DS    CL3                                                              
         DS    C                                                                
PRNTTYP  DS    CL19                                                             
         DS    C                                                                
PRNTCMT  DS    CL39                                                             
         SPACE 4                                                                
       ++INCLUDE FASYSLSTD                                                      
         EJECT                                                                  
* DDCOMFACS                                                                     
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
       ++INCLUDE SRFADDSECT                                                     
         EJECT                                                                  
       ++INCLUDE FADSECTS                                                       
         EJECT                                                                  
       ++INCLUDE SRFADFFD                                                       
         EJECT                                                                  
       ++INCLUDE DDFLDHDR                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003SRFAD00   05/01/02'                                      
         END                                                                    
