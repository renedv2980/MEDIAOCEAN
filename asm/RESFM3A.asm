*          DATA SET RESFM3A    AT LEVEL 067 AS OF 11/14/07                      
*PHASE T8183AA                                                                  
*        TITLE 'T8183A - RESFM3A - ACTIVITY REPORT'                             
*                                                                               
*******************************************************************             
*                                                                 *             
*        RESFM0C (T8183A) --- ACTIVITY REPORT                     *             
*                                                                 *             
*  * NOTE R5 - REPIOBLK                                           *             
* --------------------------------------------------------------- *             
*                                                                 *             
*  JAN06/03 (JRD) - FIX GROUP/SUBGROUP FILTERING                  *             
*                                                                 *             
*  MAR28/05 (BU ) - ADD DEMFILV TO LIST                           *             
*                                                                 *             
*  OCT24/07 (DE ) --- USE SOFT DEMO FILE OPEN LISTS               *             
*                                                                 *             
*                                                                 *             
*******************************************************************             
*                                                                               
T8183A   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**183A**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         ST    R3,RELO                                                          
         SPACE                                                                  
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VKEY                                                             
*                                                                               
         B     XIT                                                              
XIT      XIT1                                                                   
         EJECT                                                                  
****************************************************************                
****************************************************************                
*              VALIDATE KEY ROUTINE                            *                
****************************************************************                
****************************************************************                
         SPACE 1                                                                
VKEY     DS    0H                                                               
         LA    RE,MYWORK           CLEAR COMMON WORK AREA                       
         LA    RF,MYWORKLQ                                                      
         XCEF                                                                   
                                                                                
* SET UP REPIO BLOCK                                                            
         LA    R5,REPIOTBL                                                      
         USING REPIOD,R5                                                        
                                                                                
         L     R1,AIO              IOAREA                                       
         ST    R1,RIPIOARE                                                      
         L     R1,DATAMGR          DATAMGR                                      
         ST    R1,RIPDTMGR                                                      
         MVC   RIPREP,AGENCY                                                    
                                                                                
         B     PND05                                                            
                                                                                
         EJECT                                                                  
****************************************************************                
*    VALIDATE REGION                                                            
****************************************************************                
         SPACE 1                                                                
PND05    MVI   ERROR,INVALID                                                    
                                                                                
         LA    R2,PNDREGH                                                       
         CLI   5(R2),0                                                          
         BE    PND10                                                            
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RREGKEY,R4                                                       
         MVI   RREGKTYP,X'03'                                                   
         MVC   RREGKREP,AGENCY                                                  
         MVC   RREGKREG,8(R2)                                                   
         GOTO1 HIGH                                                             
         CLC   RREGKEY,KEYSAVE                                                  
         BNE   ERREND                                                           
         MVC   FLTREG,8(R2)                                                     
         B     PND10                                                            
         DROP  R4                                                               
                                                                                
**********************************************************************          
*    VALIDATE OFFICE                                                            
**********************************************************************          
                                                                                
PND10    LA    R2,PNDOFFH                                                       
         CLI   5(R2),0                                                          
         BE    PND20                                                            
         GOTO1 VALIOFF,DMCB                                                     
         MVC   RIPOFF,PNDOFF                                                    
         B     PND20                                                            
                                                                                
* WORK+2(20)=NAME , EOFFADD1,EOFFADD2,EOFFSTT,EOFFZIP                           
                                                                                
****************************************************************                
*    VALIDATE PERIOD - REQUIRED                                                 
****************************************************************                
                                                                                
PND20    LA    R2,PNDPERH                                                       
         CLI   5(R2),0                                                          
         BE    ERREND                                                           
         MVI   ERROR,INVDATE                                                    
         OC    ARFPBLK,ARFPBLK     IS IT RFP ?                                  
         BZ    PND22                                                            
         CLC   =Y(RE#RFPMY),9(R2)  ALREADY VALIDATED?                           
         BE    PND30               YES                                          
         CLC   =C'PERMY',8(R2)    'PERMY' SYMBOLIC NAME?                        
         BNE   PND22               NO - CHECK IF DATE ENTERED                   
         BAS   RE,VALRFP           YES - LET RFP DO IT'S THING                  
         B     PND30                                                            
*                                                                               
PND22    BAS   RE,DASH                                                          
         LTR   R4,R4                                                            
         BNZ   PND25                                                            
                                                                                
* SINGLE DATE VALIDATION                                                        
         GOTO1 DATVAL,DMCB,(2,PNDPER),WORK                                      
         OC    DMCB(4),DMCB                                                     
         BZ    ERREND                                                           
         CLI   OFFLINE,C'Y'        IF OFFLINE, GET BROADS                       
         BNE   PND30                                                            
         GOTO1 GETBROAD,DMCB,(1,WORK),WORK+6,GETDAY,ADDAY                       
         GOTO1 DATCON,DMCB,(0,WORK+6),(2,RIPDATS)                               
         GOTO1 DATCON,DMCB,(0,WORK+12),(2,RIPDATE)                              
         B     PND26                                                            
                                                                                
* TWO DATE VALIDATION                                                           
PND25    BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   DUB(0),PNDPER       VALIDATE 1ST MON/YR                          
         GOTO1 DATVAL,DMCB,(2,DUB),WORK                                         
         OC    DMCB(4),DMCB                                                     
         BZ    ERREND                                                           
         LA    R4,2(R4)            POINT R4 TO 2ND Y/M DATE                     
         LA    R3,PNDPER                                                        
         AR    R3,R4                                                            
         GOTO1 DATVAL,DMCB,(2,0(R3)),WORK+6                                     
         OC    DMCB(4),DMCB                                                     
         BZ    ERREND                                                           
         CLC   WORK(6),WORK+6                                                   
         BH    ERREND                                                           
         CLI   OFFLINE,C'Y'                                                     
         BNE   PND30                                                            
                                                                                
* START DATE IN WORK - END DATE IN WORK+6                                       
* SET BROADCAST START DATE IN RIPDATS                                           
         GOTO1 GETBROAD,DMCB,(1,WORK),WORK+12,GETDAY,ADDAY                      
         GOTO1 DATCON,DMCB,(0,WORK+12),(2,RIPDATS)                              
                                                                                
* SET BROADCAST END DATE IN RIPDATE                                             
         GOTO1 GETBROAD,DMCB,(1,WORK+6),WORK+12,GETDAY,ADDAY                    
         GOTO1 DATCON,DMCB,(0,WORK+18),(2,RIPDATE)                              
                                                                                
                                                                                
*   RIPDATS-1 YEAR = READ START DATE FOR REPIO                                  
PND26    GOTO1 DATCON,DMCB,(2,RIPDATS),(0,WORK)                                 
         GOTO1 ADDAY,DMCB,(C'Y',WORK),(0,WORK),-1                               
*                                                                               
         GOTO1 DATCON,DMCB,(0,WORK),(2,RIPDATSS)                                
         GOTO1 DATCON,DMCB,(2,RIPDATS),(3,YMDST)                                
         GOTO1 DATCON,DMCB,(2,RIPDATE),(3,YMDND)                                
         B     PND30                                                            
*                                                                               
*                                                                               
* ROUTINE RETURNS LENGTH OF VALID FIELD BEFORE DASH IN R4 (LENGTH-1)            
* R2 POINTS TO FIELD HEADER                                                     
*                                                                               
DASH     NTR1                                                                   
         SR    R4,R4                                                            
         ZIC   R3,5(R2)            LENGTH OF INPUT FIELD                        
         LA    R6,8(R2)            POINT R6 TO DATA                             
DASH2    CLI   0(R6),C'-'                                                       
         BE    DASH4                                                            
         LA    R6,1(R6)                                                         
         LA    R4,1(R4)            LENGTH OF VALID FIELD BEFORE DASH            
         BCT   R3,DASH2                                                         
         SR    R4,R4               SET R4 TO INDICATE NO DASH                   
         B     DASHX                                                            
*                                                                               
DASH4    LTR   R4,R4               TEST DASH IN FIRST POSITION                  
         BZ    ERREND                                                           
DASHX    DS    0H                                                               
         XIT1  REGS=(R4)                                                        
                                                                                
         EJECT                                                                  
****************************************************************                
*    VALIDATE GRP+SUB                                                           
****************************************************************                
         SPACE 1                                                                
PND30    MVI   ERROR,INVALID                                                    
         LA    R2,PNDGRPH                                                       
         CLI   5(R2),0                                                          
         BE    PND40                                                            
         GOTO1 VALIGRP,DMCB                                                     
         MVC   RIPGRP(2),PNDGRP                                                 
*                                                                               
         CLI   RIPSBGP,C' '        CLEAR SUBGROUP                               
         BH    *+8                                                              
         MVI   RIPSBGP,0                                                        
*                                                                               
         B     PND40                                                            
                                                                                
* GROUP NAME WORK+10(10) SUBNAME WORK+20(10)                                    
                                                                                
********************************************************************            
*    VALIDATE STATION                                                           
********************************************************************            
         SPACE 1                                                                
PND40    LA    R2,PNDSTAH                                                       
         MVI   ERROR,INVALID                                                    
         CLI   5(R2),0                                                          
         BNE   PND45                                                            
* IF STATION SIGN-ON, MUST HAVE INPUT                                           
         CLI   TWAACCS,C'$'                                                     
         BNE   PND50                                                            
         B     ERREND                                                           
                                                                                
PND45    GOTO1 VALISTA,DMCB                                                     
         MVC   RIPSTA,WORK                                                      
                                                                                
* IF STATION SIGN-ON, CHECK IF IT'S A VALID SIGN ON ID                          
         CLI   TWAACCS,C'$'                                                     
         BNE   PND50                                                            
         L     R6,AIO                                                           
         USING RSTASOEL,R6                                                      
         MVI   ELCODE,6            GET VALID SIGN ON ID ELEMENT                 
         BAS   RE,GETEL                                                         
         BNE   PND50                                                            
PND47    DS    0H                                                               
         CLC   RSTASID,TWAORIG     VALID SIGN-ON?                               
         BE    PND50               YES, PROCEED                                 
         BAS   RE,NEXTEL           NOPE, CHECK NEXT ELEMENT                     
         BE    PND47                                                            
         B     ERREND              ALL DONE, NO MATCH, NOT VALID                
         DROP  R6                                                               
                                                                                
* WORK=CALL LETTER                                                              
* WORK+4  A-AM F-FM C-CM T=BLANK                                                
* WORK+10 MARKET NAME                                                           
* WORK+40  1 OR 2 IF SATELLITE STATION                                          
* WORK+41 GROUP/SUB GROUP CODE                                                  
*                                                                               
         SPACE 1                                                                
****************************************************************                
*   VALIDATE ACTIVITY  -  DEFAULT=TODAY'S DATE                                  
****************************************************************                
         SPACE 1                                                                
PND50    LA    R2,PNDADTEH                                                      
         CLI   5(R2),0                                                          
         BE    PND60                                                            
         OC    ARFPBLK,ARFPBLK     IS IT RFP ?                                  
         BZ    PND52                                                            
         CLC   =Y(RE#RFPRD),9(R2)  ALREADY VALIDATED                            
         BE    PND60               YES                                          
         CLC   =C'STEND',8(R2)     SYMBOLIC NAME ?                              
         BNE   PND52               NO - SEE IF DATE ENTERED                     
         BAS   RE,VALRFP           YES/LET RFP DO ITS THING                     
         B     PND60                                                            
PND52    GOTO1 DATVAL,DMCB,(0,PNDADTE),WORK                                     
         OC    DMCB(4),DMCB                                                     
         BZ    ERREND                                                           
         GOTO1 DATCON,DMCB,(0,WORK),(3,ACTSTR)                                  
         MVC   ACTEND,ACTSTR                                                    
* - ARE THERE TWO DATES ?                                                       
         LA    R2,PNDADTE                                                       
         LA    R3,9                LOOK FOR DASH                                
         CLI   0(R2),C'-'                                                       
         BE    *+16                                                             
         LA    R2,1(R2)                                                         
         BCT   R3,*-12                                                          
         B     PND60                                                            
         GOTO1 DATVAL,DMCB,(0,1(R2)),WORK                                       
         GOTO1 DATCON,DMCB,(0,WORK),(3,ACTEND)                                  
         B     PND60                                                            
                                                                                
****************************************************************                
*    VALIDATE SALESPERSON                                      *                
****************************************************************                
         SPACE 1                                                                
PND60    LA    R2,PNDSALH                                                       
         CLI   5(R2),0                                                          
         BNE   PND64                                                            
         CLI   TWAWHEN,2           ,,IF IT'S SOON                               
         BNE   PND70               NO                                           
         CLI   PNDSTAH+5,0         ,,NEED STATION OR SALESPERSON                
         BE    MYEND               ,,DO MY OWN ERROR MESSAGE                    
         B     PND70                                                            
PND64    CLC   PNDSAL(2),=C'D-'    TEST TEAM OPTION                             
         BE    VALTEAM                                                          
         CLI   5(R2),3             TEST LENGTH ERROR                            
         BH    ERREND                                                           
*                                                                               
         OC    PNDSAL(3),SPACES    OR IN SPACES                                 
         MVI   KEY,6                                                            
         XC    KEY+1(21),KEY+1                                                  
         MVC   KEY+22(2),AGENCY                                                 
         MVC   KEY+24(3),PNDSAL                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   ERREND                                                           
         MVC   RIPSAL,PNDSAL                                                    
         OC    RIPOFF,RIPOFF               IF OFFICE                            
         BZ    PND70                                                            
         GOTO1 GETREC                   GET REC AND MATCH ON OFFICE             
         L     R6,AIO                                                           
         MVI   ELCODE,1                                                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RSALELEM,R6                                                      
         CLC   RSALOFF,RIPOFF                                                   
         BNE   MYEND2                                                           
         B     PND70                                                            
         DROP  R6                                                               
         EJECT                                                                  
VALTEAM  DS    0H                                                               
         CLI   5(R2),6                                                          
         BH    ERREND                                                           
         OC    PNDSAL,SPACES                                                    
*                                  SET KEY                                      
         MVI   KEY,5                                                            
         XC    KEY+1(22),KEY+1                                                  
         MVC   KEY+23(2),AGENCY                                                 
         MVC   KEY+25(2),PNDSAL+2                                               
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   ERREND                                                           
         LA    R3,KEY                                                           
         USING RTEMREC,R3                                                       
*                                                                               
         MVC   RIPTEAM,RTEMKTEM                                                 
         B     PND70                                                            
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
         SPACE 1                                                                
****************************************************************                
*    VALIDATE DIV+TEAM                                                          
****************************************************************                
         SPACE 1                                                                
PND70    LA    R2,PNDTEMH                                                       
         CLI   5(R2),0                                                          
         BE    PND80                                                            
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RTEMKEY,R4                                                       
         MVI   RTEMKTYP,X'05'                                                   
         MVC   RTEMKREP,AGENCY                                                  
         MVC   RTEMKTEM,8(R2)                                                   
         GOTO1 HIGH                                                             
         CLI   5(R2),1             WAS ONLY GROUP ENTERED                       
         BE    PND72                                                            
         CLC   RTEMKEY,KEYSAVE                                                  
         BNE   ERREND                                                           
         MVC   RIPTEAM,8(R2)                                                    
         B     PND80                                                            
         SPACE                                                                  
PND72    CLC   RTEMKEY(26),KEYSAVE    ONLY NEED DIVISION                        
         BNE   ERREND                                                           
         MVC   RIPTEAM(1),8(R2)                                                 
         B     PND80                                                            
         DROP  R4                                                               
         SPACE 1                                                                
****************************************************************                
*    VALIDATE ADVERTISER                                                        
****************************************************************                
         SPACE 1                                                                
PND80    LA    R2,PNDADVH                                                       
         CLI   5(R2),0                                                          
         BE    PND90                                                            
         GOTO1 VALIADV                                                          
         MVC   RIPADV,WORK                                                      
         B     PND90                                                            
                                                                                
* WORK= ADV CODE WORK+10=ADV NAME                                               
         SPACE 2                                                                
******************************************************************              
*  VALIDATE AGENCY                                                              
******************************************************************              
         SPACE                                                                  
PND90    LA    R2,PNDAGYH                                                       
         CLI   5(R2),0                                                          
         BE    PND100                                                           
         OC    RIPAGY,SPACES       FILL WITH SPACES                             
         LA    R0,5                                                             
         LA    R1,8(,R2)                                                        
         LA    RE,RIPAGY                                                        
         SR    RF,RF                                                            
VAGY10   CLI   0(R1),C'-'                                                       
         BE    VAGY14                                                           
         CLI   0(R1),C' '                                                       
         BNH   VAGY14                                                           
         MVC   0(1,RE),0(R1)                                                    
         LA    R1,1(,R1)                                                        
         LA    RE,1(,RE)                                                        
         LA    RF,1(,RF)                                                        
         BCT   R0,VAGY10                                                        
         B     ERREND                                                           
         SPACE                                                                  
VAGY14   CLM   RE,1,5(R2)          THIS ALL THERE IS                            
         BE    VAGY20                                                           
         LA    R0,2                                                             
         LA    R1,1(,R1)                                                        
         LA    RE,RIPAGOFF                                                      
         SR    RF,RF                                                            
VAGY16   CLI   0(R1),C' '                                                       
         BNH   VAGY18                                                           
         MVC   0(1,RE),0(R1)                                                    
         LA    R1,1(,R1)                                                        
         LA    RE,1(,RE)                                                        
         LA    RF,1(,RF)                                                        
         BCT   R0,VAGY16                                                        
VAGY18   CH    RF,=H'2'                                                         
         BH    ERREND                                                           
         SPACE                                                                  
VAGY20   XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RAGYKEY,R4                                                       
         MVI   RAGYKTYP,X'0A'                                                   
         MVC   RAGYKAGY,RIPAGY                                                  
         OC    RAGYKAGY,SPACES                                                  
         MVC   RAGYKAOF,RIPAGOFF                                                
         OC    RAGYKAOF,SPACES                                                  
         MVC   RAGYKREP,RIPREP                                                  
         GOTO1 HIGH                                                             
         CLC   RAGYKEY,KEYSAVE                                                  
         BNE   ERREND                                                           
         SPACE                                                                  
         CLI   RIPAGOFF,X'40'                                                   
         BNE   PND100                                                           
         XC    RIPAGOFF,RIPAGOFF                                                
         B     PND100                                                           
         DROP  R4                                                               
         EJECT                                                                  
*******************************************************************             
*  VALIDATE CLASS                                                               
*******************************************************************             
PND100   LA    R2,PNDCLSH                                                       
         CLI   5(R2),0                                                          
         BE    PND110                                                           
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RCLSKEY,R4                                                       
         MVI   RCLSKTYP,X'0D'                                                   
         MVC   RCLSKREP,AGENCY                                                  
         MVC   RCLSKCLS,8(R2)                                                   
         OC    RCLSKCLS,SPACES                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   ERREND                                                           
         SPACE                                                                  
         MVC   FLTCLS,8(R2)                                                     
         OC    FLTCLS,SPACES                                                    
         B     PND110                                                           
         DROP  R4                                                               
         SPACE 2                                                                
*******************************************************************             
* VALIDATE CATEGORY                                                             
*****************************************************************               
PND110   LA    R2,PNDCATH                                                       
         CLI   5(R2),0                                                          
         BE    PND115                                                           
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RCTGKEY,R4                                                       
         MVI   RCTGKTYP,X'0F'                                                   
         MVC   RCTGKREP,AGENCY                                                  
         MVC   RCTGKCTG,8(R2)                                                   
         OC    RCTGKCTG,SPACES                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   ERREND                                                           
         MVC   RIPCAT,8(R2)                                                     
         B     PND115                                                           
         DROP  R4                                                               
         EJECT                                                                  
PND115   DS    0H                                                               
*******************************************************                         
* VALIDATE CONTRACT TYPE                                                        
*******************************************************                         
         MVI   REPORT,0                                                         
         LA    R2,PNDCTPH                                                       
         XC    CONTYPES(255),CONTYPES                                           
         CLI   5(R2),0                                                          
         BE    VCTYPX                                                           
*                                                                               
         MVI   BYTE,C'N'                                                        
         LA    R3,8(R2)                                                         
         CLI   0(R3),C'*'                                                       
         BNE   *+12                                                             
         LA    R3,1(R3)                                                         
         MVI   BYTE,C'Y'           INDICATE MIGHT BE EXCLUDE                    
*                                                                               
         LR    RE,R3                                                            
         LA    R1,8(R2)                                                         
         SR    RE,R1                                                            
         ZIC   R1,5(R2)                                                         
         SR    R1,RE               LENGTH OF SET NAME                           
         CH    R1,=H'4'            CHECK LENGTH                                 
         BH    ERREND                                                           
         CH    R1,=H'1'                                                         
         BL    ERREND                                                           
         BH    VCTYP10             SET RECORD                                   
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'32'           ACCESS CONTRACT TYPE RECORD                  
         MVC   KEY+24(2),RIPREP    REP ALPHA CODE                               
         MVC   KEY+26(1),0(R3)     INSERT CONTRACT TYPE                         
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   ERREND                                                           
*                                                                               
         MVC   CONTYPES,0(R3)                                                   
*                                                                               
         CLI   BYTE,C'Y'           EXCLUDE TYPE?                                
         BNE   *+8                                                              
         OI    REPORT,RPTQNCT      YES                                          
         B     VCTYPX                                                           
*                                                                               
VCTYP10  DS    0H                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'38'           ACCESS SET RECORD                            
         MVC   KEY+19(2),RIPREP    REP ALPHA CODE                               
         MVC   KEY+21(2),=C'CT'    INSERT SET TYPE                              
         MVC   KEY+23(4),0(R3)     INSERT SET NAME                              
         OC    KEY+23(4),SPACES                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   ERREND                                                           
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'        NEW DESCRIPTION ELEMENT                      
         BAS   RE,GETEL                                                         
         BNE   VCTYP12             MUST BE OLD SET RECORD                       
*                                                                               
         USING RSET1DES,R6                                                      
         TM    RSET1FLG,X'08'      EXCLUSION SET?                               
         BZ    *+8                                                              
         OI    REPORT,RPTQNCT      YES                                          
*                                                                               
         TM    RSET1FLG,X'80'      SET OF SETS?                                 
         BNZ   VCTYP20             YES                                          
         DROP  R6                                                               
*                                                                               
VCTYP12  DS    0H                  PROCESS NORMAL SET RECORD                    
         L     R6,AIO                                                           
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                 FOUND                                        
         DC    H'0'                NOT FOUND                                    
         ZIC   RF,1(R6)            GET ELEMENT LENGTH                           
         LA    RE,3                SUBTRACT CONTROL                             
         SR    RF,RE                                                            
         LA    R3,CONTYPES                                                      
         LA    R0,255(R3)                                                       
         LA    R6,3(R6)                                                         
*                                                                               
VCTYP14  DS    0H                                                               
         MVC   0(1,R3),0(R6)                                                    
         LA    R3,1(R3)                                                         
         LA    R6,1(R6)                                                         
         CR    R3,R0                                                            
         BNL   MYEND3                                                           
         BCT   RF,VCTYP14                                                       
         B     VCTYPX                                                           
*                                                                               
VCTYP20  DS    0H                  PROCESS SET OF SETS                          
         L     R6,AIO                                                           
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                 FOUND                                        
         DC    H'0'                NOT FOUND                                    
         L     RE,AIO2                                                          
         XC    0(255,RE),0(RE)                                                  
         ZIC   RF,1(R6)            GET ELEMENT LENGTH                           
         LA    R0,3                SUBTRACT CONTROL                             
         SR    RF,R0                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),3(R6)       KEEP IDENTIFIERS HERE                        
         ST    RE,FULL             KEEP ADDRESS                                 
*                                                                               
VCTYP22  DS    0H                                                               
         L     RE,FULL                                                          
         CLC   0(4,RE),SPACES      ANY SET NAME?                                
         BNH   VCTYPX              NO                                           
*                                                                               
         LA    RF,4(RE)            SETUP NEXT SET NAME                          
         ST    RF,FULL                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'38'           ACCESS SET RECORD                            
         MVC   KEY+19(2),RIPREP    REP ALPHA CODE                               
         MVC   KEY+21(2),=C'CT'    INSERT SET TYPE                              
         MVC   KEY+23(4),0(RE)     INSERT SET NAME                              
         OC    KEY+23(4),SPACES                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   ERREND                                                           
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'        NEW DESCRIPTION ELEMENT                      
         BAS   RE,GETEL                                                         
         BNE   VCTYP24             MUST BE OLD SET RECORD                       
*                                                                               
         USING RSET1DES,R6                                                      
         TM    RSET1FLG,X'80'      SET OF SETS?                                 
         BNZ   VCTYP22             YES SKIP IT                                  
         DROP  R6                                                               
*                                                                               
VCTYP24  DS    0H                  PROCESS NORMAL SET RECORD                    
         L     R6,AIO                                                           
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                 FOUND                                        
         DC    H'0'                NOT FOUND                                    
         ZIC   RF,1(R6)            GET ELEMENT LENGTH                           
         LA    RE,3                SUBTRACT CONTROL                             
         SR    RF,RE                                                            
         LA    R0,CONTYPES+255                                                  
         LA    R6,3(R6)                                                         
*                                                                               
VCTYP30  DS    0H                  CHECK FOR REPEATS                            
         LA    R3,CONTYPES                                                      
*                                                                               
VCTYP32  DS    0H                  CHECK FOR REPEATS                            
         CLI   0(R3),0             END OF TABLE                                 
         BE    VCTYP34             YES - INSERT CONTRACT TYPE HERE              
         CLC   0(1,R3),0(R6)       CONTRACT TYPE MATCH?                         
         BE    VCTYP36             YES - SKIP THIS ONE                          
         LA    R3,1(R3)                                                         
         CR    R3,R0                                                            
         BL    VCTYP32                                                          
         B     MYEND3                                                           
*                                                                               
VCTYP34  DS    0H                                                               
         MVC   0(1,R3),0(R6)                                                    
VCTYP36  DS    0H                                                               
         LA    R6,1(R6)                                                         
         BCT   RF,VCTYP30                                                       
         B     VCTYP22             NEXT SET NAME                                
*                                                                               
VCTYPX   DS    0H                                                               
         SPACE 2                                                                
*******************************************************                         
* VALIDATE FILTERS                                                              
********************************************************                        
         LA    R2,PNDFILTH                                                      
         CLI   5(R2),0             ANY INPUT?                                   
         BNE   *+12                YES                                          
         OI    REPORT,RPTQALL      SET FOR ALL REPORT                           
         B     VFILX               NO                                           
*                                                                               
         XC    WORK2,WORK2                                                      
         GOTO1 SCANNER,DMCB,PNDFILTH,WORK2                                      
         ZIC   R1,DMCB+4                                                        
         LA    R3,WORK2                                                         
VFIL00   CLI   12(R3),C'P'         PENDING                                      
         BNE   VFIL02                                                           
         OI    REPORT,RPTQPND                                                   
         B     VFIL50                                                           
VFIL02   CLI   12(R3),C'I'         INCOMPLETE                                   
         BNE   VFIL04                                                           
         OI    REPORT,RPTQINC                                                   
         B     VFIL50                                                           
VFIL04   CLI   12(R3),C'C'         COMPLETE                                     
         BNE   VFIL06                                                           
         OI    REPORT,RPTQCPL                                                   
         B     VFIL50                                                           
VFIL06   CLI   12(R3),C'L'         LOSSES                                       
         BNE   VFIL08                                                           
         OI    REPORT,RPTQLOS                                                   
         B     VFIL50                                                           
VFIL08   DS    0H                                                               
         B     ERREND                                                           
*                                                                               
VFIL50   LA    R3,32(R3)                                                        
         BCT   R1,VFIL00                                                        
*                                                                               
VFILX    DS    0H                                                               
****************************************************************                
*  REPORT TYPE - SALESPERSON SORT/STATIONS SORT                                 
*****************************************************************               
         LA    R2,PNDTYPH          REPORT TYPE                                  
         CLI   5(R2),0                                                          
         BE    VRTYPX                                                           
         CLC   =C'STA',8(R2)                                                    
         BNE   *+12                                                             
         OI    REPORT,RPTQSTA      STATION TYPE REPORT                          
         B     VRTYPX                                                           
         CLC   =C'SAL',8(R2)                                                    
         BNE   ERREND                                                           
VRTYPX   DS    0H                                                               
*******************************************************                         
* VALIDATE RECAP FIELD                                                          
********************************************************                        
         MVI   TOTALS,0                                                         
         LA    R2,PNDRCPH                                                       
         CLI   5(R2),0                                                          
         BE    VRCPX                                                            
*                                                                               
         CLI   8(R2),C'R'                                                       
         BNE   *+12                                                             
         OI    TOTALS,TTLQONLY                                                  
         B     VRCPX                                                            
*                                                                               
         CLI   8(R2),C'D'                                                       
         BNE   ERREND                                                           
*                                                                               
VRCPX    DS    0H                                                               
*******************************************************                         
* VALIDATE DETAIL TOTAL OPTIONS                                                 
********************************************************                        
         TM    TOTALS,TTLQONLY     RECAP?                                       
         BO    VTTLX               YES                                          
*                                                                               
         LA    R2,PNDTTLH                                                       
         CLI   5(R2),0             ANY INPUT?                                   
         BNE   VTTL00              YES                                          
*                                                                               
         OI    TOTALS,TTLQALL      NO - SET FOR ALL TOTALS                      
         B     VTTLX                                                            
*                                                                               
VTTL00   XC    WORK2,WORK2                                                      
         GOTO1 SCANNER,DMCB,PNDTTLH,WORK2                                       
         LA    R3,WORK2                                                         
*                                                                               
VTTL02   DS    0H                                                               
         CLI   0(R3),0             ANY OPTION?                                  
         BE    VTTLX               NO                                           
         CLI   1(R3),0             OPTION=X?                                    
         BNE   ERREND              YES                                          
*                                                                               
         ZIC   RE,0(R3)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R3),=CL04'ALL'                                              
         BNE   VTTL04                                                           
*                                                                               
         OI    TOTALS,TTLQALL      SET FOR ALL TOTALS                           
         B     VTTL20                                                           
*                                                                               
VTTL04   DS    0H                                                               
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R3),=CL12'SALESPERSON'                                      
         BNE   VTTL05                                                           
*                                                                               
         TM    REPORT,RPTQSTA                                                   
         BZ    *+12                                                             
         OI    TOTALS,STAQSAL                                                   
         B     *+8                                                              
         OI    TOTALS,SALQSAL                                                   
         B     VTTL20                                                           
*                                                                               
VTTL05   DS    0H                                                               
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R3),=CL08'STATION'                                          
         BNE   VTTL06                                                           
*                                                                               
         TM    REPORT,RPTQSTA                                                   
         BZ    *+12                                                             
         OI    TOTALS,STAQSTA                                                   
         B     *+8                                                              
         OI    TOTALS,SALQSTA                                                   
         B     VTTL20                                                           
*                                                                               
VTTL06   DS    0H                                                               
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R3),=CL05'TEAM'                                             
         BNE   VTTL07                                                           
*                                                                               
         TM    REPORT,RPTQSTA                                                   
         BNZ   ERREND                                                           
         OI    TOTALS,SALQTEAM                                                  
         B     VTTL20                                                           
*                                                                               
VTTL07   DS    0H                                                               
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R3),=CL07'OFFICE'                                           
         BNE   VTTL08                                                           
*                                                                               
         TM    REPORT,RPTQSTA                                                   
         BZ    *+12                                                             
         OI    TOTALS,STAQOFF                                                   
         B     *+8                                                              
         OI    TOTALS,SALQOFF                                                   
         B     VTTL20                                                           
*                                                                               
VTTL08   DS    0H                                                               
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R3),=CL08'COMPANY'                                          
         BNE   VTTL09                                                           
*                                                                               
         TM    REPORT,RPTQSTA                                                   
         BZ    *+12                                                             
         OI    TOTALS,STAQCOMP                                                  
         B     *+8                                                              
         OI    TOTALS,SALQCOMP                                                  
         B     VTTL20                                                           
*                                                                               
VTTL09   DS    0H                                                               
         B     ERREND                                                           
*                                                                               
VTTL20   LA    R3,32(R3)                                                        
         B     VTTL02                                                           
*                                                                               
VTTLX    DS    0H                                                               
********************************************************                        
* RECAP REPORT OPTION                                                           
********************************************************                        
         TM    TOTALS,TTLQONLY     RECAP?                                       
         BNO   VROPX               NO                                           
*                                                                               
         LA    R2,PNDROPH                                                       
         CLI   5(R2),0             ANY INPUT?                                   
         BNE   VROP00              YES                                          
*                                                                               
         TM    REPORT,RPTQSTA      STATION REPORT?                              
         BO    *+12                YES                                          
         OI    TOTALS,SALRSTA      NO - SET FOR STATION RECAP                   
         B     VROPX                                                            
*                                                                               
         OI    TOTALS,STARSAL      YES - SALESPERSON RECAP                      
         B     VROPX                                                            
*                                                                               
VROP00   XC    WORK2,WORK2                                                      
         GOTO1 SCANNER,DMCB,PNDROPH,WORK2                                       
         LA    R3,WORK2                                                         
*                                                                               
         CLI   0(R3),0             ANY OPTION?                                  
         BE    VROPX               NO                                           
         CLI   1(R3),0             OPTION=X?                                    
         BNE   ERREND              YES                                          
*                                                                               
         ZIC   RE,0(R3)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R3),=CL12'SALESPERSON'                                      
         BNE   VROP04                                                           
*                                                                               
         TM    REPORT,RPTQSTA                                                   
         BZ    *+12                                                             
         OI    TOTALS,STARSAL                                                   
         B     *+8                                                              
         OI    TOTALS,SALRSAL                                                   
         B     VROP20                                                           
*                                                                               
VROP04   DS    0H                                                               
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R3),=CL08'STATION'                                          
         BNE   VROP05                                                           
*                                                                               
         TM    REPORT,RPTQSTA                                                   
         BZ    *+12                                                             
         OI    TOTALS,STARSTA                                                   
         B     *+8                                                              
         OI    TOTALS,SALRSTA                                                   
         B     VROP20                                                           
*                                                                               
VROP05   DS    0H                                                               
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R3),=CL05'TEAM'                                             
         BNE   VROP06                                                           
*                                                                               
         TM    REPORT,RPTQSTA                                                   
         BNZ   ERREND                                                           
         OI    TOTALS,SALRTEAM                                                  
         B     VROP20                                                           
*                                                                               
VROP06   DS    0H                                                               
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R3),=CL07'OFFICE'                                           
         BNE   VROP07                                                           
*                                                                               
         TM    REPORT,RPTQSTA                                                   
         BZ    *+12                                                             
         OI    TOTALS,STAROFF                                                   
         B     *+8                                                              
         OI    TOTALS,SALROFF                                                   
         B     VROP20                                                           
*                                                                               
VROP07   DS    0H                                                               
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R3),=CL08'COMPANY'                                          
         BNE   VROP08                                                           
*                                                                               
         TM    REPORT,RPTQSTA                                                   
         BZ    *+12                                                             
         OI    TOTALS,STARCOMP                                                  
         B     *+8                                                              
         OI    TOTALS,SALRCOMP                                                  
         B     VROP20                                                           
*                                                                               
VROP08   DS    0H                                                               
         B     ERREND                                                           
*                                                                               
VROP20   LA    R3,32(R3)                                                        
         CLI   0(R3),0             ONLY ONE OPTION ALLOWED                      
         BNE   ERREND                                                           
*                                                                               
VROPX    DS    0H                                                               
*******************************************************                         
* VALIDATE OPTIONS                                                              
********************************************************                        
         LA    R2,PNDOPTH                                                       
         CLI   5(R2),0             ANY INPUT?                                   
         BE    VOPTX               NO                                           
*                                                                               
         XC    WORK2,WORK2                                                      
         GOTO1 SCANNER,DMCB,PNDOPTH,WORK2                                       
         LA    R3,WORK2                                                         
*                                                                               
VOPT02   DS    0H                                                               
         CLI   0(R3),0             ANY OPTION?                                  
         BE    VOPTX               NO                                           
         CLI   1(R3),0             OPTION=X?                                    
         BNE   ERREND              YES                                          
*                                                                               
         ZIC   RE,0(R3)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R3),=CL06'ROUND'                                            
         BNE   VOPT04                                                           
*                                                                               
         OI    REPORT,RPTQRND      SET TO ROUND SHARES                          
         B     VOPT20                                                           
*                                                                               
VOPT04   DS    0H                                                               
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R3),=CL04'WKS'                                              
         BNE   VOPT06                                                           
*                                                                               
         OI    TOTALS,TTLQWKS      SET TO SHOW WEEKLY TOTALS                    
         B     VOPT20                                                           
*                                                                               
VOPT06   DS    0H                                                               
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R3),=CL10'PROPOSALS'                                        
         BNE   VOPT08                                                           
*                                                                               
         CLI   TWAWHEN,2           IS IT SOON?                                  
         BNE   VOPT06A             NO - THEN ITS OV AND OK                      
*                                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(45),=CL45'PROPOSAL OPTION ONLY VALID OVERNIGHT'          
         GOTO1 MYENDX                                                           
*                                                                               
VOPT06A  DS    0H                                                               
         TM    TOTALS,TTLQONLY     RECAP REPORT?                                
         BO    ERREND              YES                                          
*                                                                               
         OI    REPORT,RPTQPRO      SET TO COUNT PROPOSALS                       
         B     VOPT20                                                           
*                                                                               
VOPT08   DS    0H                                                               
         B     ERREND                                                           
*                                                                               
VOPT20   LA    R3,32(R3)                                                        
         B     VOPT02                                                           
*                                                                               
VOPTX    DS    0H                                                               
*                                                                               
PNDX     DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
*********************************************************************           
* THIS ROUTINE WILL GET THE AGENCY/MEDIA CODE                                   
*********************************************************************           
GETAGY   ST    RE,FULL                                                          
         XC    KEY,KEY                                                          
         MVI   KEY,X'06'                                                        
         MVC   KEY+1(2),AGENCY                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         B     VPJ20                                                            
VPJ10    BAS   RE,NEXTEL                                                        
VPJ20    BE    *+6                                                              
         DC    H'0'                                                             
         CLI   2(R6),C'T'          WANT TELEVISION                              
         BNE   VPJ10                                                            
****     MVC   CAGYMED,3(R6)                                                    
         L     RE,FULL                                                          
         BR    RE                                                               
         EJECT                                                                  
         SPACE 1                                                                
*  RESET INTERNAL VALUES FOR REP                                                
         MVI   SYSTEM,C'R'                                                      
         MVC   LKEY,=H'27'                                                      
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'34'                                                  
         MVC   SYSFIL,=C'REPFIL  '                                              
         MVC   SYSDIR,=C'REPDIR  '                                              
         MVC   REQFILE,=C'REPREQ '                                              
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 2                                                                
MYEND    XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(9),=C'* ERROR *'                                         
         MVC   CONHEAD+10(36),=C'SOON REQUIRES STATION OR SALESPERSON'          
         B     MYENDX                                                           
                                                                                
MYEND2   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(9),=C'* ERROR *'                                         
         MVC   CONHEAD+10(27),=C'OFFICE/SALESPERSON MISMATCH'                   
         B     MYENDX                                                           
                                                                                
MYEND3   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(9),=C'* ERROR *'                                         
         MVC   CONHEAD+10(30),=C'TOO MANY CONTRACT TYPES IN SET'                
         B     MYENDX                                                           
                                                                                
MYENDX   OI    CONHEADH+6,X'80'                                                 
         MVI   ERROR,X'FE'         USING MY OWN ERROR MESSAGE                   
         MVI   GENSTAT2,USMYOK                                                  
         GOTO1 ERREX2                                                           
         SPACE 2                                                                
ERREND   GOTO1 ERREX                                                            
         SPACE 3                                                                
RELO     DS    A                                                                
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
         SPACE 2                                                                
SPTFLIST DC    CL8'NSPTFILE'                                                    
         DC    CL8'NSPTDIR'                                                     
         DC    CL8'NSTAFILE'                                                    
         DC    CL8'NCTFILE'                                                     
         DC    CL8'NDEMDIRA'                                                    
         DC    CL8'NDEMDIRN'                                                    
         DC    CL8'NL=DEMFA'                                                    
         DC    CL8'NL=DEMFN'                                                    
         DC    C'X'                                                             
         SPACE                                                                  
REPFLIST DC    CL8'NREPFILE'                                                    
         DC    CL8'NREPDIR'                                                     
         DC    CL8'NCTFILE'                                                     
         DC    C'X'                                                             
CTLSWTCH DC    C'N'                CONTROL FILE SWITCH                          
         EJECT                                                                  
* RFP VALIDATION                                                                
VALRFP   NTR1                                                                   
         MVI   ERROR,INVALID                                                    
         CLI   5(R2),0                                                          
         BE    ERREND                                                           
                                                                                
         XC    WORK,WORK                                                        
         LA    R3,WORK                                                          
         USING QRFPD,R3                                                         
         MVI   QRFPMODE,QRFPSYMB   SYMBOLIC NAME VALIDATION                     
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   QRFPWORK(0),8(R2)   PASS SYMBOLIC NAME                           
         OC    QRFPWORK,SPACES                                                  
         GOTO1 RFP,DMCB,(R3)                                                    
         OC    QRFPWORK,QRFPWORK   ERROR                                        
         BZ    ERREND                                                           
         MVC   8(L'QRFPESC,R2),QRFPWORK                                         
         MVI   5(R2),8             SET LENGTH OF EXPLODED DATA                  
         MVI   11(R2),8            PASS LENGTH OF EXPLODED DATA                 
         OI    4(R2),X'20'         SET VALIDATED BIT                            
VALRFPX  XIT1                                                                   
         DROP  R3                                                               
         LTORG                                                                  
         EJECT                                                                  
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* RESFMFFD                                                                      
* DDGENTWA                                                                      
* RESFMFCD                                                                      
* REGENMKT                                                                      
* SRBLKD DSECT                                                                  
*      SPRANSIDD                                                                
* RESFMWORKD                                                                    
*        PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE REGENREG                                                       
         EJECT                                                                  
       ++INCLUDE REGENSAL                                                       
         EJECT                                                                  
       ++INCLUDE REGENSET                                                       
         EJECT                                                                  
       ++INCLUDE REGENTEM                                                       
         EJECT                                                                  
       ++INCLUDE REGENAGY                                                       
         EJECT                                                                  
       ++INCLUDE REGENCLS                                                       
         EJECT                                                                  
       ++INCLUDE REGENCTG                                                       
         EJECT                                                                  
       ++INCLUDE RESFMFFD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE RESFMEFD                                                       
         EJECT                                                                  
       ++INCLUDE RESFMWORKD                                                     
         SPACE 3                                                                
         ORG   SYSSPARE                                                         
       ++INCLUDE RESFM3AWRK                                                     
         EJECT                                                                  
       ++INCLUDE REPIOBLK                                                       
* RFP INCLUDES                                                                  
       ++INCLUDE GEGENRFPD                                                      
       ++INCLUDE REDDEQUS                                                       
STAD     DSECT                                                                  
       ++INCLUDE REGENSTA                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'067RESFM3A   11/14/07'                                      
         END                                                                    
