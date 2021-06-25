*          DATA SET ACCAP04    AT LEVEL 028 AS OF 12/11/09                      
*PHASE T61D04A                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        ACCAP04 -- SALARY HISTORY - MULTIPLE ADJUSTMENT      *         
*                                                                     *         
*  COMMENTS:     MAKES MULTIPLE ADJUSTMENTS TO SALARY HISTORY         *         
*                RECORDS                                              *         
*                                                                     *         
*  CALLED FROM:  CAP CONTROLLER (T61D00), WHICH CALLS                 *         
*                GEGENCON (T00A30), WHICH CALLS THIS.                 *         
*                                                                     *         
*  INPUTS:       SCREENS ACCAPF4 (LIST)                               *         
*                                                                     *         
*  OUTPUTS:      UPDATED SALARY HISTORY RECORD                        *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- SCREEN FIELD HEADER                            *         
*                R3 -- WORK                                           *         
*                R4 -- WORK                                           *         
*                R5 -- WORK                                           *         
*                R6 -- GETEL REGISTER                                 *         
*                R7 -- SECOND BASE                                    *         
*                R8 -- SPOOLD                                         *         
*                R9 -- SYSD                                           *         
*                RA -- TWA                                            *         
*                RB -- FIRST BASE                                     *         
*                RC -- GEND                                           *         
*                RD -- SYSTEM                                         *         
*                RE -- SYSTEM                                         *         
*                RF -- SYSTEM                                         *         
*                                                                     *         
***********************************************************************         
         TITLE 'T61D04 - SALARY HISTORY - MULTIPLE ADJUSTMENTS'                 
T61D04   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1D04**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING T61DFFD,RA          BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         LA    R5,SYSSPARE                                                      
         USING BLOCKSD,R5                                                       
         ST    R3,RELO                                                          
*                                                                               
         NI    GENSTAT2,X'FF'-USMYOK     RESET                                  
         LA    R3,MADPFKYH         PFKEY LINE                                   
         SR    R2,R2                                                            
         CLI   PFKEY,6             FOR UPDATE                                   
         BE    PFINIT                                                           
         LA    R2,PFTABLE                                                       
PFINIT   GOTO1 INITIAL,DMCB,(X'40',(R2)),(R3)                                   
*                                                                               
         GOTO1 GETLDG,DMCB,C'1R'      GET FIELD LENGTHS                         
         MVC   LEVELLN,ABCDLEN                                                  
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE LIST                                
         BE    VL                                                               
XYES     SR    RC,RC                                                            
XNO      LTR   RC,RC                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*        VALIDATE KEY                                                           
***********************************************************************         
*                                                                               
VK       OI    GLSTSTAT,APPLCDSP+NOSELFLD+CHNGLIST                              
         MVC   LLIST,=Y(LINNEXTL-LINDATEH)    LENGTH OF LIST LINE               
         MVI   BITS,0                                                           
         XC    PREVDATE,PREVDATE                                                
         XC    PREVLEN,PREVLEN                                                  
         XC    PREVPER,PREVPER                                                  
         XC    PREVPERL,PREVPERL                                                
         XC    PREVTYPE,PREVTYPE                                                
         XC    PREVADJ,PREVADJ                                                  
         XC    PREVDESC,PREVDESC                                                
         LA    R2,CONOPTH                                                       
         CLI   CONOPTH+5,0                                                      
         BNE   EINVOPT             THERE ARE NO VALID OPTIONS                   
         BAS   RE,FLDSEC           CHECK PERSON NAME FIELD SECURITY             
         BAS   RE,MKTYPETB         MAKE TABLE OF TYPE CODES AND NUMBERS         
         XC    BIGKEY,BIGKEY                                                    
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        VALIDATE LISTED RECORD                                                 
***********************************************************************         
*                                                                               
         USING LIND,R3                                                          
VL       LA    R3,MADFRSTH         ADDRESS OF LINE 1                            
VL00A    OI    LINDATEH+6,X'80'                                                 
         OI    LINCODEH+6,X'80'    MUST TRANSMIT EVERY FIELD ON LINE            
         OI    LINNAMEH+6,X'80'    BECAUSE THEY ARE SO CLOSE TOGETHER           
         OI    LINTYPEH+6,X'80'                                                 
         OI    LINADJH+6,X'80'                                                  
         OI    LINDESCH+6,X'80'                                                 
*                                                                               
         CLI   LINDATEH+5,0        ANYTHING ON SCREEN                           
         BNE   VL00                                                             
         CLI   LINCODEH+5,0        ANYTHING ON SCREEN                           
         BNE   VL00                                                             
         XC    LINNAME,LINNAME     ERASE NAME IF NO CODE                        
         CLI   LINTYPEH+5,0                                                     
         BNE   VL00                                                             
         CLI   LINADJH+5,0                                                      
         BNE   VL00                                                             
         XC    LINDESC,LINDESC     ERASE DESC IF NO CODE                        
*                                                                               
         LA    R3,LINNEXTL         NEXT LINE                                    
         LA    R1,MADLSTH                                                       
         CR    R3,R1                                                            
         BNH   VL00A                                                            
*                                                                               
         OI    GENSTAT2,USMYOK     I WILL SET MESSAGE                           
         OI    MADFRSTH+6,X'40'                                                 
         B     INFOEVAL                                                         
         B     XIT                                                              
*                                                                               
VL00     XC    REVELEM1,REVELEM1                                                
         MVI   SEQNUM,0            START WITH REC SEQ NUMBER 0                  
         NI    BITS,X'FF'-NEWREC                                                
*                                                                               
         OI    LINDATEH+6,X'80'                                                 
         OI    LINCODEH+6,X'80'    MUST TRANSMIT EVERY FIELD ON LINE            
         OI    LINNAMEH+6,X'80'    BECAUSE THEY ARE SO CLOSE TOGETHER           
         OI    LINTYPEH+6,X'80'                                                 
         OI    LINADJH+6,X'80'                                                  
         OI    LINDESCH+6,X'80'                                                 
*                                                                               
         CLI   LINDATEH+5,0        ANYTHING ON LINE                             
         BNE   VL05                                                             
         CLI   LINCODEH+5,0                                                     
         BNE   VL05                                                             
         XC    LINNAME,LINNAME     ERASE NAME IF NO CODE                        
         CLI   LINTYPEH+5,0                                                     
         BNE   VL05                                                             
         CLI   LINADJH+5,0                                                      
         BNE   VL05                                                             
         B     VL100                                                            
         EJECT                                                                  
***********************************************************************         
*        VALIDATE DATE                                                          
***********************************************************************         
*                                                                               
VL05     LA    R2,LINDATEH                                                      
         CLI   5(R2),0             ANY DATE?                                    
         BE    ERRPLS              MISSING                                      
*                                                                               
         CLI   LINDATEH+5,1                                                     
         BNE   VL06                                                             
         CLI   LINDATE,C'"'        SAME DATE AS PREVIOUS                        
         BNE   VL06                                                             
         MVC   LINDATE,PREVDATE                                                 
         MVC   LINDATEH+5(1),PREVLEN    PREVIOUS LENGTH                         
*                                                                               
VL06     MVC   PREVDATE,LINDATE                                                 
         MVC   PREVLEN,LINDATEH+5                                               
         XC    BLOCK(L'PVALOUTB),BLOCK                                          
         MVC   BYTE,LANGCODE                                                    
         OI    BYTE,X'60'                                                       
         GOTO1 PERVAL,DMCB,(LINDATEH+5,LINDATE),(BYTE,BLOCK)                    
         LA    R1,BLOCK                                                         
         USING PERVALD,R1                                                       
         CLI   DMCB+4,PVRCMISS                                                  
         BE    EINVDATE                                                         
         CLI   DMCB+4,PVRCINV1                                                  
         BE    EINVDATE                                                         
*                                                                               
         MVC   INDATE,PVALPSTA     REGULAR DATE FOR ELEMENT                     
         DROP  R1                                                               
         MVI   BYTE,10                                                          
         CLI   LANGCODE,3          GERMANY                                      
         BNE   *+8                                                              
         MVI   BYTE,17                                                          
         GOTO1 DATCON,DMCB,(1,INDATE),(BYTE,LINDATE)                            
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,INDATE           USE 'FFFF'-YYMM                            
         LNR   R0,R0                                                            
         STH   R0,RECDATE          MONTH AND YEAR FOR REC                       
*                                                                               
         XC    YYMMDD,YYMMDD                                                    
         MVC   YYMMDD(2),INDATE                                                 
         MVI   YYMMDD+2,X'01'                                                   
         GOTO1 DATCON,DMCB,(1,YYMMDD),(10,WORK)                                 
*                                                                               
*&&US*&& GOTO1 VBMONVAL,DMCB,(8,WORK),(97,ACOMFACS),(0,BLOCK),(CMPY,0)          
*&&UK                                                                           
         GOTO1 VBMONVAL,DMCB,(8,WORK),(97,ACOMFACS),(0,BLOCK),         X        
               (CMPY,TWAACCS)                                                   
*&&                                                                             
*                                                                               
         LA    R1,BLOCK                                                         
         USING BMONVALD,R1                                                      
         CLI   BMOERR,BMOEOKQ      EVERYTHING OK?                               
         BE    VL10                                                             
         MVC   GERROR,BMOMSG       SET ERROR MESSAGE                            
         B     ACCERRX                                                          
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
*        VALIDATE PERSON CODE                                                   
***********************************************************************         
*                                                                               
VL10     LA    R2,LINCODEH                                                      
         CLI   LINCODEH+5,0        ANY CODE INPUT                               
         BE    ERRMISS                                                          
*                                                                               
         CLI   LINCODEH+5,1                                                     
         BNE   VL11                                                             
         CLI   LINCODE,C'"'        SAME PERSON AS PREVIOUS                      
         BNE   VL11                                                             
         MVC   LINCODE,PREVPER                                                  
         MVC   LINCODEH+5(1),PREVPERL                                           
*                                                                               
VL11     CLC   5(1,R2),ABCDLEN+3                                                
         BH    ETOOLONG                                                         
         MVC   PERSON,SPACES                                                    
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         MVC   PERSON(0),LINCODE     SAVE PERSON CODE                           
         EX    R1,*-6                                                           
*                                                                               
         XC    BIGKEY,BIGKEY                                                    
         LA    R6,BIGKEY                                                        
         USING PERRECD,R6                                                       
         MVC   PERKEY,SPACES                                                    
         MVI   PERKTYP,PERKTYPQ                                                 
         MVC   PERKCPY,CMPY                                                     
         MVC   PERKCODE,PERSON                                                  
         GOTO1 HIGH                                                             
         CLC   BIGKEY(L'ACTKEY),KEYSAVE                                         
         BNE   EINVPER                                                          
         GOTO1 GETREC                                                           
*                                                                               
         USING LOCELD,R6                                                        
         L     R6,AIO              GET LOCATION                                 
         MVI   ELCODE,LOCELQ                                                    
         BAS   RE,GETEL                                                         
         B     VL14                                                             
VL12LP   BAS   RE,NEXTEL                                                        
         BNE   EPERDTE             INVALID PERSON FOR THIS DATE                 
VL14     CLC   INDATE,LOCSTART                                                  
         BL    VL12LP                                                           
         OC    LOCSALKD,LOCSALKD   SALARY LOCK DATE                             
         BZ    VL15A                                                            
         CLC   INDATE,LOCSALKD                                                  
         BNH   VL15B                                                            
         B     VL12LP                                                           
VL15A    OC    LOCEND,LOCEND                                                    
         BZ    VL15                                                             
         CLC   INDATE,LOCEND                                                    
         BNH   VL15                                                             
         B     VL12LP                                                           
*                                                                               
VL15B    ST    R6,SVADDR           IF SAL DATE OVERLAPS WITH NEXT LOC           
         BAS   RE,NEXTEL           USER MUST MAKE ENTRIES IN HIS SCREEN         
         BNE   VL15C                                                            
         CLC   INDATE,LOCSTART                                                  
         BNL   ERRINV              ***** NEED MESSAGE                           
VL15C    L     R6,SVADDR                                                        
*                                                                               
VL15     MVC   OFFICE,LOCOFF                                                    
         MVC   DEPT,LOCDEPT                                                     
         MVC   SUBDPT,LOCSUB                                                    
         MVC   PREVPER,PERSON      SAVE FOR DITTO                               
         MVC   PREVPERL,5(R2)                                                   
*                                                                               
         XC    WORK,WORK           DISPLAY PERSON'S OFFICE/NAME                 
         TM    BITS,NONAME         SECURITY SAYS DON'T SHOW NAME                
         BO    VL18                STILL SHOW LOCATION                          
*                                                                               
         USING GPNELD,R6                                                        
         L     R6,AIO                                                           
         LA    R4,WORK                                                          
         MVI   ELCODE,GPNELQ       GENERAL PURPOSE NAME ELEMENT                 
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                THIS ELEMENT IS REQUIRED                     
VL16     ZIC   R1,GPNLN                                                         
         SH    R1,=H'4'            3 FOR CODE,LEN,AND TYPE + 1 FOR EX           
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),GPNNME                                                   
         CLI   GPNTYP,GPNTLST                                                   
         BNE   VL18                                                             
         AR    R4,R1               BUMP LENGTH                                  
         LA    R4,1(R4)            ADD 1 MORE FOR EX                            
         MVI   0(R4),C','                                                       
         LA    R4,2(R4)            1 FOR , THEN 1 MORE                          
         BAS   RE,NEXTEL           GET FIRST NAME                               
         BNE   VL18                                                             
         B     VL16                                                             
*                                                                               
VL18     XC    SQSHBLK,SQSHBLK                                                  
         MVC   SQSHBLK(L'OFFICE),OFFICE                                         
         MVC   SQSHBLK+L'OFFICE+2(L'DEPT),DEPT                                  
         MVC   SQSHBLK+L'OFFICE+L'DEPT+4(L'SUBDPT),SUBDPT                       
         MVC   SQSHBLK+L'OFFICE+L'DEPT+L'SUBDPT+6(25),WORK                      
         GOTO1 SQUASHER,DMCB,SQSHBLK,L'SQSHBLK                                  
         MVC   LINNAME,SQSHBLK                                                  
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        CHECK ACCOUNT SECURITY                                                 
***********************************************************************         
*                                                                               
*                                                                               
         XC    BIGKEY,BIGKEY                                                    
         LA    R6,BIGKEY                                                        
         USING ACTRECD,R6                                                       
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CMPY        COMPANY                                      
         MVC   ACTKUNT(2),=C'1R'   1R                                           
         LA    R4,ACTKACT          BUILD ACCOUNT                                
         LA    RE,BILDACCT                                                      
         USING BILDD,RE                                                         
VL19     CLI   BILDLEV,X'FF'                                                    
         BE    VL19B                                                            
         SR    RF,RF                                                            
         ICM   RF,3,BILDLEV        LEVEL LENGTH OF R1                           
         LA    RF,STARTWRK(RF)                                                  
         SR    R1,R1                                                            
         IC    R1,0(RF)                                                         
         SR    R2,R2                                                            
         ICM   R2,3,BILDCODE       1R LEVEL CODE                                
         LA    R2,STARTWRK(R2)                                                  
         SH    R1,=H'1'                                                         
         BNM   *+6                                                              
         DC    H'0'                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(R2)                                                    
         LA    R4,1(R1,R4)         SET 1R FOR NEXT LEVEL                        
         LA    RE,BILDLEN(RE)                                                   
         B     VL19                                                             
         DROP  RE                                                               
VL19B    DS    0H                                                               
         LA    R2,LINCODEH         CURSOR ON PERSON CODE                        
         MVC   AIO,AIO2                                                         
         GOTO1 TSTSEC                                                           
         MVC   AIO,AIO1                                                         
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        VALIDATE TYPE CODE AND GET CORRESPONDING NUMBER                        
***********************************************************************         
*                                                                               
VL20     LA    R2,LINTYPEH                                                      
         CLI   LINTYPEH+5,0                                                     
         BE    ERRMISS                                                          
         CLI   LINTYPEH+5,1                                                     
         BNE   VL22                                                             
         CLI   LINTYPE,C'"'        SAME CODE AS PREVIOUS                        
         BNE   VL22                                                             
         MVC   LINTYPE,PREVTYPE                                                 
VL22     MVC   TYPECODE,LINTYPE                                                 
         OC    TYPECODE,SPACES                                                  
         MVC   PREVTYPE,TYPECODE                                                
         BAS   RE,VALTYPE                                                       
         BNE   EINVTYPE                                                         
         EJECT                                                                  
***********************************************************************         
*        VALIDATE AMOUNT AND SAVE VALUE IN ADJUST                               
***********************************************************************         
*                                                                               
VL30     LA    R2,LINADJH                                                       
         CLI   5(R2),0                                                          
         BE    ERRMISS                                                          
         CLI   LINADJH+5,1                                                      
         BNE   VL32                                                             
         CLI   LINADJ,C'"'        SAME AMOUNT AS PREVIOUS                       
         BNE   VL32                                                             
         OC    PREVADJ,PREVADJ                                                  
         BZ    EINVAMT                                                          
         ZAP   DMCB+4(8),PREVADJ                                                
         B     VL34                                                             
*                                                                               
VL32     LA    R1,LINADJ           CHECK FOR /HR                                
         SR    R6,R6                                                            
         ZIC   R0,LINADJH          LEN                                          
         SH    R0,=H'8'                                                         
         TM    LINADJH+1,X'02'     XTENED HEADER                                
         BNO   *+8                                                              
         SH    R0,=H'8'                                                         
*                                                                               
VL33     CLC   0(L'AC@PERHR,R1),AC@PERHR                                        
         BE    VL33A                                                            
         LA    R6,1(R6)            LENGTH TO VALIDATE                           
         LA    R1,1(R1)                                                         
         BCT   R0,VL33                                                          
*                                                                               
         ZIC   R6,5(R2)            VALIDATE ADJUSTMENT                          
VL33A    GOTO1 CASHVAL,DMCB,(X'82',LINADJ),(R6)                                 
         CLI   DMCB,X'FF'                                                       
         BNE   *+8                                                              
         B     EINVAMT                                                          
*                                                                               
VL34     TM    TYPESTAT,PAYSHRTE   HOURLY RATE PAYTYPE                          
         BNO   VL35                                                             
         CP    DMCB+4(8),=P'999.99'                                             
         BH    ERRINV              NUMBER TOO LARGE                             
         CP    DMCB+4(8),=P'-999.99'                                            
         BL    ERRINV              NUMBER TOO SMALL                             
         B     VL36                                                             
*                                                                               
VL35     CP    DMCB+4(8),=P'9999999.99'                                         
         BH    ERRINV              NUMBER TOO LARGE - WILL NOT DISPLAY          
         CP    DMCB+4(8),=P'-9999999.99'                                        
         BL    ERRINV              NUMBER TOO SMALL - WILL NOT DISPLAY          
*                                                                               
VL36     ZAP   ADJUST(6),DMCB+4(8)                                              
         ZAP   PREVADJ,ADJUST                                                   
*                                                                               
         TM    TYPESTAT,PAYSHRTE   HOURLY RATE                                  
         BNO   VL37                                                             
         MVC   BLOCK(25),SPACES                                                 
         CURED (P6,ADJUST),(11,BLOCK),2,FLOAT=-,ZERO=NOLANK,ALIGN=LEFT          
         LA    R1,BLOCK                                                         
         AR    R1,R0                                                            
         MVC   0(L'AC@PERHR,R1),AC@PERHR                                        
         LR    R1,R0                                                            
         LA    R1,3(R1)                                                         
         ZIC   R0,LINADJH          LEN                                          
         SH    R0,=H'8'                                                         
         TM    LINADJH+1,X'02'     XTENED HEADER                                
         BNO   *+8                                                              
         SH    R0,=H'8'                                                         
         SR    R0,R1                                                            
         LA    R6,LINADJ                                                        
         MVC   LINADJ,SPACES                                                    
         AR    R6,R0                                                            
         BCTR  R1,0                                                             
         MVC   0(0,R6),BLOCK                                                    
         EX    R1,*-6                                                           
         B     VL38                                                             
*                                                                               
VL37     CURED (P6,ADJUST),(11,LINADJ),2,FLOAT=-                                
*                                                                               
VL38     CLI   LINDESCH+5,1        COPY DOWN DESCRIPTION?                       
         BNE   VL39                                                             
         CLI   LINDESC,C'"'        SAME AMOUNT AS PREVIOUS                      
         LA    R2,LINDESCH                                                      
         BNE   EDESC1              CAN'T HAVE DESC LEN = 1                      
         MVC   LINDESC,PREVDESC                                                 
VL39     MVC   PREVDESC,LINDESC                                                 
         EJECT                                                                  
***********************************************************************         
*        READ PAYROLL HISTORY RECORD -- PF6 = UPDATE RECORD                     
***********************************************************************         
*                                                                               
         USING PHIRECD,R6                                                       
VL40     XC    BIGKEY,BIGKEY                                                    
         MVI   IOOPT,C'Y'          DOING MY OWN IO                              
         LA    R6,BIGKEY                                                        
         MVC   PHIKEY,SPACES                                                    
         MVI   PHIKTYP,PHIKTYPQ    PAYROLL CODE RECORD TYPE                     
         MVI   PHIKSUB,PHIKSUBQ    PAYROLL CODE SUB TYPE                        
         MVC   PHIKCPY,CMPY        COMPANY                                      
         MVC   PHIKOFC,OFFICE                                                   
         MVC   PHIKDPT,DEPT                                                     
         MVC   PHIKSBD,SUBDPT                                                   
         MVC   PHIKPER,PERSON                                                   
         MVC   PHIKMOA,RECDATE                                                  
         MVC   PHIKSEQ,SEQNUM      SEQUENCE NUMBER                              
         OI    DMINBTS,X'08'      READ FOR DELETES                              
         GOTO1 HIGH                                                             
         CLC   BIGKEY(L'ACTKEY),KEYSAVE                                         
         BE    VL44                GO GET RECORD                                
         CLI   PFKEY,6                                                          
         BNE   VL100                                                            
         BAS   RE,NEWSEQ           BUILD NEW KEY                                
         B     VL70                GO BUILD ELEMENT                             
*                                                                               
VL44     MVI   RDUPDATE,C'Y'       READ FOR UPDATE                              
         GOTO1 GETREC                                                           
         BAS   RE,DELOFF           TURN OFF DELETE BITS                         
         USING PDEELD,R6                                                        
         L     R6,AIO                                                           
         MVI   ELCODE,PDEELQ                                                    
         BAS   RE,GETEL                                                         
         BE    VL48                                                             
         B     VL60                CHECK FOR NEXT SEQ NUMBER                    
VL46     BAS   RE,NEXTEL                                                        
         BNE   VL60                                                             
VL48     CLC   DATENUM,PDEDTE      MATCH ON DATE AND CODE NUMBER                
         BNE   VL46                                                             
         MVI   SEQNUM,0                                                         
         LA    R2,LINDATEH                                                      
         B     EDUPEN                                                           
         DROP  R6                                                               
*                                                                               
VL60     ZIC   R1,SEQNUM           CHECK FOR NEXT SEQ NUMBER                    
         LA    R1,1(R1)                                                         
         STC   R1,SEQNUM                                                        
*                                                                               
         LA    R6,BIGKEY                                                        
         USING PHIRECD,R6                                                       
         MVC   PHIKSEQ,SEQNUM      SEQUENCE NUMBER                              
         OI    DMINBTS,X'08'                                                    
         GOTO1 HIGH                                                             
         CLC   BIGKEY(L'ACTKEY),KEYSAVE                                         
         BE    VL44                CHECK FOR DUPLICATE                          
         CLI   PFKEY,6                                                          
         BNE   VL100               NO DUPLICATE WILL ADD ELEM ON PF6            
*                                                                               
         ZIC   R1,SEQNUM           BACK UP AND TRY TO ADD ELEM                  
         SH    R1,=H'1'                                                         
         STC   R1,SEQNUM                                                        
         BAS   RE,NEXTSEQ                                                       
         B     VL70                ADD ELEM                                     
         EJECT                                                                  
***********************************************************************         
*        CREATE AND ADD ELEMENT                                                 
***********************************************************************         
*                                                                               
VL70     XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING PDEELD,R6                                                        
         MVI   PDEEL,PDEELQ                                                     
         MVC   PDEDTE,INDATE                                                    
         MVC   PDENUM,TYPENUM                                                   
         ZAP   PDEAMT(6),=P'0'                                                  
         ZAP   PDEADJ(6),ADJUST(6)                                              
         TM    TYPESTAT,PAYSHRTE   HOURLY RATE                                  
         BNO   *+8                                                              
         OI    PDESTAT2,PDESHRTE                                                
*                                                                               
         LA    R0,PDELNQ            DESCRIPTION AND LENGTH                      
         ZIC   R1,LINDESCH+5                                                    
         AR    R0,R1                                                            
         STC   R0,PDELN                                                         
         CLI   LINDESCH+5,0                                                     
         BE    VL74                                                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PDEDESC(0),LINDESC                                               
         OC    PDEDESC,SPACES                                                   
*                                                                               
VL74     BAS   RE,ADDS             ADDS ELEM IF CAN                             
         BE    VL90                EVERYTHING OK- WRITE OUT REC                 
*                                                                               
         GOTO1 WRITE               WRITE OUT PREVIOUS REC FIRST                 
         GOTO1 PUTREC                                                           
         ZIC   R1,SEQNUM                                                        
         LA    R1,1(R1)                                                         
         STC   R1,SEQNUM                                                        
         BAS   RE,NEXTSEQ          CHECK FOR NEXT SEQ REC                       
         BE    VL74                YES FOUND, ADD ELEM TO THAT REC              
         BAS   RE,NEWSEQ           MAKE NEW KEY IN AIO TO ADD                   
         B     VL74                ADD ELEM TO NEW REC                          
*                                                                               
VL90     TM    BITS,NEWREC         DOES RECORD ALREADY EXIST/                   
         BNZ   VL92                                                             
         GOTO1 WRITE                                                            
         L     R6,AIO              TRAP ERROR (WRITING BAD RECS)                
         OC    0(40,R6),0(R6)                                                   
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTO1 PUTREC                                                           
         B     VL95                                                             
VL92     GOTO1 ADDREC                                                           
*                                                                               
VL95     OC    REVELEM1,REVELEM1   ANY REVERSALS                                
         BZ    VL100                                                            
         BAS   RE,UPDREV           ADD REVERSAL ELEM                            
*                                                                               
VL100    CLI   PFKEY,6             CLEAR LINE ON PF6                            
         BNE   VL102                                                            
         XC    LINDATE,LINDATE                                                  
         OI    LINDATEH+6,X'80'                                                 
         XC    LINCODE,LINCODE                                                  
         OI    LINCODEH+6,X'80'                                                 
         XC    LINNAME,LINNAME                                                  
         OI    LINNAMEH+6,X'80'                                                 
         XC    LINTYPE,LINTYPE                                                  
         OI    LINTYPEH+6,X'80'                                                 
         XC    LINADJ,LINADJ                                                    
         OI    LINADJH+6,X'80'                                                  
         XC    LINDESC,LINDESC                                                  
         OI    LINDESCH+6,X'80'                                                 
*                                                                               
VL102    LA    R3,LINNEXTL         NEXT LINE                                    
         LA    R1,MADLSTH                                                       
         CR    R3,R1                                                            
         BNH   VL00                                                             
*                                                                               
VLX      OI    GENSTAT2,USMYOK     I WILL SET MESSAGE                           
*&&US*&& LA    R2,MADFRSTH         POSITION CURSOR AT PERIOD                    
         CLI   PFKEY,6                                                          
         BE    VLXX                                                             
         B     INFOPFUD                                                         
         B     XIT                                                              
*                                                                               
VLXX     B     INFOREUP                                                         
*&&US*&& LA    R2,MADFRSTH         POSITION CURSOR AT PERIOD                    
         B     XIT                                                              
         DROP  R6,R3                                                            
         EJECT                                                                  
***********************************************************************         
*        TURN OFF DELETE BITS                                                   
***********************************************************************         
*                                                                               
DELOFF   NTR1                                                                   
         USING PHIRECD,R6                                                       
         L     R6,AIO                                                           
         NI    PHIRSTA,X'FF'-X'80'                                              
         LA    R6,BIGKEY                                                        
         NI    PHIKSTA,X'FF'-X'80'                                              
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        CHECK FIELD SECURITY TO SEE WHETHER TO DISPLAY PERSON NAME             
***********************************************************************         
*                                                                               
FLDSEC   NTR1                                                                   
         XC    DMCB(24),DMCB                                                    
         MVI   SBYTE,NAMEFLDQ                                                   
         LA    R2,SBYTE                                                         
         GOTO1 SECRET,DMCB,('SECPFLDP',ASECBLK),(R2)                            
         CLI   DMCB,SECPYES                                                     
         BE    FLDSECX                                                          
         CLI   DMCB,SECPREAD                                                    
         BE    FLDSECX                                                          
         OI    BITS,NONAME         ACCESS DENIED DO NOT DISPLAY NAME            
FLDSECX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        MAKE TABLE OF TYPE CODES AND THEIR CORRESPONDING NUMBER                
*        TO BE USED FOR VALIDATION LATER                                        
***********************************************************************         
*                                                                               
MKTYPETB NTR1                                                                   
         LA    R0,CODEBLK          CLEAR BLOCK FOR TABLE                        
         L     R1,=A(CODEEND-CODEBLK)                                           
         LR    RE,R0                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         USING CODED,R3                                                         
         LA    R3,CODEBLK          MAKE TABLE OF CURRENT CODES                  
         MVI   SEQNUM,X'00'                                                     
MT10     LA    R6,BIGKEY                                                        
         XC    BIGKEY,BIGKEY                                                    
         USING PAYRECD,R6                                                       
         MVC   PAYKEY,SPACES                                                    
         MVI   PAYKTYP,PAYKTYPQ    PAYROLL CODE RECORD TYPE                     
         MVI   PAYKSUB,PAYKSUBQ    PAYROLL CODE SUB TYPE                        
         MVC   PAYKCPY,CMPY        COMPANY                                      
         MVC   PAYKSEQ,SEQNUM      SEQUENCE NUMBER                              
         GOTO1 HIGH                                                             
         CLC   BIGKEY(L'ACTKEY),KEYSAVE                                         
         BNE   MTX                                                              
         GOTO1 GETREC                                                           
         B     MT20                                                             
*                                                                               
MT15     GOTO1 SEQ                                                              
         CLC   BIGKEY(3),KEYSAVE   SAME COMPANY                                 
         BNE   MTX                                                              
MT20     GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,PAYELQ                                                    
         USING PAYELD,R6                                                        
         BAS   RE,GETEL                                                         
         BNE   MT15                                                             
         B     MT30                                                             
*                                                                               
MT20NX   BAS   RE,NEXTEL                                                        
         BNE   MT15                                                             
MT30     MVC   CODENUM,PAYNUM                                                   
         MVC   CODENME,PAYCODE                                                  
         MVC   CODEREV,PAYREV                                                   
         MVC   CODESTAT,PAYSTAT                                                 
         LA    R3,CODELEN(R3)                                                   
         LA    R1,CODEEND          END OF BLOCK                                 
         CR    R3,R1                                                            
         BL    MT20NX                                                           
         B     ERRINV                                                           
*                                                                               
MTX      B     XIT                                                              
         DROP  R6,R3                                                            
         EJECT                                                                  
***********************************************************************         
*        CHECK TABLE OF TYPE CODES FOR MATCH AND GET CORRESPONDING              
*        NUMBER TO USE IN ELEMENT                                               
*        INPUT:  TYPECODE = CODE TO VALIDATE                                    
*        OUTPUT: TYPENUM = CORRESPONDING NUMBER                                 
*                TYPESTAT = STATUS OF CODE (HOURLY RATE)                        
*                CC=EQ FOR VALID, CC=NEQ FOR NOT VALID                          
***********************************************************************         
*                                                                               
VALTYPE  NTR1                                                                   
         USING CODED,R4                                                         
         LA    R4,CODEBLK                                                       
VT10     CLC   CODENME,TYPECODE                                                 
         BNE   VT15                                                             
         MVC   TYPENUM,CODENUM    SAVE NUMBER                                   
         MVC   TYPESTAT,CODESTAT                                                
         CLI   PFKEY,6                                                          
         BNE   XYES                                                             
         OC    CODEREV,CODEREV     IS THERE A REVERSAL CODE                     
         BZ    XYES                                                             
         MVC   REVCODE,CODEREV                                                  
         BAS   RE,REVELEM          SET UP REVERSAL ELEMENT                      
         B     XYES                                                             
*                                                                               
VT15     LA    R4,CODELEN(R4)                                                   
         OC    0(CODELEN,R4),0(R4)   END OF TABLE                               
         BZ    XNO                 NO MATCH                                     
         LA    R1,CODEEND          END OF BLOCK                                 
         CR    R4,R1                                                            
         BL    VT10                                                             
         B     XNO                 NO MATCH                                     
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*        SETS UP REVERSAL ELEMENT THAT GETS ADDED TO NEXT                       
*        MONTH'S RECORD FOR THE NEGATIVE AMOUNT                                 
***********************************************************************         
*                                                                               
REVELEM  NTR1                                                                   
         USING LIND,R3            CURRENT LINE                                  
         USING PDEELD,R6                                                        
         LA    R6,REVELEM1                                                      
*                                                                               
RE10     MVI   PDEEL,PDEELQ                                                     
         ZAP   PDEAMT(6),=P'0'                                                  
*                                                                               
         GOTO1 DATCON,DMCB,(1,INDATE),(0,EBDATE)                                
         XC    WORK,WORK                                                        
         GOTO1 ADDAY,DMCB,(C'M',EBDATE),WORK,1                                  
         GOTO1 DATCON,DMCB,(0,WORK),(1,PDEDTE)                                  
         LA    R1,1                                                             
         STC   R1,PDEDTE+2         ALWAYS USE FIRST DAY OF MONTH                
         MVC   NXTDATE,PDEDTE      SAVE FOR KEY                                 
*                                                                               
         ZIC   R0,LINADJH+5        VALIDATE ADJUSTMENT                          
         GOTO1 CASHVAL,DMCB,(X'82',LINADJ),(R0)                                 
         CLI   DMCB,X'FF'                                                       
         BNE   *+12                                                             
         LA    R2,LINADJH                                                       
         B     EINVAMT                                                          
         ZAP   PDEADJ(6),DMCB+6(6)                                              
         OI    PDEADJ+5,X'0D'      MAKE NEGATIVE                                
*                                                                               
         LA    R4,CODEBLK          CHECK TABLE FOR CODE                         
         USING CODED,R4            AND GET CORRESPONDING NUM                    
         B     RE20                                                             
RE20NX   LA    R4,CODELEN(R4)                                                   
         OC    CODENUM(CODELEN),CODENUM                                         
         BNZ   RE20                                                             
         XC    REVELEM1,REVELEM1   NO MATCH ON REV CODE                         
         B     XIT                 SO DON'T DO REVERSAL                         
*                                                                               
RE20     CLC   CODENME,REVCODE     MATCH ON REVERSAL CODE                       
         BNE   RE20NX                                                           
         MVC   PDENUM,CODENUM      SAVE MATCHING NUMBER                         
         MVC   PDEDESC(8),AC@REVU                                               
         GOTO1 DATCON,DMCB,(1,INDATE),(17,WORK)                                 
         MVC   PDEDESC+9(5),WORK                                                
         MVC   PDEDESC+15(5),TYPECODE                                           
         LA    R1,20                                                            
         AH    R1,=Y(PDELNQ)                                                    
         STC   R1,PDELN                                                         
         B     XIT                                                              
         DROP  R4,R3,R6                                                         
         EJECT                                                                  
***********************************************************************         
*        ADDS OR UPDATES THE NEXT MONTH'S RECORD WITH                           
*        THE REVERSAL ELEMENT                                                   
***********************************************************************         
*                                                                               
UPDREV   NTR1                                                                   
         NI    BITS,X'FF'-NEWREC                                                
*                                                                               
         SR    R1,R1               FORMAT DATE                                  
         ICM   R1,3,NXTDATE                                                     
         LNR   R1,R1                                                            
         STCM  R1,3,NXTDATE                                                     
*                                                                               
         MVI   SEQNUM,0                                                         
         USING PHIRECD,R6                                                       
         XC    BIGKEY,BIGKEY                                                    
         LA    R6,BIGKEY                                                        
         MVC   PHIKEY,SPACES                                                    
         MVI   PHIKTYP,PHIKTYPQ    PAYROLL HOURS '3E05' RECS                    
         MVI   PHIKSUB,PHIKSUBQ                                                 
         MVC   PHIKCPY,CMPY        COMPANY                                      
         MVC   PHIKOFC,OFFICE                                                   
         MVC   PHIKDPT,DEPT                                                     
         MVC   PHIKSBD,SUBDPT                                                   
         MVC   PHIKPER,PERSON                                                   
         MVC   PHIKMOA,NXTDATE                                                  
         MVC   PHIKSEQ,SEQNUM                                                   
         OI    DMINBTS,X'08'       READ FOR DELETES                             
*                                                                               
         GOTO1 HIGH                                                             
         CLC   BIGKEY(L'ACTKEY),KEYSAVE   DOES THIS MONTH EXIST                 
         BNE   UR50                NO THEN ADD IT                               
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
         BAS   RE,DELOFF                                                        
         B     UR100               ADD ELEMS                                    
*                                                                               
UR50     BAS   RE,NEWSEQ           MAKE NEW KEY IN AIO TO ADD                   
UR100    LA    R6,AIO                                                           
         XC    ELEM,ELEM                                                        
         MVC   ELEM(50),REVELEM1                                                
*                                                                               
UR105    BAS   RE,ADDS             ADDS ELEM IF CAN                             
         BE    UR110               EVERYTHING OK                                
         GOTO1 WRITE               WRITE OUT PREVIOUS REC FIRST                 
         GOTO1 PUTREC                                                           
         ZIC   R1,SEQNUM                                                        
         LA    R1,1(R1)                                                         
         STC   R1,SEQNUM                                                        
         BAS   RE,NEXTSEQ          CHECK FOR NEXT SEQ REC                       
         BE    UR105               YES FOUND, ADD ELEM TO THAT REC              
         BAS   RE,NEWSEQ           MAKE NEW KEY IN AIO TO ADD                   
         B     UR105               ADD ELEM TO NEW REC                          
*                                                                               
UR110    TM    BITS,NEWREC                                                      
         BO    UR115                                                            
         B     UR117                                                            
UR115    GOTO1 ADDREC                                                           
         B     URX                                                              
UR117    GOTO1 WRITE                                                            
         GOTO1 PUTREC                                                           
URX      B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        ADDS ELEMENT IF POSSIBLE, IF REC TOO BIG                               
*        LOOKS FOR NEXT SEQ OR MAKES NEW REC                                    
***********************************************************************         
*                                                                               
ADDS     NTR1                                                                   
         USING PHIRECD,R6                                                       
         L     R6,AIO                                                           
         LH    R1,PHIRLEN                                                       
         ZIC   R0,ELEM+1                                                        
         AR    R1,R0                                                            
         CH    R1,=H'1990'                                                      
         BH    XNO                                                              
         GOTO1 ADDELEM                                                          
         B     XYES                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        CHECK FOR NEXT SEQ NUMBER                                              
***********************************************************************         
*                                                                               
NEXTSEQ  NTR1                                                                   
         USING PHIRECD,R6                                                       
         XC    BIGKEY,BIGKEY                                                    
         MVC   BIGKEY(L'ACTKEY),KEYSAVE                                         
         LA    R6,BIGKEY                                                        
         MVC   PHIKSEQ,SEQNUM                                                   
         OI    DMINBTS,X'08'       READ FOR DELETES                             
*                                                                               
         GOTO1 HIGH                                                             
         CLC   BIGKEY(L'ACTKEY),KEYSAVE                                         
         BNE   XNO                 NEED TO ADD NEW RECORD?                      
         CLI   PFKEY,6                                                          
         BNE   XYES                NO UPDATING UNLESS PFKEY=6                   
         MVI   RDUPDATE,C'Y'                                                    
         XC    BIGKEY,BIGKEY                                                    
         MVC   BIGKEY(L'ACTKEY),KEYSAVE                                         
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
         BAS   RE,DELOFF                                                        
         B     XYES                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        NEED TO ADD NEW RECORD WITH NEXT SEQ NUMBER                            
***********************************************************************         
*                                                                               
NEWSEQ   NTR1                                                                   
         L     RE,AIO              CLEAR I/O                                    
         LA    RF,LIOS                                                          
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         L     R6,AIO                                                           
         MVC   0(L'ACTKEY,R6),KEYSAVE                                           
         OI    BITS,NEWREC         FLAG TO ADD REC                              
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        RANDOM STUFF                                                           
***********************************************************************         
*                                                                               
ERRMISS  MVI   GERROR1,MISSING                                                  
         B     ERRX                                                             
ERRPLS   MVI   GERROR1,2                                                        
         MVI   GMSGTYPE,C'I'                                                    
         B     ERRX                                                             
ERRINV   MVI   GERROR1,INVALID                                                  
         B     ERRX                                                             
ERRDATE  MVI   GERROR1,X'06'                                                    
         B     ERRX                                                             
ERRX     MVI   GMSYS,X'FF'                                                      
         GOTO1 MYERR                                                            
*                                                                               
EPERDTE  MVC   GERROR,=AL2(ACEPERDT)     INVALID DATE FOR THIS PERSON           
         B     ACCERRX                                                          
EDESC1   MVC   GERROR,=AL2(ACEDESC1)                                            
         B     ACCERRX                                                          
ETOOSHRT MVC   GERROR,=AL2(ACELSHO)                                             
         B     ACCERRX                                                          
ETOOLONG MVC   GERROR,=AL2(ACELLONG)                                            
         B     ACCERRX                                                          
EINVOPT  MVC   GERROR,=AL2(ACEINVOP)                                            
         B     ACCERRX                                                          
EINVMET  MVC   GERROR,=AL2(ACEIVMET)                                            
         B     ACCERRX                                                          
EINVAMT  MVC   GERROR,=AL2(ACEAMNT)                                             
         B     ACCERRX                                                          
ENOMETH  MVC   GERROR,=AL2(ACENOMET)                                            
         B     ACCERRX                                                          
ECDXSTS  MVC   GERROR,=AL2(ACECDXST)                                            
         B     ACCERRX                                                          
ENOREV   MVC   GERROR,=AL2(ACENOREV)                                            
         B     ACCERRX                                                          
EINVTYPE MVC   GERROR,=AL2(ACEIVTYP)                                            
         B     ACCERRX                                                          
EINVDATE MVC   GERROR,=AL2(ACEIVDTE)                                            
         B     ACCERRX                                                          
EINVPER  MVC   GERROR,=AL2(ACEIVPER)                                            
         B     ACCERRX                                                          
EDUPEN   MVC   GERROR,=AL2(ACEDUPEN)                                            
         B     ACCERRX                                                          
INFOEVAL MVC   GERROR,=AL2(ACIEVAL)                                             
         MVI   GMSGTYPE,C'I'                                                    
         B     ACCERRX                                                          
INFOPFUD MVC   GERROR,=AL2(ACIPFUPD)                                            
         MVI   GMSGTYPE,C'I'                                                    
         B     ACCERRX                                                          
INFOREUP MVC   GERROR,=AL2(ACIRECUP)                                            
         MVI   GMSGTYPE,C'I'                                                    
         B     ACCERRX                                                          
*                                                                               
ACCERRX  DS    0H                                                               
         MVI   GMSYS,6             ACC SYSTEM MESSAGES                          
         GOTO1 MYERR                                                            
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                                  
***********************************************************************         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        PFKEY TABLE                                                  *         
***********************************************************************         
*                                                                               
PFTABLE  DS    0C                                                               
*                                                                               
*        PERSON LIST                                                            
*                                                                               
         DC    AL1(PPF01X-*,01,PFTCPROG,0,0)                                    
         DC    CL3' '                                                           
         DCDD  AC#CPRSN,8                                                       
         DCDD  AC#LIST,8                                                        
PPF01X   EQU   *                                                                
*                                                                               
*        PERSON DISPLAY                                                         
*                                                                               
         DC    AL1(PPF02X-*,02,PFTCPROG,0,0)                                    
         DC    CL3' '                                                           
         DCDD  AC#CPRSN,8                                                       
         DCDD  AC#DSP,8                                                         
PPF02X   EQU   *                                                                
*                                                                               
*        CALENDAR DISPLAY                                                       
*                                                                               
         DC    AL1(PPF05X-*,05,PFTCPROG,0,0)                                    
         DC    CL3' '                                                           
         DCDD  AC#CAL,8                                                         
         DCDD  AC#DSP,8                                                         
PPF05X   EQU   *                                                                
*                                                                               
*        PAYROLL LIST                                                           
*                                                                               
         DC    AL1(PPF04X-*,04,PFTCPROG,0,0)                                    
         DC    CL3' '                                                           
         DCDD  AC#PAYRL,8                                                       
         DCDD  AC#LIST,8                                                        
PPF04X   EQU   *                                                                
*                                                                               
*        RATES DISPLAY                                                          
*                                                                               
         DC    AL1(PPF10X-*,10,PFTCPROG,(PPF10X-PPF10)/KEYLNQ,0)                
         DC    CL3' '                                                           
         DCDD  AC#CRATS,8                                                       
         DCDD  AC#DSP,8                                                         
PPF10    DC    AL1(KEYTYCOM,3-1),AL2(0)                                         
PPF10X   EQU   *                                                                
*                                                                               
*        HISTORY LIST                                                           
*                                                                               
         DC    AL1(PPF03X-*,03,PFTCPROG,0,0)                                    
         DC    CL3' '                                                           
         DCDD  AC#HIST,8                                                        
         DCDD  AC#LIST,8                                                        
PPF03X   EQU   *                                                                
*                                                                               
*        RETURN TO CALLER                                                       
*                                                                               
         DC    AL1(PPF12X-*,12,PFTRPROG,0,0)                                    
         DC    CL3' ',CL8' ',CL8' '                                             
PPF12X   EQU   *                                                                
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
*        BUILD 1R ACCOUNT TABLE                                                 
***********************************************************************         
*                                                                               
BILDACCT DC    AL2(LEVELLNA-STARTWRK),AL2(OFFICE-STARTWRK)                      
         DC    AL2(LEVELLNB-STARTWRK),AL2(DEPT-STARTWRK)                        
         DC    AL2(LEVELLNC-STARTWRK),AL2(SUBDPT-STARTWRK)                      
         DC    AL2(LEVELLND-STARTWRK),AL2(PERSON-STARTWRK)                      
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
*        TABLES AND DSECTS                                                      
***********************************************************************         
*                                                                               
CODED    DSECT                                                                  
CODENUM  DS    CL1                 CODE NUMBER                                  
CODENME  DS    CL5                 CODE NAME                                    
CODEREV  DS    CL5                 REVERSAL CODE                                
CODESTAT DS    XL1                 STATUS                                       
CODELEN  EQU   *-CODED                                                          
         EJECT                                                                  
***********************************************************************         
*        INCLUDES                                                               
***********************************************************************         
*                                                                               
*        DDSPOOLD                                                               
*        DDSPLWORKD                                                             
*        DDPERVALD                                                              
*        FASECRETD                                                              
*        ACCAPWORKD                                                             
*        ACCAPDSECT                                                             
*        ACBMONVALD                                                             
*        ACGENFILE                                                              
*        ACDDEQUS                                                               
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE FASECRETD                                                      
       ++INCLUDE ACCAPWORKD                                                     
       ++INCLUDE ACCAPDSECT                                                     
       ++INCLUDE ACBMONVALD                                                     
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
         EJECT                                                                  
***********************************************************************         
*        SCREENS                                                                
***********************************************************************         
*                                                                               
       ++INCLUDE ACCAPFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE ACCAPF4D                                                       
         EJECT                                                                  
***********************************************************************         
*        REMAINING WORK AREA                                                    
***********************************************************************         
*                                                                               
         ORG   MADWORK                                                          
STARTWRK DS    0F                                                               
RELO     DS    A                                                                
SVADDR   DS    F                   SAVES ADDRESS                                
RECDATE  DS    H                                                                
NXTDATE  DS    H                                                                
OFFICE   DS    CL2                                                              
DEPT     DS    CL3                                                              
SUBDPT   DS    CL3                                                              
PERSON   DS    CL8                                                              
TYPECODE DS    CL5                                                              
REVCODE  DS    CL5                                                              
LEVELLN  DS    0CL4                LENGTHS OF ALL LEVELS                        
LEVELLNA DS    CL1                 LENGTH OF A                                  
LEVELLNB DS    CL1                 LENGTH OF B                                  
LEVELLNC DS    CL1                 LENGTH OF C                                  
LEVELLND DS    CL1                 LENGTH OF D                                  
DATENUM  DS    0CL4                DON'T MOVE                                   
INDATE   DS    XL3                 DON'T MOVE                                   
TYPENUM  DS    CL1                 DON'T MOVE                                   
TYPESTAT DS    CL1                 PAYCODE TYPE STATUS                          
SEQNUM   DS    XL1                 RECORD SEQUENCE NUMBER                       
SBYTE    DS    XL1                 FIELD NUMBER                                 
PREVDATE DS    CL8                 PREVIOUS DATE                                
PREVLEN  DS    XL1                 PREVIOUS DATE LENGTH                         
PREVPER  DS    CL8                 PREVIOUS PERSON CODE                         
PREVPERL DS    CL1                 PREVIOUS PERSON CODE LENGTH                  
PREVTYPE DS    CL5                 PREVIOUS TYPE                                
PREVADJ  DS    PL6                 PREVIOUS ADJUSTMENT                          
PREVDESC DS    CL21                PREVIOUS DESCRIPTION                         
ADJUST   DS    PL6                                                              
YYMMDD   DS    PL3                                                              
EBDATE   DS    CL6                 EBCDIC YYMMDD                                
*                                                                               
BITS     DS    X                                                                
NEWREC   EQU   X'80'               DO AN ADD                                    
NONAME   EQU   X'40'               DON'T SHOW JPERSON'S NAME                    
*                                                                               
NAMEFLDQ EQU   1                   NAME FIELD NUMBER FOR SECURITY               
*                                                                               
REVELEM1 DS    CL50                SAVED REVERSAL ELEMENT                       
SAVEKEY  DS    XL42                ACCFILE KEY                                  
PVALBLK  DS    XL75                PERVAL  BLOCK                                
SQSHBLK  DS    CL100                                                            
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
***********************************************************************         
*        DSECTS                                                                 
***********************************************************************         
BLOCKSD  DSECT                     PUT IN DISPBLK                               
CODEBLK  DS    300CL(CODELEN)                                                   
CODEEND  DS    CL1                                                              
*                                                                               
*                                                                               
BILDD    DSECT                                                                  
BILDLEV  DS    AL2                 DISPLACEMENT TO LEVEL LENGTH                 
BILDCODE DS    AL2                 DISPLACEMENT TO LEVEL CODE                   
BILDLEN  EQU   *-BILDD                                                          
*                                                                               
*                                                                               
LIND     DSECT                     LINE DSECT                                   
LINDATEH DS    CL8                                                              
LINDATE  DS    CL8                 DATE                                         
LINCODEH DS    CL8                                                              
LINCODE  DS    CL8                 PERSON CODE                                  
LINNAMEH DS    CL8                                                              
LINNAME  DS    CL21                PERSON NAME                                  
LINTYPEH DS    CL8                                                              
LINTYPE  DS    CL5                 PAYROLL TYPE CODE                            
LINADJH  DS    CL8                                                              
LINADJ   DS    CL11                AMOUNT OF ADJUSTMENT                         
LINDESCH DS    CL8                                                              
LINDESC  DS    CL21                DESCRIPTION                                  
LINNEXTL DS    0C                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'028ACCAP04   12/11/09'                                      
         END                                                                    
